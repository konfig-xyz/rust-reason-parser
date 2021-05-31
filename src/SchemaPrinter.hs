module SchemaPrinter where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Text.Casing
import Text.Parsec
import Types

printTypeAlias :: (String, String) -> String
printTypeAlias (x, y) = "type " <> x <> " = " <> y <> ";"

printTypeAliases :: Mapping -> String
printTypeAliases xs = L.intercalate "\n" (map printTypeAlias $ M.toList xs) <> "\n\n"

-- TODO - Pull out to parser
parseContainerType :: Parsec String () (String, String)
parseContainerType = do
  containerType <- manyTill anyChar $ try $ string "<"
  valueType <- manyTill anyChar $ try $ string ">" <* (eof >> pure "")
  pure (containerType, valueType)

printTypeContainer :: Configuration -> Either ParseError (String, String) -> String
printTypeContainer configuration (Left y) = "Parse error: " <> show y
printTypeContainer configuration (Right (x, value))
  | isJust key = fromJust key <> nesting
  | otherwise = x <> nesting
  where
    key = M.lookup x $ nested configuration
    nesting = "(" <> printTypeValue configuration value <> ")"

printTypeValue :: Configuration -> String -> String
printTypeValue configuration xs
  | isJust key = fromJust key
  | otherwise = printTypeContainer configuration (runParser parseContainerType () "Err" xs)
  where
    key = M.lookup xs $ base configuration

printTypeName :: String -> String
printTypeName typeName = toCamel (fromSnake typeName)

printType :: Configuration -> TypePair -> String
printType configuration (typeName, typeValue) = printTypeName typeName <> ": " <> printTypeValue configuration typeValue

printTableName :: String -> String
printTableName tableName = "module " <> toPascal (fromSnake tableName) <> " { };\n"

printTableTypes :: [String] -> String
printTableTypes xs = " {\n\ttype t = {\n\t\t" <> L.intercalate ",\n\t\t" xs <> "\n\t};\n};\n"

printTable :: Table -> String
printTable (tableName, types) = printTableName tableName <> printTableTypes types
