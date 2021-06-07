module SchemaPrinter (printTypeAliases, printSchema) where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S
import SchemaParser
import Text.Casing
import Text.Parsec
import Types

mergeQualified :: Configuration -> String -> Hidden
mergeQualified configuration typeName = case M.lookup typeName (qualified configuration) of
  Just xs -> S.union xs (keys configuration)
  Nothing -> keys configuration

printTypeAlias :: (String, String) -> String
printTypeAlias (x, y) = "type " <> x <> " = " <> y <> ";"

printTypeAliases :: Mapping -> String
printTypeAliases xs = L.intercalate "\n" (map printTypeAlias $ M.toList xs) <> "\n\n"

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
  | otherwise = printTypeContainer configuration (runParser parseTypeContainer () "Err" xs)
  where
    key = M.lookup xs $ base configuration

printTypeName :: String -> String
printTypeName typeName = toCamel (fromSnake typeName)

printType :: Configuration -> String -> TypePair -> String
printType configuration tableName (typeName, typeValue)
  | S.member typeName mergedQualifiedKeys = "// " <> typeString
  | otherwise = typeString
  where
    mergedQualifiedKeys = mergeQualified configuration tableName
    typeString = printTypeName typeName <> ": " <> printTypeValue configuration typeValue

printModuleName :: String -> String
printModuleName xs = "module " <> toPascal (fromSnake xs)

printTableName :: String -> Visibility String -> String
printTableName tableName (Visible types) = printModuleName tableName <> " {\n" <> types <> "\n};\n"
printTableName tableName Hidden = "// " <> printModuleName tableName <> " { };\n\n"

printTableTypes :: Configuration -> String -> [TypePair] -> String
printTableTypes configuration tableName xs = "\ttype t = {\n\t\t" <> L.intercalate ",\n\t\t" (map (printType configuration tableName) xs) <> "\n\t};\n"

printTable :: Configuration -> Table -> String
printTable configuration (tableName, types)
  | S.member tableName (tables configuration) = printTableName tableName Hidden
  | otherwise = printTableName tableName (Visible $ printTableTypes configuration tableName types)

printSchema :: Configuration -> Schema -> String
printSchema configuration = concatMap (printTable configuration)
