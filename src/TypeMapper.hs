module TypeMapper where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Text.Casing
import Text.Parsec
import Types

parseContainer :: Parsec String () (String, String)
parseContainer = do
  containerType <- manyTill anyChar $ try $ string "<"
  valueType <- manyTill anyChar $ try $ string ">" <* (eof >> pure "")
  pure (containerType, valueType)

mapContainer :: Mapping -> Mapping -> Either ParseError (String, String) -> String
mapContainer bMap nMap (Left y) = "Parse error: " <> show y
mapContainer bMap nMap (Right (x, value))
  | isJust key = fromJust key <> nesting
  | otherwise = x <> nesting
  where
    key = M.lookup x nMap
    nesting = "(" <> mapType bMap nMap value <> ")"

mapType :: Mapping -> Mapping -> String -> String
mapType bMap nMap xs
  | isJust key = fromJust key
  | otherwise = mapContainer bMap nMap (runParser parseContainer () "Err" xs)
  where
    key = M.lookup xs bMap

mapTypePair :: Mapping -> Mapping -> TypePair -> String
mapTypePair bMap nMap (typeName, typeValue) = toCamel (fromSnake typeName) <> ": " <> mapType bMap nMap typeValue

mapTableName :: String -> String
mapTableName tableName = "module " <> toPascal (fromSnake tableName) <> " { };\n"

mapTable :: Table -> String
mapTable (tableName, types) = "module " <> toPascal (fromSnake tableName) <> " {\n\ttype t = {\n\t\t" <> L.intercalate ",\n\t\t" types <> "\n\t};\n};\n"
