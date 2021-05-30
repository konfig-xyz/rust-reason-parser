module TypeMapper where

import qualified Data.List as L
import Text.Casing
import Text.Parsec
import Types

parseContainer :: Parsec String () (String, String)
parseContainer = do
  containerType <- manyTill anyChar $ try $ string "<"
  valueType <- manyTill anyChar $ try $ string ">" <* (eof >> pure "")
  pure (containerType, valueType)

mapType :: String -> String
mapType "Uuid" = "string" -- TODO -> Global type aliases
mapType "Text" = "string"
mapType "Bool" = "bool"
mapType "Int4" = "int"
mapType "Float4" = "float"
mapType x = case runParser parseContainer () "Err" x of
  Right ("Array", x) -> "array(" <> mapType x <> ")"
  Right ("Nullable", x) -> "option(" <> mapType x <> ")"
  Left y -> "Tried parsing: " <> x <> "; Errored with: " <> show y

mapTypePair :: TypePair -> String
mapTypePair (typeName, typeValue) = toCamel (fromSnake typeName) <> ": " <> mapType typeValue

mapTable :: Table -> String
mapTable (tableName, types) = "module " <> toPascal (fromSnake tableName) <> " {\n\ttype t = {\n\t\t" <> L.intercalate ",\n\t\t" types <> "\n\t};\n};\n"
