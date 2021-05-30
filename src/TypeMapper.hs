module TypeMapper where

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
  Right ("Nullable", x) -> "Js.Nullable.t(" <> mapType x <> ")"
  Left x -> show x

mapTypePair :: TypePair -> String
mapTypePair (typeName, typeValue) = typeName <> ": " <> mapType typeValue
