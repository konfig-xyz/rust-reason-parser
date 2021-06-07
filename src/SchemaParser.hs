module SchemaParser (parseTypeContainer, parseSchema) where

import Data.Set (member)
import Text.Parsec
import Type.Reflection.Unsafe
import Types

parseTypeContainer :: Parsec String () (String, String)
parseTypeContainer = do
  containerType <- manyTill anyChar $ try $ string "<"
  valueType <- manyTill anyChar $ try $ string ">" <* (eof >> pure "")
  pure (containerType, valueType)

parseType :: Parsec String () (String, String)
parseType = do
  spaces
  typeName <- manyTill anyChar $ spaces *> string "->" <* spaces
  typeVar <- manyTill anyChar $ string ","
  optional eof
  pure (typeName, typeVar)

parseTable :: Parsec String () (String, [(String, String)])
parseTable = do
  string "table! {" <* try spaces
  string "use diesel::sql_types::*;" <* try spaces
  typeName <- manyTill anyChar $ try space
  spaces
  try $ manyTill anyChar $ try $ string "{"
  contents <- manyTill (try parseType) $ try $ spaces *> string "}"
  spaces
  try $ string "}"
  pure (typeName, contents)

parseSchema :: String -> [(String, [(String, String)])]
parseSchema xs = case runParser schemaParser () "Error Parsing" xs of
  Right x -> x
  Left y -> []
  where
    schemaParser = manyTill (try parseTable <* spaces) $ try (string "joinable" <|> (eof >> pure ""))
