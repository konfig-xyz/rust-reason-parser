module SchemaParser (parseSchema) where

import Data.Set (member)
import Text.Parsec
import SchemaPrinter
import Type.Reflection.Unsafe
import Types

parseTypeContainer :: Parsec String () (String, String)
parseTypeContainer = do
  containerType <- manyTill anyChar $ try $ string "<"
  valueType <- manyTill anyChar $ try $ string ">" <* (eof >> pure "")
  pure (containerType, valueType)

parseType :: Configuration -> Parsec String () String
parseType configuration = do
  spaces
  typeName <- manyTill anyChar $ spaces *> string "->" <* spaces
  typeVar <- manyTill anyChar $ string ","
  optional eof
  if member typeName $ keys configuration
    then do
      pure $ "// " <> printType configuration (typeName, typeVar)
    else do
      pure $ printType configuration (typeName, typeVar)

parseTable :: Configuration -> Parsec String () String
parseTable configuration = do
  string "table! {" <* try spaces
  string "use diesel::sql_types::*;" <* try spaces
  typeName <- manyTill anyChar $ try space
  spaces
  try $ manyTill anyChar $ try $ string "{"
  contents <- manyTill (try (parseType configuration)) $ try $ spaces *> string "}"
  spaces
  try $ string "}"
  if member typeName $ tables configuration
    then do
      pure $ "// " <> printTableName typeName Nothing
    else do
      pure $ printTable (typeName, contents)

parseSchema :: Configuration -> String -> String
parseSchema configuration xs = case runParser schemaParser () "Error Parsing" xs of
  Right x -> unlines x
  Left y -> "Could not parse schema: " <> show y
  where
    schemaParser = manyTill (try (parseTable configuration) <* spaces) $ try (string "joinable" <|> (eof >> pure ""))
