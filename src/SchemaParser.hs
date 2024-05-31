{-# LANGUAGE OverloadedStrings #-}

module SchemaParser (parseTypeContainer, parseSchema) where

import Control.Monad
import Data.Set (member)
import qualified Data.Text as T
import Debug.Trace
import Text.Parsec
import Type.Reflection.Unsafe
import Types

data TypeName = Simple T.Text | Qualified (T.Text, T.Text)
  deriving (Eq, Ord)

parseQualifiedType :: Parsec T.Text () (T.Text, T.Text)
parseQualifiedType = do
  spaces
  base <- manyTill anyChar $ string "."
  nesting <- manyTill anyChar eof
  pure (T.pack base, T.pack nesting)

parseTypeContainer :: Parsec T.Text () (T.Text, T.Text)
parseTypeContainer = do
  containerType <- manyTill anyChar $ try $ string "<"
  valueType <- manyTill anyChar $ try $ string ">" <* (eof >> pure "")
  pure (T.pack containerType, T.pack valueType)

parseType :: Parsec T.Text () (T.Text, T.Text)
parseType = do
  spaces
  typeName <- manyTill anyChar $ spaces *> string "->" <* spaces
  typeVar <- manyTill anyChar $ string ","
  optional eof
  pure (T.pack typeName, T.pack typeVar)

parseTable :: Parsec T.Text () (T.Text, [(T.Text, T.Text)])
parseTable = do
  optional $ string "diesel::" -- Diesel v2
  string "table! {"
  optional (spaces *> string "use" <* manyTill anyChar (try $ newline <* newline))

  spaces
  typeName <- manyTill anyChar $ try space
  spaces

  try $ manyTill anyChar (try $ string "{")
  contents <- manyTill (try parseType) (try $ spaces *> string "}")
  spaces

  string "}"
  pure (T.pack typeName, contents)

sqlTypes = do
  string "pub mod sql_types {" <* try spaces
  manyTill anyChar (try (string "}"))

parseSchema :: T.Text -> [(T.Text, [(T.Text, T.Text)])]
parseSchema xs = case runParser schemaParser () "Error Parsing" xs of
  Right x -> x
  Left y -> []
  where
    schemaParser = do
      optional $ string "// @generated automatically by Diesel CLI." <* spaces
      optional sqlTypes <* spaces
      manyTill (try parseTable <* spaces) $ try (optional $ string "diesel::" <* string "joinable" <|> (eof >> pure ""))
