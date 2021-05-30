module Lib (someFunc) where

import Text.Parsec
import TypeMapper
import Types

someFunc :: IO ()
someFunc = do
  contents <- readFile "schema.rs"
  print $ parseSchema contents

typeTuple :: Parsec String () String
typeTuple = do
  spaces
  typeName <- manyTill anyChar $ spaces *> string "->" <* spaces
  typeVar <- manyTill anyChar $ string ","
  optional eof
  pure $ mapTypePair (typeName, typeVar)

table :: Parsec String () Table
table = do
  string "table! {" <* try spaces
  string "use diesel::sql_types::*;" <* try spaces
  typeName <- manyTill anyChar $ try space
  spaces
  try $ manyTill anyChar $ try $ string "{"
  contents <- manyTill (try typeTuple) $ try $ spaces *> string "}"
  spaces
  try $ string "}"
  return (typeName, contents)

schema :: Parsec String () Schema
schema = manyTill (try table <* spaces) $ try $ string "joinable"

--parseSchema :: String -> String
parseSchema = runParser schema () "Err"
