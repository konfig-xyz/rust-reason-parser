module Lib (someFunc) where

import Text.Parsec hiding (count)

someFunc :: IO ()
someFunc = do
  contents <- readFile "schema.rs"
  print $ parseSchema contents

type TypePair = (String, String)

type Table = (String, [TypePair])

type Schema = [Table]

typeTuple :: Parsec String () TypePair
typeTuple = do
  spaces
  typeName <- manyTill anyChar $ spaces *> string "->" <* spaces
  typeVar <- manyTill anyChar $ string ","
  optional eof
  return (typeName, typeVar)

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
