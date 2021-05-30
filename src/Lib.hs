module Lib (someFunc) where

import Text.Parsec
import TypeMapper
import Types
import System.Environment ( getArgs )

someFunc :: IO ()
someFunc = do
  fileName <- getArgs
  contents <- readFile $ concat fileName
  putStrLn $ parseSchema contents

typeTuple :: Parsec String () String
typeTuple = do
  spaces
  typeName <- manyTill anyChar $ spaces *> string "->" <* spaces
  typeVar <- manyTill anyChar $ string ","
  optional eof
  pure $ mapTypePair (typeName, typeVar)

table :: Parsec String () String
table = do
  string "table! {" <* try spaces
  string "use diesel::sql_types::*;" <* try spaces
  typeName <- manyTill anyChar $ try space
  spaces
  try $ manyTill anyChar $ try $ string "{"
  contents <- manyTill (try typeTuple) $ try $ spaces *> string "}"
  spaces
  try $ string "}"
  pure $ mapTable (typeName, contents)

schema :: Parsec String () [String]
schema = manyTill (try table <* spaces) $ try (string "joinable" <|> (eof >> pure ""))

--parseSchema :: String -> String
parseSchema xs = case runParser schema () "Err" xs of
  Right x -> unlines x
  Left y -> "Could not parse schema: " <> show y
