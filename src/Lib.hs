{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (someFunc) where

import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Yaml.Config as Y
import System.Environment (getArgs)
import Text.Parsec
import TypeMapper
import Types

someFunc :: IO ()
someFunc = do
  [configFile, fileName] <- getArgs
  config <- Y.load configFile
  types <- Y.subconfig "types" config
  let aliases :: Mapping = M.fromList $ mapMaybe splitConversion $ Y.lookupDefault "aliases" [] types
  let base :: Mapping = M.fromList $ mapMaybe splitConversion $ Y.lookupDefault "base" [] types
  let nested :: Mapping = M.fromList $ mapMaybe splitConversion $ Y.lookupDefault "nested" [] types

  --base <- subconfig "base" types
  --nested <- subconfig "nested" types
  --hiding <- subconfig "hiding" config

  contents <- readFile fileName
  putStrLn $ typeAliases aliases <> parseSchema base nested contents

typeAlias :: (String, String) -> String
typeAlias (x, y) = "type: " <> x <> ": " <> y <> ";"

typeAliases :: Mapping -> String
typeAliases xs = L.intercalate "\n" (map typeAlias $ M.toList xs) <> "\n\n"

splitConversion :: String -> Maybe (String, String)
splitConversion xs = case S.splitOn "->" xs of
  [x, y] -> Just (x, y)
  _ -> Nothing

typeTuple :: Mapping -> Mapping -> Parsec String () String
typeTuple bMap nMap = do
  spaces
  typeName <- manyTill anyChar $ spaces *> string "->" <* spaces
  typeVar <- manyTill anyChar $ string ","
  optional eof
  pure $ mapTypePair bMap nMap (typeName, typeVar)

table :: Mapping -> Mapping -> Parsec String () String
table bMap nMap = do
  string "table! {" <* try spaces
  string "use diesel::sql_types::*;" <* try spaces
  typeName <- manyTill anyChar $ try space
  spaces
  try $ manyTill anyChar $ try $ string "{"
  contents <- manyTill (try (typeTuple bMap nMap)) $ try $ spaces *> string "}"
  spaces
  try $ string "}"
  pure $ mapTable (typeName, contents)

schema :: Mapping -> Mapping -> Parsec String () [String]
schema bMap nMap = manyTill (try (table bMap nMap) <* spaces) $ try (string "joinable" <|> (eof >> pure ""))

--parseSchema :: String -> String
parseSchema bMap nMap xs = case runParser (schema bMap nMap) () "Err" xs of
  Right x -> unlines x
  Left y -> "Could not parse schema: " <> show y
