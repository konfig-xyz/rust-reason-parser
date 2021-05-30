{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (fn) where

import qualified Data.List as L
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Yaml.Config as Y
import System.Environment (getArgs)
import Text.Parsec
import TypeMapper
import Types

fn :: IO ()
fn = do
  [configFile, fileName] <- getArgs
  config <- Y.load configFile
  types <- Y.subconfig "types" config
  let aliases :: Mapping = M.fromList $ mapMaybe splitConversion $ Y.lookupDefault "aliases" [] types
  let bMap :: Mapping = M.fromList $ mapMaybe splitConversion $ Y.lookupDefault "base" [] types
  let nMap :: Mapping = M.fromList $ mapMaybe splitConversion $ Y.lookupDefault "nested" [] types

  hiding <- Y.subconfig "hiding" config
  let tSet :: Hidden = S.fromList $ Y.lookupDefault "tables" [] hiding
  let kSet :: Hidden = S.fromList $ Y.lookupDefault "keys" [] hiding

  contents <- readFile fileName
  putStrLn $ typeAliases aliases <> parseSchema bMap nMap tSet kSet contents

typeAlias :: (String, String) -> String
typeAlias (x, y) = "type: " <> x <> ": " <> y <> ";"

typeAliases :: Mapping -> String
typeAliases xs = L.intercalate "\n" (map typeAlias $ M.toList xs) <> "\n\n"

splitConversion :: String -> Maybe (String, String)
splitConversion xs = case splitOn "->" xs of
  [x, y] -> Just (x, y)
  _ -> Nothing

typeTuple :: Mapping -> Mapping -> Hidden -> Parsec String () String
typeTuple bMap nMap kSet = do
  spaces
  typeName <- manyTill anyChar $ spaces *> string "->" <* spaces
  typeVar <- manyTill anyChar $ string ","
  optional eof
  if S.member typeName kSet
    then do
      pure $ "// " <> mapTypePair bMap nMap (typeName, typeVar)
    else do
      pure $ mapTypePair bMap nMap (typeName, typeVar)

table :: Mapping -> Mapping -> Hidden -> Hidden -> Parsec String () String
table bMap nMap tSet kSet = do
  string "table! {" <* try spaces
  string "use diesel::sql_types::*;" <* try spaces
  typeName <- manyTill anyChar $ try space
  spaces
  try $ manyTill anyChar $ try $ string "{"
  contents <- manyTill (try (typeTuple bMap nMap kSet)) $ try $ spaces *> string "}"
  spaces
  try $ string "}"
  if S.member typeName tSet
    then do
      pure $ "// " <> typeName
    else do
      pure $ mapTable (typeName, contents)

schema :: Mapping -> Mapping -> Hidden -> Hidden -> Parsec String () [String]
schema bMap nMap tSet kSet =
  manyTill (try (table bMap nMap tSet kSet) <* spaces) $
    try
      (string "joinable" <|> (eof >> pure ""))

parseSchema :: Mapping -> Mapping -> Hidden -> Hidden -> String -> String
parseSchema bMap nMap tSet kSet xs = case runParser (schema bMap nMap tSet kSet) () "Err" xs of
  Right x -> unlines x
  Left y -> "Could not parse schema: " <> show y
