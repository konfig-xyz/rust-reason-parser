{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.List as L
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Yaml.Config as Y
import Lib
import System.Environment (getArgs)
import Text.Parsec
import TypeMapper
import Types

main :: IO ()
main = do
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
