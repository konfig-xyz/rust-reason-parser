{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConfigurationParser (makeConfig) where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Yaml.Config (load, lookupDefault, subconfig)
import Types

parseTypeMapConfiguration :: String -> Maybe (String, String)
parseTypeMapConfiguration xs = case splitOn "->" xs of
  [x, y] -> Just (x, y) --TODO - trim whitespace
  _ -> Nothing

toTypeMap :: [String] -> Mapping
toTypeMap = M.fromList . mapMaybe parseTypeMapConfiguration

makeConfig :: String -> IO Configuration
makeConfig path = do
  config <- load path
  types <- subconfig "types" config
  hiding <- subconfig "hiding" config

  pure $
    Configuration
      (toTypeMap $ lookupDefault "aliases" [] types)
      (toTypeMap $ lookupDefault "base" [] types)
      (toTypeMap $ lookupDefault "nested" [] types)
      (S.fromList $ lookupDefault "tables" [] hiding)
      (S.fromList $ lookupDefault "keys" [] hiding)
