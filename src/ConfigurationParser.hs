{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConfigurationParser (makeConfig) where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Yaml.Config (Config, keys, load, lookup, lookupDefault, subconfig)
import qualified Types as T

parseTypeMapConfiguration :: String -> Maybe (String, String)
parseTypeMapConfiguration xs = case splitOn "->" xs of
  [x, y] -> Just (x, y) --TODO - trim whitespace
  _ -> Nothing

toTypeMap :: [String] -> T.Mapping
toTypeMap = M.fromList . mapMaybe parseTypeMapConfiguration

toQualified :: Config -> T.HiddenQualified
toQualified xs = M.fromList $ map (\k -> (T.unpack k, S.fromList $ lookupDefault k [] xs)) (keys xs)

makeConfig :: String -> IO T.Configuration
makeConfig path = do
  config <- load path
  types <- subconfig "types" config
  hiding <- subconfig "hiding" config
  qualified <- subconfig "qualified" hiding

  pure $
    T.Configuration
      (toTypeMap $ lookupDefault "aliases" [] types)
      (toTypeMap $ lookupDefault "base" [] types)
      (toTypeMap $ lookupDefault "nested" [] types)
      (S.fromList $ lookupDefault "tables" [] hiding)
      (S.fromList $ lookupDefault "keys" [] hiding)
      (toQualified qualified)
