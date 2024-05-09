{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConfigurationParser (makeConfig) where

import Data.Bifunctor (first)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Helpers (parseTypeSplitBy)
import qualified Types as T
import Yaml
  ( Config,
    keys,
    load,
    lookup,
    lookupDefault,
    subconfig,
  )

parseTypeMapConfiguration :: T.Text -> Maybe (T.Text, T.Text)
parseTypeMapConfiguration = parseTypeSplitBy "->"

toTypeMap :: [T.Text] -> T.Mapping
toTypeMap = M.fromList . mapMaybe parseTypeMapConfiguration

toQualified :: Config -> T.HiddenQualified
toQualified xs = M.fromList $ map (\k -> (k, S.fromList $ lookupDefault k [] xs)) (keys xs)

toLanguage :: T.Text -> T.Language
toLanguage "reason" = T.Reason
toLanguage "rescript" = T.Rescript
toLanguage _ = T.Reason

makeConfig :: String -> IO T.Configuration
makeConfig path = do
  config <- load path
  types <- subconfig "types" config
  hiding <- subconfig "hiding" config
  annotations <- subconfig "annotations" config
  qualified <- subconfig "qualified" hiding

  pure $
    T.Configuration
      (toLanguage $ lookupDefault "language" "reason" config)
      (lookupDefault "alias-ppx" [] annotations)
      (lookupDefault "type-ppx" [] annotations)
      (lookupDefault "containerized-ppx" [] annotations)
      (toTypeMap $ lookupDefault "aliases" [] types)
      (toTypeMap $ lookupDefault "containerized" [] types)
      (toTypeMap $ lookupDefault "base" [] types)
      (toTypeMap $ lookupDefault "nested" [] types)
      (toTypeMap $ lookupDefault "qualified" [] types)
      (S.fromList $ lookupDefault "tables" [] hiding)
      (S.fromList $ lookupDefault "keys" [] hiding)
      (toQualified qualified)
