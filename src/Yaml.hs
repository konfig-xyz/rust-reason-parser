{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Yaml
  ( -- * Types
    Config (..),
    KeyError (..),
    Key,

    -- * Loading
    load,

    -- * Access functions
    keys,
    subconfig,
    lookup,
    lookupDefault,
    fullpath,
  )
where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (Exception, throw)
import Control.Monad (foldM)
import qualified Data.Aeson as Aeson
import Data.Aeson.Key (fromText, toText)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as ST
import Data.Typeable (Typeable)
import Data.Yaml (FromJSON (parseJSON), Object, parseMaybe)
import qualified Data.Yaml as Yaml
import Prelude hiding (lookup)

-- | Config or field name
type Key = Aeson.Key

type QueryKey = ST.Text

-- | This error can be raised if config has not target path.
newtype KeyError = KeyError Key
  deriving (Show, Typeable)

instance Exception KeyError

-- | Type contains config section and path from root.
data Config = Config [Key] Object
  deriving (Eq, Show)

instance NFData Config where
  rnf (Config p o) = rnf p `seq` rnf o

ke :: (Monad m) => Key -> m a
ke = throw . KeyError

-- | Returns full path from the root to the given key.
-- Levels are separated by dots.
--
-- >>> fullpath sub "field1"
-- "section1.field1"
fullpath :: Config -> Key -> Key
fullpath (Config parents _) path =
  fromText $ ST.intercalate "." $ reverse $ map toText (path : parents)

newtype YamlException = Error String
  deriving (Show, Typeable)

instance Exception YamlException

validate f (Right object) = Config [] object
validate f (Left err) = throw $ Error $ show err

-- | Attempts to load a config from a given YAML file.
-- Fails with @InvalidYaml@ if the file does not exist.
--
-- >>> config <- load "example.yaml"
load :: FilePath -> IO Config
load f = do
  parse <- Yaml.decodeFileEither f
  return $ validate f parse

-- | Returns all toplevel keys in a config.
--
-- >>> keys config
-- ["section1","section2"]
keys :: Config -> [QueryKey]
keys (Config _ o) = map toText $ KeyMap.keys o

-- | Returns a value for a given key.
-- Fails with a @KeyError@ if the key doesn't exist.
--
-- >>> keys sub
-- ["field1","field2"]
-- >>> putStrLn =<< lookup "field1" sub
-- value1
lookup ::
  (Monad m, FromJSON a) =>
  -- | Field name
  QueryKey ->
  -- | Config to query
  Config ->
  -- | Looked up value
  m a
lookup path c = maybe err return $ lookupMaybe path c
  where
    err = ke $ "Field " <> fullpath c (fromText path) <> " not found or has wrong type."

-- | An exception-free alternative to @lookup@.
--
-- >>> keys sub
-- ["field1","field2"]
-- >>> lookupMaybe "field1" sub
-- Just "value1"
lookupMaybe :: (FromJSON a) => QueryKey -> Config -> Maybe a
lookupMaybe path conf =
  foldM (flip subconfig) conf (init $ map fromText pathes)
    >>= look (last $ map fromText pathes)
  where
    look k (Config _ o) = KeyMap.lookup k o >>= parseMaybe parseJSON
    pathes = ST.splitOn "." path

-- | Returns a value for a given key or a default value if a key doesn't exist.
--
-- >>> lookupDefault "field3" "def" sub
-- "def"
lookupDefault ::
  (FromJSON a) =>
  -- | Field name
  QueryKey ->
  -- | Default value
  a ->
  -- | Config to query
  Config ->
  -- | Looked up or default value
  a
lookupDefault p d = fromMaybe d . lookupMaybe p

-- | Narrows into a config section corresponding to a given key.
-- Fails with a @KeyError@ if a key doesn't exist at the current level.
--
-- >>> :set -XOverloadedStrings
-- >>> sub <- subconfig "section1" config
subconfig ::
  (Monad m) =>
  -- | Subconfig name
  Key ->
  -- | (Sub)Config to narrow into
  Config ->
  -- | Subconfig
  m Config
subconfig path c@(Config parents o) = case KeyMap.lookup path o of
  Just (Yaml.Object so) -> return $ Config (path : parents) so
  Just Yaml.Null -> return $ Config (path : parents) KeyMap.empty
  Nothing -> err
  where
    err = ke $ "Subconfig " <> fullpath c path <> " not found."
