module Types where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

------------------
-- Schema Types
------------------
type TypePair = (T.Text, T.Text)

type Table = (T.Text, [TypePair])

type Schema = [Table]

data Visibility a = Visible a | Hidden
  deriving (Eq, Ord)

------------------
-- Configuration Types
------------------
type Mapping = M.Map T.Text T.Text

type Hidden = S.Set T.Text

type HiddenQualified = M.Map T.Text (S.Set T.Text)

data Configuration = Configuration
  { aliasPPX :: [T.Text],
    typePPX :: [T.Text],
    containerizedPPX :: [T.Text],
    aliases :: Mapping,
    containerized :: Mapping,
    base :: Mapping,
    nested :: Mapping,
    qualifiedTypes :: Mapping,
    tables :: Hidden,
    keys :: Hidden,
    qualified :: HiddenQualified
  }
  deriving (Show)
