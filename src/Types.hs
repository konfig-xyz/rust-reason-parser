module Types where

import qualified Data.Map as M
import qualified Data.Set as S

------------------
-- Schema Types
------------------
type TypePair = (String, String)

type Table = (String, [TypePair])

type Schema = [Table]

data Visibility a = Visible a | Hidden
     deriving (Eq, Ord)
------------------
-- Configuration Types
------------------

type Mapping = M.Map String String

type Hidden = S.Set String

type HiddenQualified = M.Map String (S.Set String)

data Configuration = Configuration
  { aliases :: Mapping,
    base :: Mapping,
    nested :: Mapping,
    tables :: Hidden,
    keys :: Hidden,
    qualified :: HiddenQualified
  }
  deriving (Show)
