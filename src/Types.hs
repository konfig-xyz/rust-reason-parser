module Types where

import qualified Data.Map as M
import qualified Data.Set as S

type TypePair = (String, String)

type Table = (String, [TypePair])

type Schema = [Table]

type Mapping = M.Map String String

type Hidden = S.Set String

data Visibility a = Visible a | Hidden
     deriving (Eq, Ord)

data Configuration = Configuration
  { aliases :: Mapping,
    base :: Mapping,
    nested :: Mapping,
    tables :: Hidden,
    keys :: Hidden
  }
  deriving (Show)
