module Types where

import qualified Data.Map as M
import qualified Data.Set as S

type TypePair = (String, String)

type Table = (String, [String])

type Schema = [Table]

type Mapping = M.Map String String
type Hidden = S.Set String
