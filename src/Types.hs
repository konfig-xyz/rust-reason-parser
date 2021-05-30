module Types where

import qualified Data.Map as M

type TypePair = (String, String)

type Table = (String, [String])

type Schema = [Table]

type Mapping = M.Map String String
