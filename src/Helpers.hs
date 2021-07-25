module Helpers where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.Casing as C
import Types

mergeQualified :: Configuration -> T.Text -> Hidden
mergeQualified configuration typeName = case M.lookup typeName (qualified configuration) of
  Just xs -> S.union xs (keys configuration)
  Nothing -> keys configuration

snakeToCamel :: T.Text -> T.Text
snakeToCamel = T.pack . C.toCamel . C.fromSnake . T.unpack

snakeToPascal :: T.Text -> T.Text
snakeToPascal = T.pack . C.toPascal . C.fromSnake . T.unpack


parseTypeSplitBy :: T.Text -> T.Text -> Maybe (T.Text, T.Text)
parseTypeSplitBy c xs = case T.splitOn c xs of
  [x, y] -> Just (x, y) --TODO - trim whitespace
  _ -> Nothing
