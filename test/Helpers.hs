module Helpers (genSafeChar, genSafeString, SafeString, toText, checkN) where

import Data.Text
import Test.QuickCheck

checkN x = quickCheckWith (stdArgs {maxSuccess = x})

genSafeChar :: Gen Char
genSafeChar = elements $ ['a' .. 'z'] ++ ['A' .. 'Z']

genSafeString :: Gen Text
genSafeString = pack <$> listOf genSafeChar

newtype SafeString = SafeString {toText :: Text}
  deriving (Show)

instance Arbitrary SafeString where
  arbitrary = SafeString <$> genSafeString

genType :: Gen String
genType = listOf genSafeChar
