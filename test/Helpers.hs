module Helpers (genSafeChar, genSafeString, SafeString, safeStringToText, typeToString, checkN) where

import Data.Text
import Test.QuickCheck

checkN x = quickCheckWith (stdArgs {maxSuccess = x})

genSafeChar :: Gen Char
genSafeChar = elements $ ['a' .. 'z'] ++ ['A' .. 'Z']

genSafeString :: Gen Text
genSafeString = pack <$> listOf genSafeChar

newtype SafeString = SafeString {safeStringToText :: Text}
  deriving (Show)

instance Arbitrary SafeString where
  arbitrary = SafeString <$> genSafeString

genType :: Gen Text
genType = do
  lhs <- listOf genSafeChar
  rhs <- listOf genSafeChar
  pure $ pack $ lhs <> "->" <> rhs <> ","

newtype SchemaTypeDefinition = SchemaTypeDefinition {typeToString :: Text}
  deriving (Show)

instance Arbitrary SchemaTypeDefinition where
  arbitrary = SchemaTypeDefinition <$> genType
