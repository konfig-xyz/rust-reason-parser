{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module SchemaParserSpec (run) where

import Data.Text
import Helpers (SafeString, checkN, toText)
import qualified SchemaParser
import Test.QuickCheck
import Text.Parsec

parseError = "Some Parse Error"

-- (String, String) -> Either ParseError (T.Text, T.Text)
parseToEither fn s1 s2 = fn (toText s1) (toText s2) == Right (strip $ toText s1, strip $ toText s2)

-- parseQualifiedType
parseQualifiedType :: Text -> Text -> Either ParseError (Text, Text)
parseQualifiedType s1 s2 = runParser SchemaParser.parseQualifiedType () parseError (s1 <> "." <> s2)

parseQualifiedTypeTest :: SafeString -> SafeString -> Bool
parseQualifiedTypeTest = parseToEither parseQualifiedType

-- ParseTypecontainer
parseTypeContainer :: Text -> Text -> Either ParseError (Text, Text)
parseTypeContainer s1 s2 = runParser SchemaParser.parseTypeContainer () parseError (s1 <> "<" <> s2 <> ">")

parseTypeContainerTest :: SafeString -> SafeString -> Bool
parseTypeContainerTest = parseToEither parseTypeContainer

-- ParseType
parseType :: Text -> Text -> Either ParseError (Text, Text)
parseType s1 s2 = runParser SchemaParser.parseType () parseError (s1 <> "->" <> s2 <> ",")

parseTypeTest :: SafeString -> SafeString -> Bool
parseTypeTest = parseToEither parseType

run :: IO ()
run = do
  checkN 1_000 parseTypeTest
  checkN 1_000 parseTypeContainerTest
  checkN 1_000 parseQualifiedTypeTest
