{-# LANGUAGE OverloadedStrings #-}

module Main where

import ConfigurationParser
import qualified Data.Text as T
import SchemaParser
import SchemaPrinter
import System.Environment (getArgs)
import Types

main :: IO ()
main = do
  [configFile, schemaFile] <- getArgs

  configuration <- makeConfig configFile
  contents <- readFile schemaFile

  putStrLn $
    T.unpack $
      case parseSchema (T.pack contents) of
        Left err -> T.pack $ show err
        Right schema -> printTypeAliases configuration <> printSchema configuration schema
