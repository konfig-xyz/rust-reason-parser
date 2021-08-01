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

  print $ containerized configuration
  putStrLn $ T.unpack $ printTypeAliases configuration <> printSchema configuration (parseSchema $ T.pack contents)
