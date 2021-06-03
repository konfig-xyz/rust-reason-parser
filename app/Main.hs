module Main where

import ConfigurationParser
import SchemaParser
import SchemaPrinter
import System.Environment (getArgs)
import Types

main :: IO ()
main = do
  [configFile, schemaFile] <- getArgs

  configuration <- makeConfig configFile
  contents <- readFile schemaFile
  putStrLn $ printSchema configuration $ parseSchema contents

--putStrLn $ printTypeAliases (aliases configuration) <> parseSchema configuration contents
