{-# LANGUAGE OverloadedStrings #-}

module SchemaPrinter (printTypeAliases, printSchema) where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S
import qualified Data.Text as T
import Helpers
import SchemaParser
import Text.Parsec
import Types

printTypeAlias :: Configuration -> (T.Text, T.Text) -> T.Text
printTypeAlias configuration (x, y) = T.concat (aliasPPX configuration) <> "type " <> x <> " = " <> y <> ";"

printTypeAliases :: Configuration -> T.Text
printTypeAliases configuration = T.intercalate "\n" (map (printTypeAlias configuration) $ M.toList $ aliases configuration) <> "\n\n"

printTypeContainer :: Configuration -> Either ParseError (T.Text, T.Text) -> T.Text
printTypeContainer configuration (Left y) = T.pack $ "Parse error: " <> show y
printTypeContainer configuration (Right (x, value))
  | isJust key = fromJust key <> nesting
  | otherwise = x <> nesting
  where
    key = M.lookup x $ nested configuration
    nesting = "(" <> printTypeValue configuration value <> ")"

printTypeValue :: Configuration -> T.Text -> T.Text
printTypeValue configuration xs
  | isJust key = fromJust key
  | otherwise = printTypeContainer configuration (runParser parseTypeContainer () "Error" xs)
  where
    key = M.lookup xs $ base configuration

printType :: Configuration -> T.Text -> TypePair -> T.Text
printType configuration tableName (typeName, typeValue)
  | S.member typeName mergedQualifiedKeys = "// " <> typeString
  | otherwise = typeString
  where
    mergedQualifiedKeys = mergeQualified configuration tableName
    typeString = snakeToCamel typeName <> ": " <> printTypeValue configuration typeValue

printModuleName :: T.Text -> T.Text
printModuleName xs = "module " <> snakeToPascal xs <> " = "

printTableName :: T.Text -> Visibility T.Text -> T.Text
printTableName tableName (Visible types) = printModuleName tableName <> "{\n" <> types <> "\n};"
printTableName tableName Hidden = "// " <> printModuleName tableName <> "{ };"

printTableTypes :: Configuration -> T.Text -> [TypePair] -> T.Text
printTableTypes configuration tableName xs = T.concat (map ("  " <>) $ typePPX configuration) <> "  type t = {\n    " <> T.intercalate ",\n    " (map (printType configuration tableName) xs) <> ",\n  };"

printTable :: Configuration -> Table -> T.Text
printTable configuration (tableName, types)
  | S.member tableName (tables configuration) = printTableName tableName Hidden
  | otherwise = printTableName tableName (Visible $ printTableTypes configuration tableName types)

printSchema :: Configuration -> Schema -> T.Text
printSchema configuration = T.intercalate "\n\n" . map (printTable configuration)
