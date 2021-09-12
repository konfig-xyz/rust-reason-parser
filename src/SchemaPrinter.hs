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
printTypeAlias configuration (x, y) =
  T.concat (aliasPPX configuration) <> "type " <> x <> " = " <> y <> ";"

printTypeAliases :: Configuration -> T.Text
printTypeAliases configuration =
  T.intercalate
    "\n"
    ( map (printTypeAlias configuration) $
        M.toList $
          aliases configuration
    )
    <> "\n\n"

printTypeContainer :: Configuration -> Either ParseError (T.Text, T.Text) -> T.Text
printTypeContainer configuration (Left y) = T.pack $ "Parse error: " <> show y
printTypeContainer configuration (Right (x, value))
  | isJust key = fromJust key <> nesting
  | otherwise = x <> nesting
  where
    key = M.lookup x $ nested configuration
    nesting = "(" <> printTypeValue configuration value <> ")"

printTypeValue :: Configuration -> T.Text -> T.Text
printTypeValue configuration typeName
  | isJust key = fromJust key
  | otherwise = printTypeContainer configuration (runParser parseTypeContainer () "Error" typeName)
  where
    key = M.lookup typeName $ base configuration

printType :: Configuration -> T.Text -> TypePair -> T.Text
printType configuration tableName (typeName, typeValue)
  | S.member typeName mergedQualifiedKeys = "// " <> typeString
  | isJust qualifiedTypeString = fromJust qualifiedTypeString
  | otherwise = typeString
  where
    mergedQualifiedKeys = mergeQualified configuration tableName
    typeStringLHS = snakeToCamel typeName <> ": "
    qualifiedTypeString =
      fmap (typeStringLHS <>) $
        M.lookup (tableName <> "." <> typeName) $
          qualifiedTypes configuration
    typeString = typeStringLHS <> printTypeValue configuration typeValue

printModuleName :: T.Text -> T.Text
printModuleName xs = "module " <> snakeToPascal xs <> " = "

printTableName :: T.Text -> Visibility T.Text -> T.Text
printTableName tableName (Visible types) = printModuleName tableName <> "{\n" <> types <> "\n};"
printTableName tableName Hidden = "// " <> printModuleName tableName <> "{ };"

printPPXs :: [T.Text] -> T.Text
printPPXs = T.concat . map ("  " <>)

printTypePPXs :: Configuration -> T.Text
printTypePPXs configuration = printPPXs $ typePPX configuration

printContainerizedPPXs :: Configuration -> T.Text
printContainerizedPPXs configuration = printPPXs $ containerizedPPX configuration

printContainerTypeAliases :: Configuration -> T.Text
printContainerTypeAliases configuration =
  T.intercalate "\n\n" $
    ( \(x, y) ->
        printContainerizedPPXs configuration
          <> "  type "
          <> x
          <> " = "
          <> y
          <> "(t);"
    )
      <$> M.toList (containerized configuration)

printTableTypes :: Configuration -> T.Text -> [TypePair] -> T.Text
printTableTypes configuration tableName xs =
  printTypePPXs configuration
    <> "  type t = {\n    "
    <> T.intercalate ",\n    " (fmap (printType configuration tableName) xs)
    <> ",\n  };\n\n"
    <> printContainerTypeAliases configuration

printTable :: Configuration -> Table -> T.Text
printTable configuration (tableName, types)
  | S.member tableName (tables configuration) = printTableName tableName Hidden
  | otherwise = printTableName tableName (Visible $ printTableTypes configuration tableName types)

printSchema :: Configuration -> Schema -> T.Text
printSchema configuration = T.intercalate "\n\n" . map (printTable configuration)
