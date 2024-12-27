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
  printPPXs 0 configuration (aliasPPX configuration) <> "type " <> x <> " = " <> y <> semi
  where
    semi = case language configuration of
      Rescript -> ""
      Reason -> ";"

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
    nesting = lBrack <> printTypeValue configuration value <> rBrack
    (lBrack, rBrack) = case language configuration of
      Rescript -> ("<", ">")
      Reason -> ("(", ")")

printTypeValue :: Configuration -> T.Text -> T.Text
printTypeValue configuration typeName
  | isJust key = fromJust key
  | otherwise = printTypeContainer configuration (runParser parseTypeContainer () "Error" typeName)
  where
    key = M.lookup typeName $ base configuration

adaptKeyPPX :: Configuration -> T.Text -> T.Text
adaptKeyPPX configuration typeName = case keyPPX configuration of
  Just keyPPX -> T.replace (T.pack "{}") typeName keyPPX <> " " <> snakeCaseTypename
  Nothing -> snakeCaseTypename
  where
    snakeCaseTypename = snakeToCamel typeName

printType :: Configuration -> T.Text -> TypePair -> T.Text
printType configuration tableName (typeName, typeValue)
  | S.member typeName mergedQualifiedKeys = "// " <> typeString
  | isJust qualifiedTypeString = fromJust qualifiedTypeString
  | otherwise = typeString
  where
    mergedQualifiedKeys = mergeQualified configuration tableName
    typeStringLHS = typeNameWithKeyPPX <> ": "
    typeNameWithKeyPPX = adaptKeyPPX configuration typeName
    qualifiedTypeString =
      fmap (typeStringLHS <>) $
        M.lookup (tableName <> "." <> typeName) $
          qualifiedTypes configuration
    typeString = typeStringLHS <> printTypeValue configuration typeValue

printModuleName :: T.Text -> T.Text
printModuleName xs = "module " <> snakeToPascal xs <> " = "

printTableName :: Configuration -> T.Text -> Visibility T.Text -> T.Text
printTableName configuration tableName (Visible types) = printModuleName tableName <> "{\n" <> types <> "\n}" <> semi
  where
    semi = case language configuration of
      Rescript -> ""
      Reason -> ";"
printTableName configuration tableName Hidden = "// " <> printModuleName tableName <> "{ }" <> semi
  where
    semi = case language configuration of
      Rescript -> ""
      Reason -> ";"

printPPXs :: Int -> Configuration -> [T.Text] -> T.Text
printPPXs i configuration = T.concat . map (\x -> Helpers.repeat i " " <> before <> x <> after <> "\n")
  where
    (before, after) = case language configuration of
      Rescript -> ("@", "")
      Reason -> ("[@", "]")

printTypePPXs :: Configuration -> T.Text
printTypePPXs configuration = printPPXs 2 configuration $ typePPX configuration

printContainerizedPPXs :: Configuration -> T.Text
printContainerizedPPXs configuration = printPPXs 2 configuration $ containerizedPPX configuration

printContainerTypeAliases :: Configuration -> T.Text
printContainerTypeAliases configuration = case p of
  [] -> T.pack ""
  xs -> "\n\n" <> T.intercalate "\n\n" p
  where
    p =
      ( \(x, y) ->
          printContainerizedPPXs configuration
            <> "  type "
            <> x
            <> " = "
            <> y
            <> lBrack
            <> "t"
            <> rBrack
            <> semi
      )
        <$> M.toList (containerized configuration)
    (lBrack, rBrack, semi) = case language configuration of
      Rescript -> ("<", ">", "")
      Reason -> ("(", ")", ";")

printTableTypes :: Configuration -> T.Text -> [TypePair] -> T.Text
printTableTypes configuration tableName xs =
  printTypePPXs configuration
    <> "  type t = {\n    "
    <> T.intercalate ",\n    " (fmap (printType configuration tableName) xs)
    <> ",\n  }"
    <> semi
    <> printContainerTypeAliases configuration
  where
    semi = case language configuration of
      Rescript -> ""
      Reason -> ";"

printTable :: Configuration -> Table -> T.Text
printTable configuration (tableName, types)
  | S.member tableName (tables configuration) = printTableName configuration tableName Hidden
  | otherwise = printTableName configuration tableName (Visible $ printTableTypes configuration tableName types)

printSchema :: Configuration -> Schema -> T.Text
printSchema configuration = T.intercalate "\n\n" . map (printTable configuration)
