{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Dhall.CsvToDhall (
      dhallFromCsv
    , parseConversion
    , defaultConversion
    , CompileError(..)
    , Conversion(..)
    ) where

import Control.Applicative      ((<|>))
import Data.Either              (rights)
import Data.Foldable            (toList)
import Data.List                ((\\))
import Data.Text                (Text)
import Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import Data.Void                (Void)
import Dhall.Core               (Expr)
import Dhall.Src                (Src)
import Numeric.Natural          (Natural)
import Options.Applicative      (Parser)
import Text.Read                (readMaybe)

import qualified Data.Csv
import qualified Data.HashMap.Strict         as HashMap
import qualified Data.Sequence               as Sequence
import qualified Dhall.Core                  as Core
import qualified Dhall.Map                   as Map
import qualified Options.Applicative         as O

-- ----------
-- Conversion
-- ----------

-- | JSON-to-dhall translation options
data Conversion = Conversion
    { strictRecs     :: Bool
    , unions         :: UnionConv
    } deriving Show

data UnionConv = UFirst | UNone | UStrict deriving (Show, Read, Eq)

-- | Default conversion options
defaultConversion :: Conversion
defaultConversion = Conversion
    { strictRecs     = False
    , unions         = UFirst
    }

-- ---------------
-- Command options
-- ---------------

-- | Standard parser for options related to the conversion method
parseConversion :: Parser Conversion
parseConversion = Conversion <$> parseStrict
                             <*> parseUnion
  where
    parseStrict =
            O.flag' True
            (  O.long "records-strict"
            <> O.help "Fail if any CSV fields are missing from the expected Dhall type"
            )
        <|> O.flag' False
            (  O.long "records-loose"
            <> O.help "Tolerate CSV fields not present within the expected Dhall type"
            )
        <|> pure True


-- | Parser for command options related to treating union types
parseUnion :: Parser UnionConv
parseUnion =
        uFirst
    <|> uNone
    <|> uStrict
    <|> pure UFirst -- defaulting to UFirst
  where
    uFirst  =  O.flag' UFirst
            (  O.long "unions-first"
            <> O.help "The first value with the matching type (successfully parsed all the way down the tree) is accepted, even if not the only possible match. (DEFAULT)"
            )
    uNone   =  O.flag' UNone
            (  O.long "unions-none"
            <> O.help "Unions not allowed"
            )
    uStrict =  O.flag' UStrict
            (  O.long "unions-strict"
            <> O.help "Error if more than one union values match the type (and parse successfully)"
            )

type ExprX = Expr Src Void

data CompileError
    = Unsupported ExprX
    | MissingKey Text
    | UnhandledKeys [Text] -- Keys in CSV but not in schema
    | Mismatch
        ExprX           -- Expected Dhall Type
        Data.Csv.Field  -- Actual field
        Text            -- Record key
    | ContainsUnion ExprX
    | UndecidableUnion
        ExprX           -- Expected Type
        Data.Csv.Field  -- CSV Field
        [ExprX]         -- Multiple conversions
    | UndecidableMissingUnion
        ExprX           -- Expected Type
        [ExprX]         -- Multiple Conversions

dhallFromCsv :: Conversion -> ExprX -> [Data.Csv.NamedRecord] -> Either CompileError ExprX
dhallFromCsv Conversion{..} typeExpr = listConvert (Core.normalize typeExpr)
  where
    listConvert :: ExprX -> [Data.Csv.NamedRecord] -> Either CompileError ExprX
    listConvert (Core.App Core.List recordType@(Core.Record _)) [] = return $ Core.ListLit (Just recordType) Sequence.empty
    listConvert (Core.App Core.List recordType) [] = Left $ Unsupported recordType
    listConvert (Core.App Core.List recordType) csv = do
        a <- traverse (recordConvert recordType) csv
        return $ Core.ListLit Nothing $ Sequence.fromList a
    listConvert e _ = Left $ Unsupported e

    recordConvert :: ExprX -> Data.Csv.NamedRecord -> Either CompileError ExprX
    recordConvert (Core.Record record) csvRecord
        | extraKeys <- (map decodeUtf8 $ HashMap.keys csvRecord) \\ Map.keys record
        , strictRecs && not (null extraKeys)
        = Left $ UnhandledKeys extraKeys
        | otherwise
        = do
            let f = (\k v -> fieldConvert k (Core.recordFieldValue v) (HashMap.lookup (encodeUtf8 k) csvRecord))
            a <- Map.traverseWithKey f record
            let a' = Map.mapWithKey (\_ e -> Core.makeRecordField e) a
            return $ Core.RecordLit a'
    recordConvert e _ = Left $ Unsupported e

    fieldConvert :: Text -> ExprX -> Maybe Data.Csv.Field -> Either CompileError ExprX
    -- Unions
    fieldConvert recordKey t@(Core.Union tm) maybeField = do
        let f unionKey Nothing =
                case maybeField of
                    Nothing -> Left $ MissingKey recordKey
                    Just field ->
                        if decodeUtf8 field == unionKey
                        then Right $ Core.Field t $ Core.makeFieldSelection unionKey
                        else Left $ Mismatch t field recordKey
            f unionKey (Just _type) = do
                expression <- fieldConvert recordKey _type maybeField
                return (Core.App (Core.Field t $ Core.makeFieldSelection unionKey) expression)

        case (unions, rights (toList (Map.mapWithKey f tm)), maybeField) of
            (UNone  , _         , _         ) -> Left $ ContainsUnion t
            (UStrict, xs@(_:_:_), Just field) -> Left $ UndecidableUnion t field xs
            (UStrict, xs@(_:_:_), Nothing   ) -> Left $ UndecidableMissingUnion t xs
            (_      , []        , Just field) -> Left $ Mismatch t field recordKey
            (_      , []        , Nothing   ) -> Left $ MissingKey recordKey
            (UFirst , x:_       , _         ) -> Right $ x
            (UStrict, [x]       , _         ) -> Right $ x

    -- Missing Optionals
    fieldConvert _ (Core.App Core.Optional t) Nothing = return $ Core.App Core.None t

    -- Missing fields
    fieldConvert key _ Nothing = Left $ MissingKey key

    -- Bools
    fieldConvert key Core.Bool (Just field) =
        case readMaybe (show field) :: Maybe Bool of
            Nothing -> Left $ Mismatch Core.Bool field key
            Just v  -> Right $ Core.BoolLit v

    -- Naturals
    fieldConvert key Core.Natural (Just field) =
        case readMaybe (show field) :: Maybe Natural of
            Nothing -> Left $ Mismatch Core.Natural field key
            Just v  -> Right $ Core.NaturalLit v

    -- Integers
    fieldConvert key Core.Integer (Just field) =
        case readMaybe (show field) :: Maybe Integer of
            Nothing -> Left $ Mismatch Core.Integer field key
            Just v  -> Right $ Core.IntegerLit v

    -- Doubles
    fieldConvert key Core.Double (Just field) =
        case readMaybe (show field) :: Maybe Double of
            Nothing -> Left $ Mismatch Core.Double field key
            Just v  -> Right $ Core.DoubleLit $ Core.DhallDouble v

    -- Text
    fieldConvert _ Core.Text (Just field) =
        return $ Core.TextLit $ Core.Chunks [] $ decodeUtf8 field

    -- Optionals
    fieldConvert key (Core.App Core.Optional t) maybeField = do
        expression <- fieldConvert key t maybeField
        return $ Core.Some expression

    fieldConvert _ t _ = Left $ Unsupported t
