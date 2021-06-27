module Dhall.CsvToDhall (dhallFromCsv) where

import Data.Bifunctor       (bimap)
import Data.Text.Encoding   (decodeUtf8)
import Data.Void            (Void)
import Dhall.Core           (Expr, RecordField)
import Dhall.Src            (Src)

import qualified Data.Csv
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.Sequence          as Sequence
import qualified Dhall.Core             as Core
import qualified Dhall.Map

dhallFromCsv :: [Data.Csv.NamedRecord] -> Expr Src Void
dhallFromCsv csv = Core.ListLit mType $ Sequence.fromList $ map convertRecord csv
  where
    mType :: Maybe (Expr Src Void)
    mType = case csv of
        [] -> Just $ Core.App Core.List $ Core.Record $ Dhall.Map.fromList []
        _ -> Nothing
    convertRecord :: Data.Csv.NamedRecord -> Expr Src Void
    convertRecord recordCsv = Core.RecordLit dhallMap
      where
        dhallMap = Dhall.Map.fromList $ map (bimap decodeUtf8 convertField) $ HashMap.toList recordCsv
    convertField :: Data.Csv.Field -> RecordField Src Void
    convertField = Core.makeRecordField . Core.TextLit . (Core.Chunks []) . decodeUtf8
