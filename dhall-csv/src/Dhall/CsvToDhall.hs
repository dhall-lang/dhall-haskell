module Dhall.CsvToDhall (dhallFromCsv) where

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
dhallFromCsv csv = Core.ListLit Nothing $ Sequence.fromList $ map convertRecord csv
  where
    convertRecord :: Data.Csv.NamedRecord -> Expr Src Void
    convertRecord recordCsv = Core.RecordLit dhallMap
      where
        recordTextKeys = HashMap.mapKeys decodeUtf8 recordCsv
        recordDhallFields = HashMap.map convertField recordTextKeys
        dhallMap = Dhall.Map.fromList $ HashMap.toList recordDhallFields
    convertField :: Data.Csv.Field -> RecordField Src Void
    convertField = Core.makeRecordField . Core.TextLit . (Core.Chunks []) . decodeUtf8
