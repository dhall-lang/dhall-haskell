module Dhall.Csv.Util (encodeCsvDefault) where

import Data.Text (Text)

import qualified Data.ByteString.Lazy   as ByteString
import qualified Data.Csv
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.Vector            as Vector
import qualified Data.Text.Encoding

encodeCsvDefault :: [Data.Csv.NamedRecord] -> Text
encodeCsvDefault csv = Data.Text.Encoding.decodeUtf8 $ ByteString.toStrict $ Data.Csv.encodeByName header csv
  where
    header = case csv of
        [] -> Vector.empty
        (m:_) -> Vector.fromList $ HashMap.keys m
