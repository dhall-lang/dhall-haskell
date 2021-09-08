{-# LANGUAGE OverloadedStrings #-}

module Dhall.Csv.Util (encodeCsvDefault, decodeCsvDefault) where

import Data.Csv  (NamedRecord, Record)
import Data.List (sort)
import Data.Text (Text)

import qualified Data.ByteString.Lazy       as ByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString.Char8
import qualified Data.Csv
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Text.Encoding
import qualified Data.Vector                as Vector

{-| Utility to convert a list of @NamedRecord@ to Text formatted as a CSV.
-}
encodeCsvDefault :: [NamedRecord] -> Text
encodeCsvDefault csv = Data.Text.Encoding.decodeUtf8 $ ByteString.toStrict $ Data.Csv.encodeByName header csv
  where
    header = case csv of
        [] -> Vector.empty
        (m:_) -> Vector.fromList $ sort $ HashMap.keys m

{-| Utility to decode a CSV into a list of records.
    Must specify whether the CSV to decode has header or not.
-}
decodeCsvDefault :: Bool -> Text -> Either String [NamedRecord]
decodeCsvDefault hasHeader
    | hasHeader = decodeCsvWithHeader
    | otherwise = decodeCsvNoHeader

decodeCsvWithHeader :: Text -> Either String [NamedRecord]
decodeCsvWithHeader txt = do
    (_, vec) <- Data.Csv.decodeByName $ ByteString.fromStrict $ Data.Text.Encoding.encodeUtf8 txt
    return $ Vector.toList vec

decodeCsvNoHeader :: Text -> Either String [NamedRecord]
decodeCsvNoHeader txt = do
    vec <- Data.Csv.decode Data.Csv.NoHeader $ ByteString.fromStrict $ Data.Text.Encoding.encodeUtf8 txt
    return $ map addDefaultHeader $ Vector.toList vec

addDefaultHeader :: Record -> NamedRecord
addDefaultHeader = HashMap.fromList . (zip headerBS) . Vector.toList
  where
    header = map (('_' :) . show) ([1..] :: [Int])
    headerBS = map (ByteString.toStrict . ByteString.Char8.pack) header

