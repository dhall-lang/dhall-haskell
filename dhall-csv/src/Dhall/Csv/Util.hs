{-# LANGUAGE OverloadedStrings   #-}

module Dhall.Csv.Util (encodeCsvDefault, decodeCsvDefault) where

import Data.List      (sort)
import Data.Text      (Text)

import qualified Data.ByteString.Lazy       as ByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString.Char8
import qualified Data.Csv
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Text.Encoding
import qualified Data.Vector                as Vector

encodeCsvDefault :: [Data.Csv.NamedRecord] -> Text
encodeCsvDefault csv = Data.Text.Encoding.decodeUtf8 $ ByteString.toStrict $ Data.Csv.encodeByName header csv
  where
    header = case csv of
        [] -> Vector.empty
        (m:_) -> Vector.fromList $ sort $ HashMap.keys m

decodeCsvDefault :: Bool -> Text -> Either String [Data.Csv.NamedRecord]
decodeCsvDefault hasHeader
    | hasHeader = decodeCsvWithHeader
    | otherwise = decodeCsvNoHeader

decodeCsvWithHeader :: Text -> Either String [Data.Csv.NamedRecord]
decodeCsvWithHeader txt = do
    (_, vec) <- Data.Csv.decodeByName $ ByteString.fromStrict $ Data.Text.Encoding.encodeUtf8 txt
    return $ Vector.toList vec

decodeCsvNoHeader :: Text -> Either String [Data.Csv.NamedRecord]
decodeCsvNoHeader txt = do
    vec <- Data.Csv.decode Data.Csv.NoHeader $ ByteString.fromStrict $ Data.Text.Encoding.encodeUtf8 txt
    return $ map addDefaultHeader $ Vector.toList vec

addDefaultHeader :: Data.Csv.Record -> Data.Csv.NamedRecord
addDefaultHeader = HashMap.fromList . (zip headerBS) . Vector.toList
  where
    header = map (('_' :) . show) ([1..] :: [Int])
    headerBS = map (ByteString.toStrict . ByteString.Char8.pack) header

