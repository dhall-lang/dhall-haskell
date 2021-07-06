module Dhall.Csv.Util (encodeCsvDefault, decodeCsvDefault) where

import Data.List      (sort)
import Data.Text      (Text)

import qualified Data.ByteString.Lazy   as ByteString
import qualified Data.Csv
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.Text.Encoding
import qualified Data.Vector            as Vector

encodeCsvDefault :: [Data.Csv.NamedRecord] -> Text
encodeCsvDefault csv = Data.Text.Encoding.decodeUtf8 $ ByteString.toStrict $ Data.Csv.encodeByName header csv
  where
    header = case csv of
        [] -> Vector.empty
        (m:_) -> Vector.fromList $ sort $ HashMap.keys m

decodeCsvDefault :: Text -> Either String [Data.Csv.NamedRecord]
decodeCsvDefault txt = do
    (_, vec) <- Data.Csv.decodeByName $ ByteString.fromStrict $ Data.Text.Encoding.encodeUtf8 txt
    return $ Vector.toList vec
