{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy
import qualified Data.Csv
import qualified Data.HashMap.Strict
import qualified Data.Text.IO
import qualified Data.Vector
import qualified Dhall.Csv
import qualified GHC.IO.Encoding

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    text <- Data.Text.IO.getContents
    csv <- Dhall.Csv.codeToValue Nothing text

    let header = case csv of
            [] -> Data.Vector.empty
            (m:_) -> Data.Vector.fromList $ Data.HashMap.Strict.keys m

    Data.ByteString.Lazy.putStr $ Data.Csv.encodeByName header csv
