{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO
import qualified Dhall.Csv
import qualified Dhall.Csv.Util
import qualified GHC.IO.Encoding

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    text <- Data.Text.IO.getContents
    csv <- Dhall.Csv.codeToValue Nothing text

    Data.Text.IO.putStr $ Dhall.Csv.Util.encodeCsvDefault csv
