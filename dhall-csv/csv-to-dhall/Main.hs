{-#LANGUAGE OverloadedStrings#-}

module Main where

import Control.Exception    (SomeException)
import Dhall.CsvToDhall     (dhallFromCsv)
import Dhall.Pretty         (CharacterSet (..))

import qualified Control.Exception
import qualified Data.Text.IO                              as Text.IO
import qualified Dhall.Csv.Util
import qualified Dhall.Util
import qualified GHC.IO.Encoding
import qualified System.IO as IO
import qualified System.Exit

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    let toCsv file = do
            text <- case file of
                    Nothing -> Text.IO.getContents
                    Just path -> Text.IO.readFile path

            case Dhall.Csv.Util.decodeCsvDefault text of
                Left err -> fail err
                Right csv -> pure csv

    handle $ do
        csv <- toCsv Nothing

        let expression = dhallFromCsv csv

        Dhall.Util.renderExpression Unicode False Nothing expression


handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        IO.hPutStrLn IO.stderr ""
        IO.hPrint    IO.stderr e
        System.Exit.exitFailure
