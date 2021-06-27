{-#LANGUAGE OverloadedStrings#-}

module Main where

import Control.Exception    (SomeException, throwIO)
import Dhall.CsvToDhall     (dhallFromCsv)
import Dhall.Pretty         (CharacterSet (..))

import qualified Control.Exception
import qualified Data.Text.IO                              as Text.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty.Terminal
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty.Text
import qualified Dhall.Csv.Util
import qualified Dhall.Pretty
import qualified GHC.IO.Encoding
import qualified System.Console.ANSI                       as ANSI
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
                Left err -> throwIO (userError err)
                Right csv -> pure csv

    let renderExpression characterSet plain output expression = do
            let document = Dhall.Pretty.prettyCharacterSet characterSet expression

            let stream = Dhall.Pretty.layout document

            case output of
                Nothing -> do
                    supportsANSI <- ANSI.hSupportsANSI IO.stdout

                    let ansiStream =
                            if supportsANSI && not plain
                            then fmap Dhall.Pretty.annToAnsiStyle stream
                            else Pretty.unAnnotateS stream

                    Pretty.Terminal.renderIO IO.stdout ansiStream

                    Text.IO.putStrLn ""

                Just file_ ->
                    IO.withFile file_ IO.WriteMode $ \h -> do
                        Pretty.Text.renderIO h stream

                        Text.IO.hPutStrLn h ""

    handle $ do
        csv <- toCsv Nothing

        let expression = dhallFromCsv csv

        renderExpression Unicode False Nothing expression


handle :: IO a -> IO a
handle = Control.Exception.handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
        IO.hPutStrLn IO.stderr ""
        IO.hPrint    IO.stderr e
        System.Exit.exitFailure
