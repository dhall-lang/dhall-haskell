{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-| Utility executable for pretty-printing Dhall code

    You typically want to use this to either:

    * improve the readability of Dhall code (either written or generated)
    * automatically format your Dhall code to avoid stylistic debates

    Note that this does not yet support:

    * Preserving all comments
        * Currently, this only preserves all leading comments and whitespace
          up until the last newline preceding the code
        * This lets you preserve a comment header but if you want to document
          subexpressions then you will need to split them into a separate
          file for now
    * Preserving multi-line strings (this reduces them to ordinary strings)

    See the @Dhall.Tutorial@ module for example usage
-}
module Main where

import Control.Applicative (optional)
import Control.Exception (SomeException)
import Control.Monad (when)
import Data.Monoid ((<>))
import Data.Version (showVersion)
import Dhall.Parser (exprAndHeaderFromText)
import Dhall.Pretty (annToAnsiStyle, prettyExpr)
import Options.Applicative (Parser, ParserInfo)
import System.IO (stderr)
import System.Exit (exitFailure, exitSuccess)

import qualified Paths_dhall as Meta

import qualified Control.Exception
import qualified Data.Text.IO
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Options.Applicative
import qualified System.Console.ANSI
import qualified System.IO

data Options = Options
    { version :: Bool
    , inplace :: Maybe FilePath
    }

parseOptions :: Parser Options
parseOptions = Options <$> parseVersion <*> optional parseInplace
  where
    parseVersion =
        Options.Applicative.switch
        (   Options.Applicative.long "version"
        <>  Options.Applicative.help "Display version and exit"
        )

    parseInplace =
        Options.Applicative.strOption
        (   Options.Applicative.long "inplace"
        <>  Options.Applicative.help "Modify the specified file in-place"
        <>  Options.Applicative.metavar "FILE"
        )

opts :: Pretty.LayoutOptions
opts =
    Pretty.defaultLayoutOptions
        { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }

parserInfo :: ParserInfo Options
parserInfo =
    Options.Applicative.info
        (Options.Applicative.helper <*> parseOptions)
        (   Options.Applicative.progDesc "Formatter for the Dhall language"
        <>  Options.Applicative.fullDesc
        )

main :: IO ()
main = do
    Options {..} <- Options.Applicative.execParser parserInfo

    when version $ do
      putStrLn (showVersion Meta.version)
      exitSuccess

    let handler e = do
            let _ = e :: SomeException
            System.IO.hSetEncoding System.IO.stderr System.IO.utf8
            System.IO.hPrint stderr e
            System.Exit.exitFailure

    Control.Exception.handle handler (do
        case inplace of
            Just file -> do
                strictText <- Data.Text.IO.readFile file
                let lazyText = Data.Text.Lazy.fromStrict strictText
                (header, expr) <- case exprAndHeaderFromText "(stdin)" lazyText of
                    Left  err -> Control.Exception.throwIO err
                    Right x   -> return x

                let doc = Pretty.pretty header <> Pretty.pretty expr
                System.IO.withFile file System.IO.WriteMode (\handle -> do
                    Pretty.renderIO handle (Pretty.layoutSmart opts doc)
                    Data.Text.IO.hPutStrLn handle "" )
            Nothing -> do
                System.IO.hSetEncoding System.IO.stdin System.IO.utf8
                inText <- Data.Text.Lazy.IO.getContents

                (header, expr) <- case exprAndHeaderFromText "(stdin)" inText of
                    Left  err -> Control.Exception.throwIO err
                    Right x   -> return x

                let doc = Pretty.pretty header <> prettyExpr expr

                supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout

                if supportsANSI
                  then
                    Pretty.renderIO
                      System.IO.stdout
                      (fmap annToAnsiStyle (Pretty.layoutSmart opts doc))
                  else
                    Pretty.renderIO
                      System.IO.stdout
                      (Pretty.layoutSmart opts (Pretty.unAnnotate doc))
                Data.Text.IO.putStrLn "")
