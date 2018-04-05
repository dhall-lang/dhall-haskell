{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Exception (SomeException)
import Control.Monad (when)
import Data.Monoid (mempty, (<>))
import Data.Version (showVersion)
import Dhall.Core (normalize)
import Dhall.Import (Imported(..), load)
import Dhall.Parser (Src)
import Dhall.Pretty (annToAnsiStyle, prettyExpr)
import Dhall.TypeCheck (DetailedTypeError(..), TypeError, X)
import Options.Applicative (Parser, ParserInfo)
import System.Exit (exitFailure, exitSuccess)

import qualified Paths_dhall as Meta

import qualified Control.Exception
import qualified Data.Text.Lazy.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Options.Applicative
import qualified System.Console.ANSI
import qualified System.IO

data Options = Options
    { explain :: Bool
    , version :: Bool
    , plain   :: Bool
    }

parseOptions :: Parser Options
parseOptions = Options <$> parseExplain <*> parseVersion <*> parsePlain
  where
    parseExplain =
        Options.Applicative.switch
            (   Options.Applicative.long "explain"
            <>  Options.Applicative.help "Explain error messages in more detail"
            )

    parseVersion =
        Options.Applicative.switch
            (   Options.Applicative.long "version"
            <>  Options.Applicative.help "Display version and exit"
            )

    parsePlain =
        Options.Applicative.switch
            (   Options.Applicative.long "plain"
            <>  Options.Applicative.help "Disable syntax highlighting"
            )

opts :: Pretty.LayoutOptions
opts =
    Pretty.defaultLayoutOptions
        { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }

parserInfo :: ParserInfo Options
parserInfo =
    Options.Applicative.info
        (Options.Applicative.helper <*> parseOptions)
        (   Options.Applicative.progDesc "Interpreter for the Dhall language"
        <>  Options.Applicative.fullDesc
        )

main :: IO ()
main = do
    Options {..} <- Options.Applicative.execParser parserInfo
    when version $ do
      putStrLn (showVersion Meta.version)
      exitSuccess

    let handle =
                Control.Exception.handle handler2
            .   Control.Exception.handle handler1
            .   Control.Exception.handle handler0
          where
            handler0 e = do
                let _ = e :: TypeError Src X
                System.IO.hPutStrLn System.IO.stderr ""
                if explain
                    then Control.Exception.throwIO (DetailedTypeError e)
                    else do
                        Data.Text.Lazy.IO.hPutStrLn System.IO.stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO e

            handler1 (Imported ps e) = do
                let _ = e :: TypeError Src X
                System.IO.hPutStrLn System.IO.stderr ""
                if explain
                    then Control.Exception.throwIO (Imported ps (DetailedTypeError e))
                    else do
                        Data.Text.Lazy.IO.hPutStrLn System.IO.stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO (Imported ps e)

            handler2 e = do
                let _ = e :: SomeException
                System.IO.hSetEncoding System.IO.stderr System.IO.utf8
                System.IO.hPrint System.IO.stderr e
                System.Exit.exitFailure

    handle (do
        System.IO.hSetEncoding System.IO.stdin System.IO.utf8
        inText <- Data.Text.Lazy.IO.getContents

        expr <- case Dhall.Parser.exprFromText "(stdin)" inText of
            Left  err -> Control.Exception.throwIO err
            Right x   -> return x

        let render h e = do
                let doc = prettyExpr e

                let layoutOptions = opts
                let stream = Pretty.layoutSmart layoutOptions doc

                supportsANSI <- System.Console.ANSI.hSupportsANSI h
                let ansiStream =
                        if supportsANSI && not plain
                        then fmap annToAnsiStyle stream
                        else Pretty.unAnnotateS stream

                Pretty.renderIO h ansiStream
                Data.Text.Lazy.IO.hPutStrLn h ""


        expr' <- load expr

        typeExpr <- case Dhall.TypeCheck.typeOf expr' of
            Left  err      -> Control.Exception.throwIO err
            Right typeExpr -> return typeExpr

        render System.IO.stderr (normalize typeExpr)
        Data.Text.Lazy.IO.hPutStrLn System.IO.stderr mempty
        render System.IO.stdout (normalize expr') )
