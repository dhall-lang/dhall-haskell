{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import Control.Applicative ((<|>))
import Control.Exception (Exception, SomeException)
import Data.Monoid (mempty, (<>))
import Data.Text.Prettyprint.Doc (Pretty)
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import Dhall.Core (Expr, Import)
import Dhall.Import (Imported(..), load)
import Dhall.Parser (Src)
import Dhall.Pretty (annToAnsiStyle, prettyExpr)
import Dhall.TypeCheck (DetailedTypeError(..), TypeError, X)
import Options.Applicative (Parser)
import System.Exit (exitFailure)
import System.IO (Handle)

import qualified Paths_dhall as Meta

import qualified Control.Exception
import qualified Data.Text.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Dhall.Core
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Options.Applicative
import qualified System.Console.ANSI
import qualified System.IO

data Options = Options
    { mode    :: Mode
    , explain :: Bool
    , plain   :: Bool
    }

data Mode = Default | Version | Resolve | Type | Normalize

parseOptions :: Parser Options
parseOptions = Options <$> parseMode <*> parseExplain <*> parsePlain
  where
    parseExplain =
        Options.Applicative.switch
            (   Options.Applicative.long "explain"
            <>  Options.Applicative.help "Explain error messages in more detail"
            )

    parsePlain =
        Options.Applicative.switch
            (   Options.Applicative.long "plain"
            <>  Options.Applicative.help "Disable syntax highlighting"
            )

parseMode :: Parser Mode
parseMode =
        subcommand "version"   "Display version"                 Version
    <|> subcommand "resolve"   "Resolve an expression's imports" Resolve
    <|> subcommand "type"      "Infer an expression's type"      Type
    <|> subcommand "normalize" "Normalize an expression"         Normalize
    <|> pure Default
  where
    subcommand name description mode =
        Options.Applicative.subparser
            (   Options.Applicative.command name parserInfo
            <>  Options.Applicative.metavar name
            )
      where
        parserInfo =
            Options.Applicative.info parser
                (   Options.Applicative.fullDesc
                <>  Options.Applicative.progDesc description
                )

        parser =
            Options.Applicative.helper <*> pure mode

opts :: Pretty.LayoutOptions
opts =
    Pretty.defaultLayoutOptions
        { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }

data ImportResolutionDisabled =
    ImportResolutionDisabled deriving (Exception, Typeable)

instance Show ImportResolutionDisabled where
    show _ = "\nImport resolution is disabled"

throws :: Exception e => Either e a -> IO a
throws (Left  e) = Control.Exception.throwIO e
throws (Right a) = return a

getExpression :: IO (Expr Src Import)
getExpression = do
    inText <- Data.Text.IO.getContents

    throws (Dhall.Parser.exprFromText "(stdin)" inText)

assertNoImports :: Expr Src Import -> IO (Expr Src X)
assertNoImports expression =
    throws (traverse (\_ -> Left ImportResolutionDisabled) expression)

main :: IO ()
main = do
    let parserInfo =
            Options.Applicative.info
                (Options.Applicative.helper <*> parseOptions)
                (   Options.Applicative.progDesc "Interpreter for the Dhall language"
                <>  Options.Applicative.fullDesc
                )

    Options {..} <- Options.Applicative.execParser parserInfo

    System.IO.hSetEncoding System.IO.stdin System.IO.utf8

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

    let render :: Pretty a => Handle -> Expr s a -> IO ()
        render h e = do
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

    handle $ case mode of
        Version -> do
            putStrLn (showVersion Meta.version)

        Default -> do
            expression <- getExpression

            resolvedExpression <- load expression

            inferredType <- throws (Dhall.TypeCheck.typeOf resolvedExpression)

            render System.IO.stderr (Dhall.Core.normalize inferredType)

            Data.Text.Lazy.IO.hPutStrLn System.IO.stderr mempty

            render System.IO.stdout (Dhall.Core.normalize resolvedExpression)

        Resolve -> do
            expression <- getExpression

            resolvedExpression <- load expression

            render System.IO.stdout resolvedExpression

        Normalize -> do
            expression <- getExpression

            resolvedExpression <- assertNoImports expression

            _ <- throws (Dhall.TypeCheck.typeOf resolvedExpression)

            render System.IO.stdout (Dhall.Core.normalize resolvedExpression)

        Type -> do
            expression <- getExpression

            resolvedExpression <- assertNoImports expression

            inferredType <- throws (Dhall.TypeCheck.typeOf resolvedExpression)

            render System.IO.stdout (Dhall.Core.normalize inferredType)
