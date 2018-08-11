{-| This module contains the top-level entrypoint and options parsing for the
    @dhall@ executable
-}

{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dhall.Main
    ( -- * Options
      Options(..)
    , Mode(..)
    , parseOptions
    , parserInfoOptions

      -- * Execution
    , command
    , main
    ) where

import Control.Applicative (optional, (<|>))
import Control.Exception (Exception, SomeException)
import Data.Monoid (mempty, (<>))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty)
import Data.Version (showVersion)
import Dhall.Binary (ProtocolVersion)
import Dhall.Core (Expr(..), Import)
import Dhall.Import (Imported(..))
import Dhall.Parser (Src)
import Dhall.Pretty (annToAnsiStyle, prettyExpr, layoutOpts)
import Dhall.TypeCheck (DetailedTypeError(..), TypeError, X)
import Lens.Family (set)
import Options.Applicative (Parser, ParserInfo)
import System.Exit (exitFailure)
import System.IO (Handle)

import qualified Paths_dhall as Meta

import qualified Control.Exception
import qualified Control.Monad.Trans.State.Strict          as State
import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Dhall
import qualified Dhall.Binary
import qualified Dhall.Core
import qualified Dhall.Diff
import qualified Dhall.Format
import qualified Dhall.Freeze
import qualified Dhall.Hash
import qualified Dhall.Import
import qualified Dhall.Lint
import qualified Dhall.Parser
import qualified Dhall.Repl
import qualified Dhall.TypeCheck
import qualified GHC.IO.Encoding
import qualified Options.Applicative
import qualified System.Console.ANSI
import qualified System.IO

-- | Top-level program options
data Options = Options
    { mode            :: Mode
    , explain         :: Bool
    , plain           :: Bool
    , protocolVersion :: ProtocolVersion
    }

-- | The subcommands for the @dhall@ executable
data Mode
    = Default { annotate :: Bool }
    | Version
    | Resolve
    | Type
    | Normalize
    | Repl
    | Format (Maybe FilePath)
    | Freeze (Maybe FilePath)
    | Hash
    | Diff Text Text
    | Lint (Maybe FilePath)

parseInplace :: Parser String
parseInplace =
        Options.Applicative.strOption
        (   Options.Applicative.long "inplace"
        <>  Options.Applicative.help "Modify the specified file in-place"
        <>  Options.Applicative.metavar "FILE"
        )

-- | `Parser` for the `Options` type
parseOptions :: Parser Options
parseOptions =
        Options
    <$> parseMode
    <*> parseExplain
    <*> parsePlain
    <*> Dhall.Binary.parseProtocolVersion
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
        subcommand "version"   "Display version"                 (pure Version)
    <|> subcommand "resolve"   "Resolve an expression's imports" (pure Resolve)
    <|> subcommand "type"      "Infer an expression's type"      (pure Type)
    <|> subcommand "normalize" "Normalize an expression"         (pure Normalize)
    <|> subcommand "repl"      "Interpret expressions in a REPL" (pure Repl)
    <|> subcommand "diff"      "Render the difference between the normal form of two expressions" diffParser
    <|> subcommand "hash"      "Compute semantic hashes for Dhall expressions" (pure Hash)
    <|> subcommand "lint"      "Improve Dhall code"              parseLint
    <|> formatSubcommand
    <|> freezeSubcommand
    <|> parseDefault
  where
    subcommand name description modeParser =
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
            Options.Applicative.helper <*> modeParser

    diffParser =
        Diff <$> argument "expr1" <*> argument "expr2"
      where
        argument =
                fmap Data.Text.pack
            .   Options.Applicative.strArgument
            .   Options.Applicative.metavar

    parseLint =
        Lint <$> optional parseInplace

    formatSubcommand =
        Options.Applicative.hsubparser
            (   Options.Applicative.command "format" parserInfo
            <>  Options.Applicative.metavar "format"
            )
      where parserInfo =
                Options.Applicative.info parserWithHelper
                    (   Options.Applicative.fullDesc
                    <>  Options.Applicative.progDesc "Formatter for the Dhall language"
                    )
            parserWithHelper = Options.Applicative.helper <*> parser
            parser = Format <$> optional parseInplace

    freezeSubcommand = subcommand "freeze" "Add hashes to all import statements of an expression" parseFreeze
        where
            parseFreeze = Freeze <$> optional parseInplace

    parseDefault = Default <$> parseAnnotate
      where
        parseAnnotate =
            Options.Applicative.switch
                (   Options.Applicative.long "annotate"
                )

data ImportResolutionDisabled = ImportResolutionDisabled deriving (Exception)

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

-- | `ParserInfo` for the `Options` type
parserInfoOptions :: ParserInfo Options
parserInfoOptions =
    Options.Applicative.info
        (Options.Applicative.helper <*> parseOptions)
        (   Options.Applicative.progDesc "Interpreter for the Dhall language"
        <>  Options.Applicative.fullDesc
        )

-- | Run the command specified by the `Options` type
command :: Options -> IO ()
command (Options {..}) = do
    GHC.IO.Encoding.setLocaleEncoding System.IO.utf8

    let status =
            set Dhall.Import.protocolVersion protocolVersion (Dhall.Import.emptyStatus ".")


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
                        Data.Text.IO.hPutStrLn System.IO.stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO e

            handler1 (Imported ps e) = do
                let _ = e :: TypeError Src X
                System.IO.hPutStrLn System.IO.stderr ""
                if explain
                    then Control.Exception.throwIO (Imported ps (DetailedTypeError e))
                    else do
                        Data.Text.IO.hPutStrLn System.IO.stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO (Imported ps e)

            handler2 e = do
                let _ = e :: SomeException
                System.IO.hPrint System.IO.stderr e
                System.Exit.exitFailure

    let render :: Pretty a => Handle -> Expr s a -> IO ()
        render h e = do
            let doc = prettyExpr e

            let stream = Pretty.layoutSmart layoutOpts doc

            supportsANSI <- System.Console.ANSI.hSupportsANSI h
            let ansiStream =
                    if supportsANSI && not plain
                    then fmap annToAnsiStyle stream
                    else Pretty.unAnnotateS stream

            Pretty.renderIO h ansiStream
            Data.Text.IO.hPutStrLn h ""

    handle $ case mode of
        Version -> do
            putStrLn (showVersion Meta.version)

        Default {..} -> do
            expression <- getExpression

            resolvedExpression <- State.evalStateT (Dhall.Import.loadWith expression) status

            inferredType <- throws (Dhall.TypeCheck.typeOf resolvedExpression)

            let normalizedExpression = Dhall.Core.normalize resolvedExpression

            let annotatedExpression =
                    if annotate
                        then Annot normalizedExpression inferredType
                        else normalizedExpression

            render System.IO.stdout annotatedExpression

        Resolve -> do
            expression <- getExpression

            resolvedExpression <- State.evalStateT (Dhall.Import.loadWith expression) status

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

        Repl -> do
            Dhall.Repl.repl explain protocolVersion

        Diff expr1 expr2 -> do
            expression1 <- Dhall.inputExpr expr1

            expression2 <- Dhall.inputExpr expr2

            let diff = Dhall.Diff.diffNormalized expression1 expression2
                prettyDiff = fmap annToAnsiStyle diff

            Pretty.hPutDoc System.IO.stdout prettyDiff

        Format inplace -> do
            Dhall.Format.format inplace

        Freeze inplace -> do
            Dhall.Freeze.freeze inplace protocolVersion

        Hash -> do
            Dhall.Hash.hash protocolVersion

        Lint inplace -> do
            case inplace of
                Just file -> do
                    text <- Data.Text.IO.readFile file

                    (header, expression) <- throws (Dhall.Parser.exprAndHeaderFromText file text)

                    let lintedExpression = Dhall.Lint.lint expression

                    let doc = Pretty.pretty header <> Pretty.pretty lintedExpression

                    System.IO.withFile file System.IO.WriteMode (\h -> do
                        Pretty.renderIO h (Pretty.layoutSmart layoutOpts doc)
                        Data.Text.IO.hPutStrLn h "" )
                Nothing -> do
                    text <- Data.Text.IO.getContents

                    (header, expression) <- throws (Dhall.Parser.exprAndHeaderFromText "(stdin)" text)

                    let lintedExpression = Dhall.Lint.lint expression

                    let doc = Pretty.pretty header <> prettyExpr lintedExpression

                    supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout

                    if supportsANSI
                      then
                        Pretty.renderIO
                          System.IO.stdout
                          (fmap annToAnsiStyle (Pretty.layoutSmart layoutOpts doc))
                      else
                        Pretty.renderIO
                          System.IO.stdout
                          (Pretty.layoutSmart layoutOpts (Pretty.unAnnotate doc))

-- | Entry point for the @dhall@ executable
main :: IO ()
main = do
    options <- Options.Applicative.execParser parserInfoOptions
    command options
