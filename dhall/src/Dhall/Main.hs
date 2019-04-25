{-# LANGUAGE LambdaCase #-}
{-| This module contains the top-level entrypoint and options parsing for the
    @dhall@ executable
-}

{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

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
import Control.Exception (SomeException)
import Data.Monoid ((<>))
import Control.Monad.Reader
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty)
import Data.Version (showVersion)
import Data.IORef (readIORef)
import Dhall.Binary (StandardVersion)
import Dhall.Core (Expr(..), Import(..), ImportHashed(..), coerceEmbed)
import Dhall.Elaboration
import Dhall.Context
import Dhall.Import
import Dhall.Parser (Src, exprAndHeaderFromText)
import Dhall.Eval
import Dhall.Errors
import Dhall.Util (throws)
import Dhall.Pretty (Ann, CharacterSet(..), annToAnsiStyle, layoutOpts)
-- import Lens.Family (set)
import Options.Applicative (Parser, ParserInfo)
import System.Exit (exitFailure)
import System.IO (Handle)

import Data.Map.Strict (Map)
import Data.Set (Set)
import Text.Dot (Dot, NodeId, userNode, userNodeId, (.->.))

import qualified Codec.CBOR.JSON
import qualified Codec.CBOR.Read
import qualified Codec.CBOR.Write
import qualified Codec.Serialise
import qualified Control.Exception
import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
-- import qualified Dhall
import qualified Dhall.Binary
-- import qualified Dhall.Core
-- import qualified Dhall.Diff
import qualified Dhall.Format
-- import qualified Dhall.Freeze
-- import qualified Dhall.Hash
-- import qualified Dhall.Import

-- import qualified Dhall.Import.Types
import qualified Dhall.Lint
import qualified Dhall.Parser
import qualified Dhall.Pretty
-- import qualified Dhall.Repl
import qualified GHC.IO.Encoding
import qualified Options.Applicative
import qualified Paths_dhall as Meta
import qualified System.Console.ANSI
import qualified System.IO
import qualified Text.Dot
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

-- | Top-level program options
data Options = Options
    { mode            :: Mode
    , explain         :: Bool
    , plain           :: Bool
    , ascii           :: Bool
    , standardVersion :: StandardVersion
    }

-- | The subcommands for the @dhall@ executable
data Mode
    = Default { annotate :: Bool, alpha :: Bool }
    | Version
    | Resolve { resolveMode :: Maybe ResolveMode }
    | Type
    | Normalize { alpha :: Bool }
    | Repl
    | Format { formatMode :: Dhall.Format.FormatMode }
    | Freeze { inplace :: Maybe FilePath, all_ :: Bool }
    | Hash
    | Diff { expr1 :: Text, expr2 :: Text }
    | Lint { inplace :: Maybe FilePath }
    | Encode { json :: Bool }
    | Decode { json :: Bool }

data ResolveMode
    = Dot
    | ListTransitiveDependencies
    | ListImmediateDependencies


-- | `Parser` for the `Options` type
parseOptions :: Parser Options
parseOptions =
        Options
    <$> parseMode
    <*> switch "explain" "Explain error messages in more detail"
    <*> switch "plain" "Disable syntax highlighting"
    <*> switch "ascii" "Format code using only ASCII syntax"
    <*> Dhall.Binary.parseStandardVersion
  where
    switch name description =
        Options.Applicative.switch
            (   Options.Applicative.long name
            <>  Options.Applicative.help description
            )

subcommand :: String -> String -> Parser a -> Parser a
subcommand name description parser =
    Options.Applicative.hsubparser
        (   Options.Applicative.command name parserInfo
        <>  Options.Applicative.metavar name
        )
  where
    parserInfo =
        Options.Applicative.info parser
            (   Options.Applicative.fullDesc
            <>  Options.Applicative.progDesc description
            )

parseMode :: Parser Mode
parseMode =
        subcommand
            "version"
            "Display version"
            (pure Version)
    <|> subcommand
            "resolve"
            "Resolve an expression's imports"
            (Resolve <$> parseResolveMode)
    <|> subcommand
            "type"
            "Infer an expression's type"
            (pure Type)
    <|> subcommand
            "normalize"
            "Normalize an expression"
            (Normalize <$> parseAlpha)
    <|> subcommand
            "repl"
            "Interpret expressions in a REPL"
            (pure Repl)
    <|> subcommand
            "diff"
            "Render the difference between the normal form of two expressions"
            (Diff <$> argument "expr1" <*> argument "expr2")
    <|> subcommand
            "hash"
            "Compute semantic hashes for Dhall expressions"
            (pure Hash)
    <|> subcommand
            "lint"
            "Improve Dhall code"
            (Lint <$> optional parseInplace)
    <|> subcommand
            "format"
            "Formatter for the Dhall language"
            (Format <$> parseFormatMode)
    <|> subcommand
            "freeze"
            "Add integrity checks to remote import statements of an expression"
            (Freeze <$> optional parseInplace <*> parseAllFlag)
    <|> subcommand
            "encode"
            "Encode a Dhall expression to binary"
            (Encode <$> parseJSONFlag)
    <|> subcommand
            "decode"
            "Decode a Dhall expression from binary"
            (Decode <$> parseJSONFlag)
    <|> (Default <$> parseAnnotate <*> parseAlpha)
  where
    argument =
            fmap Data.Text.pack
        .   Options.Applicative.strArgument
        .   Options.Applicative.metavar

    parseAlpha =
        Options.Applicative.switch
            (   Options.Applicative.long "alpha"
            <>  Options.Applicative.help "α-normalize expression"
            )

    parseAnnotate =
        Options.Applicative.switch
            (   Options.Applicative.long "annotate"
            <>  Options.Applicative.help "Add a type annotation to the output"
            )

    parseResolveMode =
          Options.Applicative.flag' (Just Dot)
              (   Options.Applicative.long "dot"
              <>  Options.Applicative.help
                    "Output import dependency graph in dot format"
              )
        <|>
          Options.Applicative.flag' (Just ListImmediateDependencies)
              (   Options.Applicative.long "immediate-dependencies"
              <>  Options.Applicative.help
                    "List immediate import dependencies"
              )
        <|>
          Options.Applicative.flag' (Just ListTransitiveDependencies)
              (   Options.Applicative.long "transitive-dependencies"
              <>  Options.Applicative.help
                    "List transitive import dependencies"
              )
        <|> pure Nothing

    parseInplace =
        Options.Applicative.strOption
        (   Options.Applicative.long "inplace"
        <>  Options.Applicative.help "Modify the specified file in-place"
        <>  Options.Applicative.metavar "FILE"
        )

    parseJSONFlag =
        Options.Applicative.switch
        (   Options.Applicative.long "json"
        <>  Options.Applicative.help "Use JSON representation of CBOR"
        )

    parseAllFlag =
        Options.Applicative.switch
        (   Options.Applicative.long "all"
        <>  Options.Applicative.help "Add integrity checks to all imports (not just remote imports)"
        )

    parseCheck =
        Options.Applicative.switch
        (   Options.Applicative.long "check"
        <>  Options.Applicative.help "Only check if the input is formatted"
        )

    parseFormatMode = adapt <$> parseCheck <*> optional parseInplace
      where
        adapt True  path    = Dhall.Format.Check {..}
        adapt False inplace = Dhall.Format.Modify {..}

getExpression :: IO (Expr Src Import)
getExpression = do
    inText <- Data.Text.IO.getContents
    throws (Dhall.Parser.exprFromText "(stdin)" inText)

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
    let characterSet = case ascii of
            True  -> ASCII
            False -> Unicode

    GHC.IO.Encoding.setLocaleEncoding System.IO.utf8

    state <- (\s -> s {_standardVersion = standardVersion})
          <$> Dhall.Context.rootState "."

    let handle =
                Control.Exception.handle handler2
            .   Control.Exception.handle handler1
            .   Control.Exception.handle handler0
          where
            handler0 e = do
                let _ = e :: Dhall.Errors.ContextualError
                System.IO.hPutStrLn System.IO.stderr ""
                if explain
                    then Control.Exception.throwIO (DetailedContextualError e)
                    else do
                        Data.Text.IO.hPutStrLn System.IO.stderr
                           "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO e

            handler1 e = do
                let _ = e :: Dhall.Errors.ContextualError
                System.IO.hPutStrLn System.IO.stderr ""
                if explain
                    then Control.Exception.throwIO (DetailedContextualError e)
                    else do
                        Data.Text.IO.hPutStrLn System.IO.stderr
                          "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO e

            handler2 e = do
                let string = show (e :: SomeException)

                if not (null string)
                    then System.IO.hPutStrLn System.IO.stderr string
                    else return ()

                System.Exit.exitFailure

    let renderDoc :: Handle -> Doc Ann -> IO ()
        renderDoc h doc = do
            let stream = Pretty.layoutSmart layoutOpts doc

            supportsANSI <- System.Console.ANSI.hSupportsANSI h
            let ansiStream =
                    if supportsANSI && not plain
                    then fmap annToAnsiStyle stream
                    else Pretty.unAnnotateS stream

            Pretty.renderIO h ansiStream
            Data.Text.IO.hPutStrLn h ""

    let render :: Pretty a => Handle -> Expr s a -> IO ()
        render h expression = do
            let doc = Dhall.Pretty.prettyCharacterSet characterSet expression
            renderDoc h doc

    handle $ case mode of
        Version -> do
            let line₀ = "Haskell package version: "
                    <>  Data.Text.pack (showVersion Meta.version)

            let line₁ = "Standard version: "
                    <>  Dhall.Binary.renderStandardVersion Dhall.Binary.defaultStandardVersion

            Data.Text.IO.putStrLn line₀
            Data.Text.IO.putStrLn line₁

        Default {..} -> do
            expression <- getExpression
            (t, a) <- runReaderT (infer emptyCxt expression) state
            let processed =
                 (if alpha then alphaNormalize else id) $
                 (if annotate then (\t -> Annot t (coerceEmbed $ quote NEmpty a)) else id) $
                 (nfEmpty t)
            render System.IO.stdout processed

        Resolve (Just Dot) -> do
            let graphToDot :: Map Import (Int, Set Import) -> Dot NodeId
                graphToDot graph = do
                  forM_ (Map.assocs graph) $ \(imp, (i, _)) -> do
                    userNode
                        (userNodeId i)
                        [ ("label", show $ Pretty.pretty imp)
                        , ("shape", "box")
                        , ("style", "rounded")
                        ]
                  forM_ (Map.assocs graph) $ \(_, (i, imps)) -> do
                    forM_ (Set.elems imps) $ \imp' ->
                      userNodeId i .->. userNodeId (fst $ graph Map.! imp')
                  pure (userNodeId 0)

            expression <- getExpression
            (`runReaderT` state) $ do
              _ <- infer emptyCxt expression
              graph <- liftIO . readIORef =<< asks _graph
              liftIO $ putStr $ ("strict " <>) $ Text.Dot.showDot $ do
                Text.Dot.attribute ("rankdir", "LR")
                graphToDot graph

        Resolve (Just ListImmediateDependencies) -> do
            expression <- getExpression
            (`runReaderT` state) $ do
              _ <- infer emptyCxt expression
              graph <- liftIO . readIORef =<< asks _graph
              liftIO $ forM_ (Set.elems (snd $ graph Map.! rootImport ".")) $ \imp ->
                print $ Pretty.pretty $ importHashed imp

        Resolve (Just ListTransitiveDependencies) -> do
          expression <- getExpression
          (`runReaderT` state) $ do
            _ <- infer emptyCxt expression
            cache <- liftIO . readIORef =<< asks _cache
            liftIO $
              mapM_ print
                 .   fmap (   Pretty.pretty
                          .   Dhall.Core.importType
                          .   Dhall.Core.importHashed )
                 .   Map.keys
                 $   cache

        Resolve Nothing -> do
          expression <- getExpression
          state <- pure $ state {_importOptions = OldResolve}
          (t, _) <- runReaderT (infer emptyCxt expression) state
          render System.IO.stdout t

        Type -> do
          expression <- getExpression
          state     <- pure $ state {_importOptions = ImportsDisabled}
          (_, ty)    <- runReaderT (infer emptyCxt expression) state
          render System.IO.stdout (quote NEmpty ty)

        -- Repl -> do
        --     Dhall.Repl.repl characterSet explain standardVersion

        -- Diff {..} -> do
        --     expression1 <- Dhall.inputExpr expr1

        --     expression2 <- Dhall.inputExpr expr2

        --     let diff = Dhall.Diff.diffNormalized expression1 expression2

        --     renderDoc System.IO.stdout diff

        Format {..} -> do
            Dhall.Format.format (Dhall.Format.Format {..})

        Freeze {..} -> do
          let importOptions = if all_ then FreezeAll else FreezeRemote
          state <- pure $ state {_importOptions = importOptions}

          case inplace of
            Just file -> do
              text <- Data.Text.IO.readFile file

              (header, expr) <- throws (exprAndHeaderFromText file text)
              (t, _) <- runReaderT (infer emptyCxt expr) state

              let doc    = Pretty.pretty header <> Pretty.pretty t
              let stream = Pretty.layoutSmart layoutOpts doc

              System.IO.withFile file System.IO.WriteMode $ \handle -> do
                  Pretty.renderIO handle (annToAnsiStyle <$> stream)
                  Data.Text.IO.hPutStrLn handle ""

            Nothing -> do
              expr   <- getExpression
              (t, _) <- runReaderT (infer emptyCxt expr) state
              render System.IO.stdout t

        Normalize{..} -> do
            expression <- getExpression
            state     <- pure $ state {_importOptions = ImportsDisabled}
            (t, _) <- runReaderT (infer emptyCxt expression) state
            let normalForm = (if alpha then alphaNormalize else id) (nfEmpty t)
            render System.IO.stdout normalForm

        Hash -> do
            exp <- getExpression
            exp <- fst <$> runReaderT (infer emptyCxt exp) state
            let nf = alphaNormalize (nfEmpty exp)
            Data.Text.IO.putStrLn (hashExpressionToCode standardVersion nf)

        Lint {..} -> do
            case inplace of
                Just file -> do
                    text <- Data.Text.IO.readFile file
                    (header, expression) <- throws (exprAndHeaderFromText file text)
                    let lintedExpression = Dhall.Lint.lint expression

                    let doc =   Pretty.pretty header
                            <>  Dhall.Pretty.prettyCharacterSet characterSet lintedExpression

                    System.IO.withFile file System.IO.WriteMode (\h -> renderDoc h doc)

                Nothing -> do
                    text <- Data.Text.IO.getContents
                    (header, expression) <- throws (exprAndHeaderFromText "(stdin)" text)

                    let lintedExpression = Dhall.Lint.lint expression

                    let doc = Pretty.pretty header
                           <> Dhall.Pretty.prettyCharacterSet characterSet lintedExpression
                    renderDoc System.IO.stdout doc

        Encode {..} -> do
          expression <- getExpression
          let term = Dhall.Binary.encode expression
          let bytes = Codec.Serialise.serialise term
          if json
            then do
              let decoder = Codec.CBOR.JSON.decodeValue False
              (_, value) <- throws (Codec.CBOR.Read.deserialiseFromBytes decoder bytes)
              let jsonBytes = Data.Aeson.Encode.Pretty.encodePretty value
              Data.ByteString.Lazy.Char8.putStrLn jsonBytes
            else do
              Data.ByteString.Lazy.putStr bytes

        Decode {..} -> do
            bytes <- Data.ByteString.Lazy.getContents
            term <- do
              if json
                then do
                  value <- case Data.Aeson.eitherDecode' bytes of
                      Left  string -> fail string
                      Right value  -> return value

                  let encoding = Codec.CBOR.JSON.encodeValue value

                  let cborBytes = Codec.CBOR.Write.toLazyByteString encoding
                  throws (Codec.Serialise.deserialiseOrFail cborBytes)
                else do
                  throws (Codec.Serialise.deserialiseOrFail bytes)

            expression <- throws (Dhall.Binary.decodeExpression term)
            let doc = Dhall.Pretty.prettyCharacterSet characterSet expression
            renderDoc System.IO.stdout doc

        _ -> do
          putStrLn "unsupported option"

-- | Entry point for the @dhall@ executable
main :: IO ()
main = do
    options <- Options.Applicative.execParser parserInfoOptions
    command options
