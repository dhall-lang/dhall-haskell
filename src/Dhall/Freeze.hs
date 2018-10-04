{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the implementation of the @dhall freeze@ subcommand

module Dhall.Freeze
    ( -- * Freeze
      freeze
    , hashImport
    ) where

import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.Text
import Dhall.Binary (ProtocolVersion(..))
import Dhall.Core (Expr(..), Import(..), ImportHashed(..))
import Dhall.Import (hashExpression, protocolVersion)
import Dhall.Parser (exprAndHeaderFromText, Src)
import Dhall.Pretty (annToAnsiStyle, layoutOpts)
import Lens.Family (set)
import System.Console.ANSI (hSupportsANSI)

import qualified Control.Exception
import qualified Control.Monad.Trans.State.Strict          as State
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Data.Text.IO
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.TypeCheck
import qualified System.FilePath
import qualified System.IO

-- | Retrieve an `Import` and update the hash to match the latest contents
hashImport
    :: FilePath
    -- ^ Current working directory
    -> ProtocolVersion
    -> Import
    -> IO Import
hashImport directory _protocolVersion import_ = do
    let status = set protocolVersion _protocolVersion (Dhall.Import.emptyStatus directory)

    expression <- State.evalStateT (Dhall.Import.loadWith (Embed import_)) status

    case Dhall.TypeCheck.typeOf expression of
        Left  exception -> Control.Exception.throwIO exception
        Right _         -> return ()

    let normalizedExpression =
            Dhall.Core.alphaNormalize (Dhall.Core.normalize expression)

    let expressionHash =
            Just (Dhall.Import.hashExpression _protocolVersion normalizedExpression)

    let newImportHashed = (importHashed import_) { hash = expressionHash }

    let newImport = import_ { importHashed = newImportHashed }

    State.evalStateT (Dhall.Import.exprToImport newImport normalizedExpression) status

    return newImport

parseExpr :: String -> Text -> IO (Text, Expr Src Import)
parseExpr src txt =
    case exprAndHeaderFromText src txt of
        Left err -> Control.Exception.throwIO err
        Right x  -> return x

writeExpr :: Maybe FilePath -> (Text, Expr s Import) -> IO ()
writeExpr inplace (header, expr) = do
    let doc = Pretty.pretty header <> Pretty.pretty expr
    let stream = Pretty.layoutSmart layoutOpts doc

    case inplace of
        Just f ->
            System.IO.withFile f System.IO.WriteMode (\h ->
                Pretty.renderIO h (annToAnsiStyle <$> stream))

        Nothing -> do
            supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout
            if supportsANSI 
               then 
                 Pretty.renderIO System.IO.stdout (annToAnsiStyle <$> Pretty.layoutSmart layoutOpts doc)
               else
                 Pretty.renderIO System.IO.stdout (Pretty.layoutSmart layoutOpts (Pretty.unAnnotate doc)) 

-- | Implementation of the @dhall freeze@ subcommand
freeze
    :: Maybe FilePath
    -- ^ Modify file in-place if present, otherwise read from @stdin@ and write
    --   to @stdout@
    -> ProtocolVersion
    -> IO ()
freeze inplace _protocolVersion = do
    (text, directory) <- case inplace of
        Nothing -> do
            text <- Data.Text.IO.getContents

            return (text, ".")

        Just file -> do
            text <- Data.Text.IO.readFile file

            return (text, System.FilePath.takeDirectory file)

    (header, parsedExpression) <- parseExpr srcInfo text
    frozenExpression <- traverse (hashImport directory _protocolVersion) parsedExpression
    writeExpr inplace (header, frozenExpression)
        where
            srcInfo = fromMaybe "(stdin)" inplace
