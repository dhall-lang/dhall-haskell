{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the implementation of the @dhall freeze@ subcommand

module Dhall.Freeze
    ( -- * Freeze
      freeze
    , freezeImport
    , freezeRemoteImport
    ) where

import Control.Exception (SomeException)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.Text
import Dhall.Binary (StandardVersion(..))
import Dhall.Core (Expr(..), Import(..), ImportHashed(..), ImportType(..))
import Dhall.Import (standardVersion)
import Dhall.Parser (exprAndHeaderFromText, Src)
import Dhall.Pretty (CharacterSet, annToAnsiStyle, layoutOpts, prettyCharacterSet)
import Dhall.TypeCheck (X)
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
freezeImport
    :: FilePath
    -- ^ Current working directory
    -> StandardVersion
    -> Import
    -> IO Import
freezeImport directory _standardVersion import_ = do
    let unprotectedImport =
            import_
                { importHashed =
                    (importHashed import_)
                        { hash = Nothing
                        }
                }

    let status =
            set standardVersion
                _standardVersion
                (Dhall.Import.emptyStatus directory)

    let download =
            State.evalStateT (Dhall.Import.loadWith (Embed import_)) status

    -- Try again without the semantic integrity check if decoding fails
    let handler :: SomeException -> IO (Expr Src X)
        handler _ = do
            State.evalStateT (Dhall.Import.loadWith (Embed unprotectedImport)) status

    expression <- Control.Exception.handle handler download

    case Dhall.TypeCheck.typeOf expression of
        Left  exception -> Control.Exception.throwIO exception
        Right _         -> return ()

    let normalizedExpression =
            Dhall.Core.alphaNormalize (Dhall.Core.normalize expression)

    let expressionHash =
            Just (Dhall.Import.hashExpression _standardVersion normalizedExpression)

    let newImportHashed = (importHashed import_) { hash = expressionHash }

    let newImport = import_ { importHashed = newImportHashed }

    State.evalStateT (Dhall.Import.exprToImport newImport normalizedExpression) status

    return newImport

-- | Freeze an import only if the import is a `Remote` import
freezeRemoteImport
    :: FilePath
    -- ^ Current working directory
    -> StandardVersion
    -> Import
    -> IO Import
freezeRemoteImport directory _standardVersion import_ = do
    case importType (importHashed import_) of
        Remote {} -> freezeImport directory _standardVersion import_
        _         -> return import_

parseExpr :: String -> Text -> IO (Text, Expr Src Import)
parseExpr src txt =
    case exprAndHeaderFromText src txt of
        Left err -> Control.Exception.throwIO err
        Right x  -> return x

writeExpr :: Maybe FilePath -> (Text, Expr s Import) -> CharacterSet -> IO ()
writeExpr inplace (header, expr) characterSet = do
    let doc =  Pretty.pretty header
            <> Dhall.Pretty.prettyCharacterSet characterSet expr

    let unAnnotated = Pretty.layoutSmart layoutOpts (Pretty.unAnnotate doc)

    case inplace of
        Just f ->
            System.IO.withFile f System.IO.WriteMode (\handle -> do
                Pretty.renderIO handle unAnnotated
                Data.Text.IO.hPutStrLn handle "" )

        Nothing -> do
            supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout
            if supportsANSI
               then
                 Pretty.renderIO System.IO.stdout (annToAnsiStyle <$> Pretty.layoutSmart layoutOpts doc)
               else
                 Pretty.renderIO System.IO.stdout unAnnotated

-- | Implementation of the @dhall freeze@ subcommand
freeze
    :: Maybe FilePath
    -- ^ Modify file in-place if present, otherwise read from @stdin@ and write
    --   to @stdout@
    -> Bool
    -- ^ If `True` then freeze all imports, otherwise freeze only remote imports
    -> CharacterSet
    -> StandardVersion
    -> IO ()
freeze inplace everything characterSet _standardVersion = do
    (text, directory) <- case inplace of
        Nothing -> do
            text <- Data.Text.IO.getContents

            return (text, ".")

        Just file -> do
            text <- Data.Text.IO.readFile file

            return (text, System.FilePath.takeDirectory file)

    (header, parsedExpression) <- parseExpr srcInfo text

    let freezeFunction = if everything then freezeImport else freezeRemoteImport

    frozenExpression <- traverse (freezeFunction directory _standardVersion) parsedExpression
    writeExpr inplace (header, frozenExpression) characterSet
        where
            srcInfo = fromMaybe "(stdin)" inplace
