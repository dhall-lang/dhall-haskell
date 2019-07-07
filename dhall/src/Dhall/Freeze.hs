{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module contains the implementation of the @dhall freeze@ subcommand

module Dhall.Freeze
    ( -- * Freeze
      freeze
    , freezeImport
    , freezeRemoteImport

      -- * Types
    , Scope(..)
    , Intent(..)
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
import qualified Dhall.Optics
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

-- | Specifies which imports to freeze
data Scope
    = OnlyRemoteImports
    -- ^ Freeze only remote imports (i.e. URLs)
    | AllImports
    -- ^ Freeze all imports (including paths and environment variables)

-- | Specifies why we are adding semantic integrity checks
data Intent
    = Secure
    -- ^ Protect imports with an integrity check without a fallback so that
    --   import resolution fails if the import changes
    | Cache
    -- ^ Protect imports with an integrity check and also add a fallback import
    --   import without an integrity check.  This is useful if you only want to
    --   cache imports when possible but still gracefully degrade to resolving
    --   them if the semantic integrity check has changed.

-- | Implementation of the @dhall freeze@ subcommand
freeze
    :: Maybe FilePath
    -- ^ Modify file in-place if present, otherwise read from @stdin@ and write
    --   to @stdout@
    -> Scope
    -> Intent
    -> CharacterSet
    -> StandardVersion
    -> IO ()
freeze inplace scope intent characterSet _standardVersion = do
    (text, directory) <- case inplace of
        Nothing -> do
            text <- Data.Text.IO.getContents

            return (text, ".")

        Just file -> do
            text <- Data.Text.IO.readFile file

            return (text, System.FilePath.takeDirectory file)

    (header, parsedExpression) <- Dhall.Core.throws (exprAndHeaderFromText srcInfo text)

    let freezeScope =
            case scope of
                AllImports        -> freezeImport
                OnlyRemoteImports -> freezeRemoteImport

    let freezeFunction = freezeScope directory _standardVersion

    let cache
            (ImportAlt
                (Embed
                    (Import { importHashed = ImportHashed { hash = Just _expectedHash } })
                )
                import_@(ImportAlt
                    (Embed
                        (Import { importHashed = ImportHashed { hash = Just _actualHash } })
                    )
                    _
                )
            ) = do
                {- Here we could actually compare the `_expectedHash` and
                   `_actualHash` to see if they differ, but we choose not to do
                   so and instead automatically accept the `_actualHash`.  This
                   is done for the same reason that the `freeze*` functions
                   ignore hash mismatches: the user intention when using `dhall
                   freeze` is to update the hash, which they expect to possibly
                   change.
                -}
                return import_
        cache
            (Embed import_@(Import { importHashed = ImportHashed { hash = Nothing } })) = do
                frozenImport <- freezeFunction import_

                {- The two imports can be the same if the import is local and
                   `freezeFunction` only freezes remote imports
                -}
                if frozenImport /= import_
                    then return (ImportAlt (Embed frozenImport) (Embed import_))
                    else return (Embed import_)
        cache expression = do
            return expression

    let rewrite expression =
            case intent of
                Secure ->
                    traverse freezeFunction expression
                Cache  ->
                    Dhall.Optics.transformMOf
                        Dhall.Core.subExpressions
                        cache
                        (Dhall.Core.denote expression)

    frozenExpression <- rewrite parsedExpression

    writeExpr inplace (header, frozenExpression) characterSet
        where
            srcInfo = fromMaybe "(stdin)" inplace
