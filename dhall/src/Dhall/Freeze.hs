{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | This module contains the implementation of the @dhall freeze@ subcommand

module Dhall.Freeze
    ( -- * Freeze
      freeze
    , freezeExpression
    , freezeImport
    , freezeRemoteImport

      -- * Types
    , Scope(..)
    , Intent(..)
    ) where

import Data.Foldable (for_)
import Data.Monoid ((<>))
import Dhall.Pretty (CharacterSet)
import Dhall.Syntax
    (Expr(..), Hash(..), Import(..), ImportHashed(..), ImportType(..))
import Dhall.Util
    ( Censor
    , CheckFailed(..)
    , Header(..)
    , OutputMode(..)
    , PossiblyTransitiveInput(..)
    , Transitivity(..)
    )
import System.Console.ANSI (hSupportsANSI)

import qualified Control.Exception                         as Exception
import qualified Control.Monad.Trans.State.Strict          as State
import qualified Data.Text.IO                              as Text.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty.Text
import qualified Dhall.Core                                as Core
import qualified Dhall.Import
import qualified Dhall.Optics
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck
import qualified Dhall.Util                                as Util
import qualified System.AtomicWrite.Writer.LazyText        as AtomicWrite.LazyText
import qualified System.FilePath
import qualified System.IO

-- | Retrieve an `Import` and update the hash to match the latest contents
freezeImport
    :: FilePath
    -- ^ Current working directory
    -> Intent
    -> Import
    -> IO Import
freezeImport directory intent import_ = do
    let unprotectedImport =
            import_
                { importHashed =
                    (importHashed import_)
                        { hash = NoHash
                        }
                }

    let status = Dhall.Import.emptyStatus directory

    expression <- State.evalStateT (Dhall.Import.loadWith (Embed unprotectedImport)) status

    case Dhall.TypeCheck.typeOf expression of
        Left  exception -> Exception.throwIO exception
        Right _         -> return ()

    let normalizedExpression =
            case intent of
                Dhall.Freeze.Secure ->
                    Core.alphaNormalize (Core.normalize expression)
                Dhall.Freeze.Cache ->
                    Core.denote expression

    -- make sure the frozen import is present in the semantic cache
    Dhall.Import.writeExpressionToSemanticCache (Core.denote expression)

    let expressionHash = Dhall.Import.hashExpression normalizedExpression

    let hash = case intent of
            Dhall.Freeze.Secure -> Dhall.Syntax.Secure expressionHash
            Dhall.Freeze.Cache  -> Dhall.Syntax.Cache expressionHash

    let newImportHashed =
            (importHashed import_) { hash }

    let newImport = import_ { importHashed = newImportHashed }

    return newImport

-- | Freeze an import only if the import is a `Remote` import
freezeRemoteImport
    :: FilePath
    -- ^ Current working directory
    -> Intent
    -> Import
    -> IO Import
freezeRemoteImport directory intent import_ = do
    case importType (importHashed import_) of
        Remote {} -> freezeImport directory intent import_
        _         -> return import_

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
    :: OutputMode
    -> PossiblyTransitiveInput
    -> Scope
    -> Intent
    -> CharacterSet
    -> Censor
    -> IO ()
freeze outputMode input0 scope intent characterSet censor = go input0
  where
    go input = do
        let directory = case input of
                NonTransitiveStandardInput ->
                    "."
                PossiblyTransitiveInputFile file _ ->
                    System.FilePath.takeDirectory file

        let status = Dhall.Import.emptyStatus directory

        (originalText, transitivity) <- case input of
            PossiblyTransitiveInputFile file transitivity -> do
                text <- Text.IO.readFile file

                return (text, transitivity)

            NonTransitiveStandardInput -> do
                text <- Text.IO.getContents

                return (text, NonTransitive)

        (Header header, parsedExpression) <- Util.getExpressionAndHeaderFromStdinText censor originalText

        case transitivity of
            Transitive -> do
                for_ parsedExpression $ \import_ -> do
                    maybeFilepath <- Dhall.Import.dependencyToFile status import_

                    for_ maybeFilepath $ \filepath -> do
                        go (PossiblyTransitiveInputFile filepath Transitive)

            NonTransitive ->
                return ()

        frozenExpression <- freezeExpression directory scope intent parsedExpression

        let doc =  Pretty.pretty header
                <> Dhall.Pretty.prettyCharacterSet characterSet frozenExpression
                <> "\n"

        let stream = Dhall.Pretty.layout doc

        let modifiedText = Pretty.Text.renderStrict stream

        case outputMode of
            Write -> do
                let unAnnotated = Pretty.unAnnotateS stream

                case input of
                    PossiblyTransitiveInputFile file _ ->
                        if originalText == modifiedText
                            then return ()
                            else
                                AtomicWrite.LazyText.atomicWriteFile
                                    file
                                    (Pretty.Text.renderLazy unAnnotated)

                    NonTransitiveStandardInput -> do
                        supportsANSI <- hSupportsANSI System.IO.stdout
                        if supportsANSI
                           then
                             Pretty.renderIO System.IO.stdout (Dhall.Pretty.annToAnsiStyle <$> stream)
                           else
                             Pretty.renderIO System.IO.stdout unAnnotated

            Check -> do
                if originalText == modifiedText
                    then return ()
                    else do
                        let command = "freeze"

                        let modified = "frozen"

                        Exception.throwIO CheckFailed{..}

{-| Slightly more pure version of the `freeze` function

    This still requires `IO` to freeze the import, but now the input and output
    expression are passed in explicitly
-}
freezeExpression
    :: FilePath
    -- ^ Starting directory
    -> Scope
    -> Intent
    -> Expr s Import
    -> IO (Expr s Import)
freezeExpression directory scope intent expression = do
    let freezeScope =
            case scope of
                AllImports        -> freezeImport
                OnlyRemoteImports -> freezeRemoteImport

    let freezeFunction = freezeScope directory intent

    let cache
            (Embed import_@(Import { importHashed = ImportHashed { hash = _ } })) = do
                fmap Embed (freezeFunction import_)
        cache expression_ = do
            return expression_

    case intent of
        Dhall.Freeze.Secure ->
            traverse freezeFunction expression
        Dhall.Freeze.Cache  ->
            Dhall.Optics.transformMOf Core.subExpressions cache expression
