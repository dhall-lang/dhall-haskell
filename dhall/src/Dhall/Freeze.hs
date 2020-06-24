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

import Data.Foldable (toList, for_)
import Data.Monoid ((<>))
import Dhall.Pretty (CharacterSet)
import System.Console.ANSI (hSupportsANSI)

import Dhall.Syntax (Expr(..) , Import(..) , ImportHashed(..) , ImportType(..))
import Dhall.Util
    ( Censor
    , CheckFailed(..)
    , Header(..)
    , OutputMode(..)
    , PossiblyTransitiveInput(..)
    , Transitivity(..)
    )
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
    -> Import
    -> IO Import
freezeImport directory import_ = do
    let unprotectedImport =
            import_
                { importHashed =
                    (importHashed import_)
                        { hash = Nothing
                        }
                }

    let status = Dhall.Import.emptyStatus directory

    expression <- State.evalStateT (Dhall.Import.loadWith (Embed unprotectedImport)) status

    case Dhall.TypeCheck.typeOf expression of
        Left  exception -> Exception.throwIO exception
        Right _         -> return ()

    let normalizedExpression = Core.alphaNormalize (Core.normalize expression)

    -- make sure the frozen import is present in the semantic cache
    Dhall.Import.writeExpressionToSemanticCache (Core.denote expression)

    let expressionHash = Dhall.Import.hashExpression normalizedExpression

    let newImportHashed = (importHashed import_) { hash = Just expressionHash }

    let newImport = import_ { importHashed = newImportHashed }

    return newImport

-- | Freeze an import only if the import is a `Remote` import
freezeRemoteImport
    :: FilePath
    -- ^ Current working directory
    -> Import
    -> IO Import
freezeRemoteImport directory import_ = do
    case importType (importHashed import_) of
        Remote {} -> freezeImport directory import_
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
                for_ (toList parsedExpression) $ \import_ -> do
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
                        supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout
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

    let freezeFunction = freezeScope directory

    let cache
            -- This case is necessary because `transformOf` is a bottom-up
            -- rewrite rule.   Without this rule, if you were to transform a
            -- file that already has a cached expression, like this:
            --
            --     someImport sha256:… ? someImport
            --
            -- ... then you would get:
            --
            --       (someImport sha256:… ? someImport)
            --     ? (someImport sha256:… ? someImport)
            --
            -- ... and this rule fixes that by collapsing that back to:
            --
            --       (someImport sha256:… ? someImport)
            (ImportAlt
                (Core.shallowDenote -> ImportAlt
                    (Core.shallowDenote -> Embed
                        Import{ importHashed = ImportHashed{ hash = Just _expectedHash } }
                    )
                    (Core.shallowDenote -> Embed
                        Import{ importHashed = ImportHashed{ hash = Nothing } }
                    )
                )
                import_@(Core.shallowDenote -> ImportAlt
                    (Core.shallowDenote -> Embed
                        Import{ importHashed = ImportHashed{ hash = Just _actualHash } }
                    )
                    (Core.shallowDenote -> Embed
                        Import{ importHashed = ImportHashed{ hash = Nothing } }
                    )
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
                   `freezeFunction` only freezes remote imports by default
                -}
                if frozenImport /= import_
                    then return (ImportAlt (Embed frozenImport) (Embed import_))
                    else return (Embed import_)
        cache
            (Embed import_@(Import { importHashed = ImportHashed { hash = Just _ } })) = do
                -- Regenerate the integrity check, just in case it's wrong
                frozenImport <- freezeFunction import_

                -- `dhall freeze --cache` also works the other way around, adding an
                -- unprotected fallback import to imports that are already
                -- protected
                let thawedImport = import_
                        { importHashed = (importHashed import_)
                            { hash = Nothing
                            }
                        }

                return (ImportAlt (Embed frozenImport) (Embed thawedImport))
        cache expression_ = do
            return expression_

    case intent of
        Secure ->
            traverse freezeFunction expression
        Cache  ->
            Dhall.Optics.transformMOf Core.subExpressions cache expression
