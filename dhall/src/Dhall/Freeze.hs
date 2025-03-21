{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | This module contains the implementation of the @dhall freeze@ subcommand

module Dhall.Freeze
    ( -- * Freeze
      freeze
    , freezeExpression
    , freezeImport
    , freezeRemoteImport

      -- * Freeze with custom evaluation settings
    , freezeWithSettings
    , freezeExpressionWithSettings
    , freezeImportWithSettings
    , freezeRemoteImportWithSettings

      -- * Types
    , Scope(..)
    , Intent(..)

      -- * Deprecated functions
    , freezeWithManager
    , freezeExpressionWithManager
    , freezeImportWithManager
    , freezeRemoteImportWithManager
    ) where

import Data.Foldable       (for_)
import Data.List.NonEmpty  (NonEmpty)
import Data.Maybe          (fromMaybe)
import Dhall               (EvaluateSettings)
import Dhall.Pretty        (CharacterSet, detectCharacterSet)
import Dhall.Syntax
    ( Expr (..)
    , Import (..)
    , ImportHashed (..)
    , ImportType (..)
    )
import Dhall.Util
    ( Censor
    , CheckFailed (..)
    , Header (..)
    , Input (..)
    , OutputMode (..)
    , Transitivity (..)
    , handleMultipleChecksFailed
    )
import Lens.Micro          (set, transformMOf, transformOf)
import Lens.Micro.Extras   (view)
import System.Console.ANSI (hSupportsANSI)

import qualified Control.Exception                  as Exception
import qualified Control.Monad.Trans.State.Strict   as State
import qualified Data.Text.IO                       as Text.IO
import qualified Dhall
import qualified Dhall.Core                         as Core
import qualified Dhall.Import
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck
import qualified Dhall.Util                         as Util
import qualified Prettyprinter                      as Pretty
import qualified Prettyprinter.Render.Terminal      as Pretty
import qualified Prettyprinter.Render.Text          as Pretty.Text
import qualified System.AtomicWrite.Writer.LazyText as AtomicWrite.LazyText
import qualified System.FilePath
import qualified System.IO

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

-- | Retrieve an `Import` and update the hash to match the latest contents
freezeImport
    :: FilePath
    -- ^ Current working directory
    -> Import
    -> IO Import
freezeImport = freezeImportWithSettings Dhall.defaultEvaluateSettings

-- | See 'freezeImport'.
freezeImportWithManager
    :: IO Dhall.Import.Manager
    -> FilePath
    -> Import
    -> IO Import
freezeImportWithManager newManager = freezeImportWithSettings (set Dhall.newManager newManager Dhall.defaultEvaluateSettings)
{-# DEPRECATED freezeImportWithManager "Use freezeImportWithSettings directly" #-}

-- | Freeze an import only if the import is a `Remote` import
freezeRemoteImport
    :: FilePath
    -- ^ Current working directory
    -> Import
    -> IO Import
freezeRemoteImport = freezeRemoteImportWithSettings Dhall.defaultEvaluateSettings

-- | See 'freezeRemoteImport'.
freezeRemoteImportWithManager
    :: IO Dhall.Import.Manager
    -> FilePath
    -> Import
    -> IO Import
freezeRemoteImportWithManager newManager = freezeRemoteImportWithSettings (set Dhall.newManager newManager Dhall.defaultEvaluateSettings)
{-# DEPRECATED freezeRemoteImportWithManager "Use freezeRemoteImportWithSettings directly" #-}

-- | Implementation of the @dhall freeze@ subcommand
freeze
    :: OutputMode
    -> Transitivity
    -> NonEmpty Input
    -> Scope
    -> Intent
    -> Maybe CharacterSet
    -> Censor
    -> IO ()
freeze = freezeWithSettings Dhall.defaultEvaluateSettings

-- | See 'freeze'.
freezeWithManager
    :: IO Dhall.Import.Manager
    -> OutputMode
    -> Transitivity
    -> NonEmpty Input
    -> Scope
    -> Intent
    -> Maybe CharacterSet
    -> Censor
    -> IO ()
freezeWithManager newManager = freezeWithSettings (set Dhall.newManager newManager Dhall.defaultEvaluateSettings)
{-# DEPRECATED freezeWithManager "Use freezeWithSettings directly" #-}

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
freezeExpression = freezeExpressionWithSettings Dhall.defaultEvaluateSettings

-- | See 'freezeExpression'.
freezeExpressionWithManager
    :: IO Dhall.Import.Manager
    -> FilePath
    -> Scope
    -> Intent
    -> Expr s Import
    -> IO (Expr s Import)
freezeExpressionWithManager newManager = freezeExpressionWithSettings (set Dhall.newManager newManager Dhall.defaultEvaluateSettings)
{-# DEPRECATED freezeExpressionWithManager "Use freezeExpressionWithSettings directly" #-}

--------------------------------------------------------------------------------
-- Versions that take EvaluateSettings
--------------------------------------------------------------------------------

-- | See 'freezeImport'.
freezeImportWithSettings
    :: EvaluateSettings
    -> FilePath
    -> Import
    -> IO Import
freezeImportWithSettings settings directory import_ = do
    let unprotectedImport =
            import_
                { importHashed =
                    (importHashed import_)
                        { hash = Nothing
                        }
                }

    let status = Dhall.Import.emptyStatusWithManager (view Dhall.newManager settings) directory

    expression <- State.evalStateT (Dhall.Import.loadWith (Embed unprotectedImport)) status

    case Dhall.TypeCheck.typeWith (view Dhall.startingContext settings) expression of
        Left  exception -> Exception.throwIO exception
        Right _         -> return ()

    let normalizedExpression = Core.alphaNormalize (Core.normalizeWith (view Dhall.normalizer settings) expression)

    -- make sure the frozen import is present in the semantic cache
    Dhall.Import.writeExpressionToSemanticCache (Core.denote expression)

    let expressionHash = Dhall.Import.hashExpression normalizedExpression

    let newImportHashed = (importHashed import_) { hash = Just expressionHash }

    let newImport = import_ { importHashed = newImportHashed }

    return newImport

-- | See 'freezeRemoteImport'.
freezeRemoteImportWithSettings
    :: EvaluateSettings
    -> FilePath
    -> Import
    -> IO Import
freezeRemoteImportWithSettings settings directory import_ =
    case importType (importHashed import_) of
        Remote {} -> freezeImportWithSettings settings directory import_
        _         -> return import_

-- | See 'freezeRemoteImport'.
freezeNonMissingImportWithSettings
    :: EvaluateSettings
    -> FilePath
    -> Import
    -> IO Import
freezeNonMissingImportWithSettings settings directory import_ =
    case importType (importHashed import_) of
        Missing -> return import_
        _ -> freezeImportWithSettings settings directory import_

-- | See 'freeze'.
freezeWithSettings
    :: EvaluateSettings
    -> OutputMode
    -> Transitivity
    -> NonEmpty Input
    -> Scope
    -> Intent
    -> Maybe CharacterSet
    -> Censor
    -> IO ()
freezeWithSettings settings outputMode transitivity0 inputs scope intent chosenCharacterSet censor =
    handleMultipleChecksFailed "freeze" "frozen" go inputs
  where
    go input = do
        let directory = case input of
                StandardInput ->
                    "."
                InputFile file ->
                    System.FilePath.takeDirectory file

        let status = Dhall.Import.emptyStatusWithManager (view Dhall.newManager settings) directory

        (inputName, originalText, transitivity) <- case input of
            InputFile file -> do
                text <- Text.IO.readFile file

                return (file, text, transitivity0)

            StandardInput -> do
                text <- Text.IO.getContents

                return ("(input)", text, NonTransitive)

        (Header header, parsedExpression) <- Util.getExpressionAndHeaderFromStdinText censor inputName originalText

        let characterSet = fromMaybe (detectCharacterSet parsedExpression) chosenCharacterSet

        case transitivity of
            Transitive ->
                for_ parsedExpression $ \import_ -> do
                    maybeFilepath <- Dhall.Import.dependencyToFile status import_

                    for_ maybeFilepath $ \filepath ->
                        go (InputFile filepath)

            NonTransitive ->
                return ()

        frozenExpression <- freezeExpressionWithSettings settings directory scope intent parsedExpression

        let doc =  Pretty.pretty header
                <> Dhall.Pretty.prettyCharacterSet characterSet frozenExpression
                <> "\n"

        let stream = Dhall.Pretty.layout doc

        let modifiedText = Pretty.Text.renderStrict stream

        case outputMode of
            Write -> do
                let unAnnotated = Pretty.unAnnotateS stream

                case input of
                    InputFile file ->
                        if originalText == modifiedText
                            then return ()
                            else
                                AtomicWrite.LazyText.atomicWriteFile
                                    file
                                    (Pretty.Text.renderLazy unAnnotated)

                    StandardInput -> do
                        supportsANSI <- System.Console.ANSI.hSupportsANSI System.IO.stdout
                        if supportsANSI
                           then
                             Pretty.renderIO System.IO.stdout (Dhall.Pretty.annToAnsiStyle <$> stream)
                           else
                             Pretty.renderIO System.IO.stdout unAnnotated

                return (Right ())

            Check ->
                return $
                    if originalText == modifiedText
                        then Right ()
                        else Left CheckFailed{..}

-- | See 'freezeExpression'.
freezeExpressionWithSettings
    :: EvaluateSettings
    -> FilePath
    -> Scope
    -> Intent
    -> Expr s Import
    -> IO (Expr s Import)
freezeExpressionWithSettings settings directory scope intent expression = do
    let freezeScope =
            case scope of
                AllImports        -> freezeNonMissingImportWithSettings
                OnlyRemoteImports -> freezeRemoteImportWithSettings

    let freezeFunction = freezeScope settings directory

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
            ) =
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

                let frozenMissing = toMissing frozenImport

                {- The two imports can be the same if the import is local and
                   `freezeFunction` only freezes remote imports by default
                -}
                if frozenImport /= import_
                    then return (ImportAlt (Embed frozenMissing) (Embed import_))
                    else return (Embed import_)
        cache
            (Embed import_@(Import { importHashed = ImportHashed { hash = Just _ } })) = do
                -- Regenerate the integrity check, just in case it's wrong
                frozenImport <- freezeFunction import_

                let frozenMissing = toMissing frozenImport

                -- `dhall freeze --cache` also works the other way around, adding an
                -- unprotected fallback import to imports that are already
                -- protected
                let thawedImport = import_
                        { importHashed = (importHashed import_)
                            { hash = Nothing
                            }
                        }

                return (ImportAlt (Embed frozenMissing) (Embed thawedImport))
        cache expression_ =
            return expression_

    let uncache
            (ImportAlt
                (Core.shallowDenote -> Embed
                    Import{ importHashed = ImportHashed { hash = Just expectedHash, importType = Missing } }
                )
                (Core.shallowDenote -> Embed
                    import_@Import{ importHashed = ImportHashed{ hash = Nothing } }
                )
            ) = Embed
                (import_
                    { importHashed = (importHashed import_)
                        { hash = Just expectedHash
                        }
                    }
                )
        uncache expression_ = expression_

    let simplify (ImportAlt (Core.shallowDenote -> Embed import1) (Core.shallowDenote -> Embed import2))
            | import1 == import2 = Embed import1
        simplify expression_ = expression_

    transformOf Core.subExpressions simplify <$> case intent of
        Secure ->
            traverse freezeFunction (transformOf Core.subExpressions uncache expression)
        Cache  ->
            transformMOf Core.subExpressions cache expression

-- https://github.com/dhall-lang/dhall-haskell/issues/2347
toMissing :: Import -> Import
toMissing import_ =
    import_ { importHashed = (importHashed import_) { importType = Missing } }
