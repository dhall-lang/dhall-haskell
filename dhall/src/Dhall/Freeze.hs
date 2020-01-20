{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

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

import Data.Bifunctor (first)
import Data.Monoid ((<>))
import Data.Text
import Dhall.Parser (Src)
import Dhall.Pretty (CharacterSet)
import Dhall.Syntax (Expr(..), Import(..), ImportHashed(..), ImportType(..))
import Dhall.Util
    ( Censor
    , CheckFailed(..)
    , Header(..)
    , Input(..)
    , OutputMode(..)
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
import qualified Dhall.Parser                              as Parser
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

writeExpr :: Input -> (Text, Expr Src Import) -> CharacterSet -> IO ()
writeExpr input (header, expr) characterSet = do
    let doc =  Pretty.pretty header
            <> Dhall.Pretty.prettyCharacterSet characterSet expr
            <> "\n"

    let stream = Dhall.Pretty.layout doc

    let unAnnotated = Pretty.unAnnotateS stream

    case input of
        InputFile file ->
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
    -> Input
    -> Scope
    -> Intent
    -> CharacterSet
    -> Censor
    -> IO ()
freeze outputMode input scope intent characterSet censor = do
    let directory = case input of
            StandardInput  -> "."
            InputFile file -> System.FilePath.takeDirectory file

    let freezeScope =
            case scope of
                AllImports        -> freezeImport
                OnlyRemoteImports -> freezeRemoteImport

    let freezeFunction = freezeScope directory

    let cache
            (ImportAlt
                (Core.shallowDenote -> Embed
                    (Import { importHashed = ImportHashed { hash = Just _expectedHash } })
                )
                import_@(Core.shallowDenote -> ImportAlt
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
                        Core.subExpressions
                        cache
                        expression

    case outputMode of
        Write -> do
            (Header header, parsedExpression) <- do
                Util.getExpressionAndHeader censor input

            frozenExpression <- rewrite parsedExpression

            writeExpr input (header, frozenExpression) characterSet

        Check -> do
            originalText <- case input of
                InputFile file -> Text.IO.readFile file
                StandardInput  -> Text.IO.getContents

            let name = case input of
                    InputFile file -> file
                    StandardInput  -> "(stdin)"

            (Header header, parsedExpression) <- do
                Core.throws (first Parser.censor (Parser.exprAndHeaderFromText name originalText))

            frozenExpression <- rewrite parsedExpression

            let doc =  Pretty.pretty header
                    <> Dhall.Pretty.prettyCharacterSet characterSet frozenExpression
                    <> "\n"

            let stream = Dhall.Pretty.layout doc

            let modifiedText = Pretty.Text.renderStrict stream

            if originalText == modifiedText
                then return ()
                else do
                    let command = "freeze"

                    let modified = "frozen"

                    Exception.throwIO CheckFailed{..}
