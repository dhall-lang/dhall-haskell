{-| This library provides utilities for converting Dhall source code into a
    normalized AST
-}

module Dhall.Toml.Utils
    ( fileToDhall
    , inputToDhall
    , textToDhall
    ) where

import Data.Text    (Text)
import Data.Void    (Void)
import Dhall.Import (RemoteImportMode, SemanticCacheMode(..))
import Dhall.Parser (Src)

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text.IO                     as Text.IO
import qualified Dhall.Core                       as Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified System.FilePath                  as FilePath

-- | Read the file fileName and return the normalized Dhall AST
fileToDhall :: RemoteImportMode -> String -> IO (Core.Expr Src Void)
fileToDhall remoteImportMode fileName = do
    text <- Text.IO.readFile fileName
    textToDhall remoteImportMode fileName text

-- | Read from STDIN and return the normalized Dhall AST. The file name
--   is set to "(input)"
inputToDhall :: RemoteImportMode -> IO (Core.Expr Src Void)
inputToDhall remoteImportMode = do
    text <- Text.IO.getContents
    textToDhall remoteImportMode "(input)" text

-- | Parse text and return the normalized Dhall AST. A file name is required
--   by the parser for generating error messages.
textToDhall :: RemoteImportMode -> String -> Text -> IO (Core.Expr Src Void)
textToDhall remoteImportMode fileName text = do
    parsedExpression <- do
        Core.throws (Dhall.Parser.exprFromText fileName text)

    let directory = FilePath.takeDirectory fileName

    let status =
            (Dhall.Import.emptyStatus directory)
                { Dhall.Import._semanticCacheMode = UseSemanticCache
                , Dhall.Import._remoteImportMode = remoteImportMode
                }

    (resolvedExpression, status') <-
        State.runStateT (Dhall.Import.loadWith parsedExpression) status

    inferred <-
        State.evalStateT
            (Dhall.Import.typecheckLoaded parsedExpression resolvedExpression)
            status'

    _ <- Core.throws inferred

    return resolvedExpression

