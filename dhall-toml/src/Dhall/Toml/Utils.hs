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
import Dhall.Parser (Src)

import qualified Data.Text.IO    as Text.IO
import qualified Dhall.Core      as Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.TypeCheck

-- | Read the file fileName and return the normalized Dhall AST
fileToDhall :: String -> IO (Core.Expr Src Void)
fileToDhall fileName = do
    text <- Text.IO.readFile fileName
    textToDhall fileName text

-- | Read from STDIN and return the normalized Dhall AST. The file name
--   is set to "(input)"
inputToDhall :: IO (Core.Expr Src Void)
inputToDhall = do
    text <- Text.IO.getContents
    textToDhall "(input)" text

-- | Parse text and return the normalized Dhall AST. A file name is required
--   by the parser for generating error messages.
textToDhall :: String -> Text -> IO (Core.Expr Src Void)
textToDhall fileName text = do
    parsedExpression <-
        Core.throws (Dhall.Parser.exprFromText fileName text)
    resolvedExpression <- Dhall.Import.load parsedExpression
    _ <- Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)
    return resolvedExpression

