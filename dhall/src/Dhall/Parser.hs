{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module contains Dhall's parsing logic

module Dhall.Parser (
    -- * Utilities
      exprFromText
    , exprAndHeaderFromText
    , censor
    , createHeader

    -- * Parsers
    , expr, exprA

    -- * Types
    , Header(..)
    , Src(..)
    , SourcedException(..)
    , ParseError(..)
    , Parser(..)
    ) where

import Control.Exception (Exception)
import Data.Text         (Text)
import Data.Void         (Void)
import Dhall.Src         (Src (..))
import Dhall.Syntax
import Text.Megaparsec   (ParseErrorBundle (..), PosState (..))

import qualified Data.Text       as Text
import qualified Dhall.Core      as Core
import qualified Text.Megaparsec

import Dhall.Parser.Combinators
import Dhall.Parser.Expression
import Dhall.Parser.Token       hiding (text)

-- | Parser for a top-level Dhall expression
expr :: Parser (Expr Src Import)
expr = exprA (Text.Megaparsec.try import_)

-- | Parser for a top-level Dhall expression. The expression is parameterized
-- over any parseable type, allowing the language to be extended as needed.
exprA :: Parser a -> Parser (Expr Src a)
exprA = completeExpression
{-# DEPRECATED exprA "Support for parsing custom imports will be dropped in a future release" #-}

-- | A parsing error
data ParseError = ParseError {
      unwrap :: Text.Megaparsec.ParseErrorBundle Text Void
    , input  :: Text
    }

{-| Replace the source code with spaces when rendering error messages

    This utility is used to implement the @--censor@ flag
-}
censor :: ParseError -> ParseError
censor parseError =
    parseError
        { unwrap =
            (unwrap parseError)
                { bundlePosState =
                    (bundlePosState (unwrap parseError))
                        { pstateInput =
                            Core.censorText
                                (pstateInput (bundlePosState (unwrap parseError)))
                        }
                }
        }

instance Show ParseError where
    show (ParseError {..}) =
      "\n\ESC[1;31mError\ESC[0m: Invalid input\n\n" <> Text.Megaparsec.errorBundlePretty unwrap

instance Exception ParseError

-- | Parse an expression from `Text.Text` containing a Dhall program
exprFromText
  :: String -- ^ User-friendly name describing the input expression,
            --   used in parsing error messages
  -> Text   -- ^ Input expression to parse
  -> Either ParseError (Expr Src Import)
exprFromText delta text = fmap snd (exprAndHeaderFromText delta text)

-- | A header corresponds to the leading comment at the top of a Dhall file.
--
-- The header includes comment characters but is stripped of leading spaces and
-- trailing newlines
newtype Header = Header Text deriving Show

-- | Create a header with stripped leading spaces and trailing newlines
createHeader :: Text -> Header
createHeader text = Header (prefix <> newSuffix)
  where
    isWhitespace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'

    prefix = Text.dropAround isWhitespace text

    newSuffix
        | Text.null prefix = ""
        | otherwise        = "\n"

-- | Like `exprFromText` but also returns the leading comments and whitespace
-- (i.e. header) up to the last newline before the code begins
--
-- In other words, if you have a Dhall file of the form:
--
-- > -- Comment 1
-- > {- Comment -} 2
--
-- Then this will preserve @Comment 1@, but not @Comment 2@
--
-- This is used by @dhall-format@ to preserve leading comments and whitespace
exprAndHeaderFromText
    :: String -- ^ User-friendly name describing the input expression,
              --   used in parsing error messages
    -> Text   -- ^ Input expression to parse
    -> Either ParseError (Header, Expr Src Import)
exprAndHeaderFromText delta text = case result of
    Left errInfo   -> Left (ParseError { unwrap = errInfo, input = text })
    Right (txt, r) -> Right (createHeader txt, r)
  where
    parser = do
        (bytes, _) <- Text.Megaparsec.match whitespace
        r <- expr
        Text.Megaparsec.eof
        return (bytes, r)

    result = Text.Megaparsec.parse (unParser parser) delta text
