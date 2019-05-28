{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains Dhall's parsing logic

module Dhall.Parser (
    -- * Utilities
      exprFromText
    , exprAndHeaderFromText

    -- * Parsers
    , expr, exprA

    -- * Types
    , Src(..)
    , SourcedException(..)
    , ParseError(..)
    , Parser(..)
    ) where

import Control.Exception (Exception)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Data.Void (Void)
import Dhall.Core
import Prelude hiding (const, pi)

import qualified Data.Text
import qualified Text.Megaparsec

import Dhall.Parser.Combinators
import Dhall.Parser.Token
import Dhall.Parser.Expression

-- | Parser for a top-level Dhall expression
expr :: Parser (Expr Src Import)
expr = exprA (Text.Megaparsec.try import_)

-- | Parser for a top-level Dhall expression. The expression is parameterized
-- over any parseable type, allowing the language to be extended as needed.
exprA :: Parser a -> Parser (Expr Src a)
exprA = completeExpression

-- | A parsing error
data ParseError = ParseError
#if MIN_VERSION_megaparsec(7, 0, 0)
    { unwrap :: Text.Megaparsec.ParseErrorBundle Text Void
#else
    { unwrap :: Text.Megaparsec.ParseError Char Void
#endif
    , input  :: Text
    }

instance Show ParseError where
    show (ParseError {..}) =
#if MIN_VERSION_megaparsec(7, 0, 0)
      "\n\ESC[1;31mError\ESC[0m: Invalid input\n\n" <> Text.Megaparsec.errorBundlePretty unwrap
#else
      "\n\ESC[1;31mError\ESC[0m: Invalid input\n\n" <> Text.Megaparsec.parseErrorPretty unwrap
#endif

instance Exception ParseError

-- | Parse an expression from `Text` containing a Dhall program
exprFromText
  :: String -- ^ User-friendly name describing the input expression,
            --   used in parsing error messages
  -> Text   -- ^ Input expression to parse
  -> Either ParseError (Expr Src Import)
exprFromText delta text = fmap snd (exprAndHeaderFromText delta text)

{-| Like `exprFromText` but also returns the leading comments and whitespace
    (i.e. header) up to the last newline before the code begins

    In other words, if you have a Dhall file of the form:

> -- Comment 1
> {- Comment -} 2

    Then this will preserve @Comment 1@, but not @Comment 2@

    This is used by @dhall-format@ to preserve leading comments and whitespace
-}
exprAndHeaderFromText
    :: String -- ^ User-friendly name describing the input expression,
              --   used in parsing error messages
    -> Text   -- ^ Input expression to parse
    -> Either ParseError (Text, Expr Src Import)
exprAndHeaderFromText delta text = case result of
    Left errInfo   -> Left (ParseError { unwrap = errInfo, input = text })
    Right (txt, r) -> Right (Data.Text.dropWhileEnd (/= '\n') txt, r)
  where
    parser = do
        (bytes, _) <- Text.Megaparsec.match whitespace
        r <- expr
        Text.Megaparsec.eof
        return (bytes, r)

    result = Text.Megaparsec.parse (unParser parser) delta text
