module Dhall.LSP.Backend.Linting
  ( suggest
  , Suggestion(..)
  , lintAndFormatDocument
  )
where

import Dhall.Parser (Src, ParseError, exprFromText, exprAndHeaderFromText)
import Dhall.Core (Expr(..), Binding(..), Var(..), subExpressions, freeIn)
import Dhall.Lint (lint)

import Dhall.LSP.Backend.Formatting
import Dhall.LSP.Backend.Diagnostics

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..), tails, toList)
import Control.Lens (universeOf)

data Suggestion = Suggestion {
    range :: Range,
    suggestion :: Text
    }

-- Diagnose nested Let blocks.
diagLetInLet :: Expr Src a -> [Suggestion]
diagLetInLet (Note _ (Let _ (Note src (Let _ _)))) =
  [Suggestion (rangeFromDhall src) "Superfluous 'in' before nested let binding"]
diagLetInLet _ = []

-- Given a (noted) Let block compute all unused variables in the block.
unusedBindings :: Eq a => Expr s a -> [Text]
unusedBindings (Note _ (Let bindings d)) = concatMap
  (\case
    Binding var _ _ : [] | not (V var 0 `freeIn` d) -> [var]
    Binding var _ _ : (b : bs) | not (V var 0 `freeIn` Let (b :| bs) d) -> [var]
    _ -> [])
  (toList $ tails bindings)
unusedBindings _ = []

-- Diagnose unused Let bindings.
diagUnusedBinding :: Eq a => Expr Src a -> [Suggestion]
diagUnusedBinding e@(Note src (Let _ _)) = map
  (\var ->
    Suggestion (rangeFromDhall src) ("Unused let binding '" <> var <> "'"))
  (unusedBindings e)
diagUnusedBinding _ = []

-- | Given an dhall expression suggest all the possible improvements that would
--   be made by the linter.
suggest :: Text -> [Suggestion]
suggest txt = case exprFromText "" txt of
  Right expr -> concat [ diagLetInLet e ++ diagUnusedBinding e
                       | e <- universeOf subExpressions expr ]
  _ -> []

lintAndFormatDocument :: Text -> Either ParseError Text
lintAndFormatDocument text = do
    (header, expr) <- exprAndHeaderFromText "" text
    let expr' = lint expr
    pure (formatExpr header expr')
