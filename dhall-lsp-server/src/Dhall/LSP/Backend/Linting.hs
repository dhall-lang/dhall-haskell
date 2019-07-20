module Dhall.LSP.Backend.Linting
  ( Suggestion(..)
  , suggest
  , Dhall.lint
  )
where

import Dhall.Parser (Src)
import Dhall.Core (Expr(..), Binding(..), Var(..), subExpressions, freeIn, Import)
import qualified Dhall.Lint as Dhall

import Dhall.LSP.Backend.Diagnostics

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..), tails, toList)
import Control.Lens (universeOf)

data Suggestion = Suggestion {
    range :: Range,
    suggestion :: Text
    }

-- Diagnose nested let blocks.
diagLetInLet :: Expr Src a -> [Suggestion]
diagLetInLet (Note _ (Let _ (Note src (Let _ _)))) =
  [Suggestion (rangeFromDhall src) "Superfluous 'in' before nested let binding"]
diagLetInLet _ = []

-- Given a (noted) let block compute all unused variables in the block.
unusedBindings :: Eq a => Expr s a -> [Text]
unusedBindings (Note _ (Let bindings d)) = concatMap
  (\case
    Binding var _ _ : [] | not (V var 0 `freeIn` d) -> [var]
    Binding var _ _ : (b : bs) | not (V var 0 `freeIn` Let (b :| bs) d) -> [var]
    _ -> [])
  (toList $ tails bindings)
unusedBindings _ = []

-- Diagnose unused let bindings.
diagUnusedBinding :: Eq a => Expr Src a -> [Suggestion]
diagUnusedBinding e@(Note src (Let _ _)) = map
  (\var ->
    Suggestion (rangeFromDhall src) ("Unused let binding '" <> var <> "'"))
  (unusedBindings e)
diagUnusedBinding _ = []

-- | Given an dhall expression suggest all the possible improvements that would
--   be made by the linter.
suggest :: Expr Src Import -> [Suggestion]
suggest expr = concat [ diagLetInLet e ++ diagUnusedBinding e
                      | e <- universeOf subExpressions expr ]
