module Dhall.LSP.Backend.Linting
  ( Suggestion(..)
  , suggest
  , Dhall.lint
  )
where

import Dhall.Parser (Src)
import Dhall.Core ( Expr(..), Binding(..), MultiLet(..), Var(..), Import
                  , subExpressions, freeIn, multiLet, wrapInLets)
import qualified Dhall.Lint as Dhall

import Dhall.LSP.Backend.Diagnostics

import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Text (Text)
import Control.Lens (universeOf)

data Suggestion = Suggestion {
    range :: Range,
    suggestion :: Text
    }

-- Diagnose nested let-blocks.
--
-- Pattern matching on a 'Let' wrapped in a 'Note' prevents us from repeating
-- the search beginning at different @let@s in the same let-block – only
-- the outermost 'Let' of a let-block is wrapped in a 'Note'.
diagLetInLet :: Expr Src a -> Maybe Suggestion
diagLetInLet (Note _ (Let x mA a b)) = case multiLet x mA a b of
    MultiLet _ (Note src (Let _ _ _ _)) ->
      Just (Suggestion (rangeFromDhall src) "Superfluous 'in' before nested let binding")
    _ -> Nothing
diagLetInLet _ = Nothing

-- Given a let-block compute all unused variables in the block.
unusedBindings :: Eq a => MultiLet s a -> [Text]
unusedBindings (MultiLet bindings d) =
  let go (Binding var _ _ : bs) | not (V var 0 `freeIn` wrapInLets bs d) = [var]
      go _ = []
  in foldMap go (NonEmpty.tails bindings)

-- Diagnose unused let bindings.
diagUnusedBindings :: Eq a => Expr Src a -> [Suggestion]
diagUnusedBindings (Note src (Let x mA a e)) = map
  (\var ->
    Suggestion (rangeFromDhall src) ("Unused let binding '" <> var <> "'"))
  (unusedBindings (multiLet x mA a e))
diagUnusedBindings _ = []

-- | Given an dhall expression suggest all the possible improvements that would
--   be made by the linter.
suggest :: Expr Src Import -> [Suggestion]
suggest expr = concat [ maybeToList (diagLetInLet e) ++ diagUnusedBindings e
                      | e <- universeOf subExpressions expr ]
