{-# LANGUAGE NamedFieldPuns #-}

module Dhall.LSP.Backend.Linting
  ( Suggestion(..)
  , suggest
  , Lint.lint
  )
where

import Control.Lens (universeOf)
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Text (Text)
import Dhall.LSP.Backend.Diagnostics
import Dhall.Parser (Src)
import Dhall.Core (Expr(..), Binding(..), MultiLet(..), Import)

import qualified Data.Maybe         as Maybe
import qualified Dhall.Core         as Core
import qualified Dhall.Lint         as Lint
import qualified Data.List.NonEmpty as NonEmpty

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
diagLetInLet (Note _ (Let b e)) = case Core.multiLet b e of
    MultiLet _ (Note src (Let {})) ->
      Just (Suggestion (rangeFromDhall src) "Superfluous 'in' before nested let binding")
    _ -> Nothing
diagLetInLet _ = Nothing

-- Given a let-block compute all unused variables in the block.
unusedBindings :: Eq a => MultiLet s a -> [ (Text, Maybe s) ]
unusedBindings (MultiLet bindings d) =
  let go bs@(Binding { variable = var, value } : _)
          | Just _ <- Lint.removeUnusedBindings (Core.wrapInLets bs d) =
              [ (var, maybeSrc) ]
        where
          maybeSrc = case value of
              Note src _ -> Just src
              _          -> Nothing
      go _ = []
  in foldMap go (NonEmpty.tails bindings)

-- Diagnose unused let bindings.
diagUnusedBindings :: Eq a => Expr Src a -> [Suggestion]
diagUnusedBindings (Note src (Let b e)) =
    map adapt (unusedBindings (Core.multiLet b e))
  where
    adapt (var, maybeSrc) =
        Suggestion (rangeFromDhall finalSrc) ("Unused let binding '" <> var <> "'")
      where
        finalSrc = Maybe.fromMaybe src maybeSrc
diagUnusedBindings _ = []

-- | Given an dhall expression suggest all the possible improvements that would
--   be made by the linter.
suggest :: Expr Src Import -> [Suggestion]
suggest expr = concat [ maybeToList (diagLetInLet e) ++ diagUnusedBindings e
                      | e <- universeOf Core.subExpressions expr ]
