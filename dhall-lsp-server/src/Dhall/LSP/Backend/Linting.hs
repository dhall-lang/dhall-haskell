{-# LANGUAGE NamedFieldPuns #-}

module Dhall.LSP.Backend.Linting
  ( Suggestion(..)
  , suggest
  , unusedBindingEdits
  , letChains
  , deleteLetRange
  , letKeyword
  , exprStart
  , exprEnd
  , slice
  , Lint.lint
  )
where

import Control.Lens                  (universeOf)
import Data.Maybe                    (catMaybes, isJust, maybeToList)
import Data.Text                     (Text)
import Dhall.Core
    ( Binding (..)
    , Expr (..)
    , Import
    , MultiLet (..)
    )
import Dhall.LSP.Backend.Diagnostics
import Dhall.Parser                  (Src (..))
import Dhall.Scope                   (makeSrcForLabel)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe         as Maybe
import qualified Data.Text          as Text
import qualified Dhall.Core         as Core
import qualified Dhall.Lint         as Lint

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
unusedBindings :: Eq a => MultiLet Src a -> [(Text, Maybe Src)]
unusedBindings (MultiLet bindings d) =
  let go bs@(Binding { variable = var, bindingSrc0, bindingSrc1, value } : _)
          | Just _ <- Lint.removeUnusedBindings (Core.wrapInLets bs d) =
              [ (var, nameSpan) ]
        where
          -- The name sits between the whitespace after @let@ and the
          -- whitespace before @:@ or @=@.  The value's span starts at `{` in
          -- `let x = { … }`, which is not the name.
          nameSpan = case (bindingSrc0, bindingSrc1) of
              ( Just Src { srcEnd = nameStart }
                , Just Src { srcStart = nameEnd }
                ) ->
                  Just (makeSrcForLabel nameStart nameEnd var)
              _ -> case value of
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

-- | An unused binding the cursor can sit in, and the span that deletes it.
--
--   The first range runs from the @let@ keyword through the end of the value,
--   so a cursor on the name or on the value matches.  The second range is
--   what to delete.  A binding that is not the last one is deleted up to the
--   next @let@.  The last binding of several keeps the following @in@.  The
--   only binding of a block is deleted through the @in@, leaving the body.
unusedBindingEdits :: Text -> Expr Src Import -> [(Range, Range)]
unusedBindingEdits txt expr =
    [ edit
    | Note _ (Let binding body) <- universeOf Core.subExpressions expr
    , edit <- blockEdits txt (Core.multiLet binding body)
    ]

-- | Source let-blocks: the outermost @let@ of a multi-let is wrapped in a
--   'Note', so inner bindings of the same block are not listed again.
letChains :: Expr Src Import -> [([Binding Src Import], Expr Src Import)]
letChains expr =
    [ (NonEmpty.toList bindings, body)
    | Note _ (Let binding body0) <- universeOf Core.subExpressions expr
    , let MultiLet bindings body = Core.multiLet binding body0
    ]

-- | Delete the binding at @index@: through the next @let@, or through @in@
--   when it is the only binding, or up to @in@ when later bindings remain.
deleteLetRange
    :: Text
    -> [Binding Src Import]
    -> Expr Src Import
    -> Int
    -> Maybe Range
deleteLetRange txt listed body index = do
    binding <- listed `atIndex` index
    letStart <- letKeyword binding
    valueEnd <- exprEnd (value binding)
    deleteEnd <- case drop (index + 1) listed of
        next : _ ->
            letKeyword next
        []
            | index == 0 ->
                exprStart body
            | otherwise -> do
                bodyStart <- exprStart body
                inKeyword txt valueEnd bodyStart
    return (Range letStart deleteEnd)

atIndex :: [a] -> Int -> Maybe a
atIndex xs i =
    case drop i xs of
        x : _ -> Just x
        [] -> Nothing

blockEdits :: Text -> MultiLet Src Import -> [(Range, Range)]
blockEdits txt (MultiLet bindings body) =
    catMaybes (zipWith one [0 ..] listed)
  where
    listed = NonEmpty.toList bindings

    one index binding
        | not (unusedHere index) = Nothing
        | otherwise = do
            letStart <- letKeyword binding
            valueEnd <- exprEnd (value binding)
            deleteRange <- deleteLetRange txt listed body index
            return (Range letStart valueEnd, deleteRange)

    unusedHere index =
        case drop index listed of
            suffix@(_ : _) ->
                isJust (Lint.removeUnusedBindings (Core.wrapInLets suffix body))
            [] ->
                False

letKeyword :: Binding Src a -> Maybe Position
letKeyword Binding { bindingSrc0 = Just src0 } =
    let (line, col) = positionFromMegaparsec (srcStart src0)
    in if col >= 3 then Just (line, col - 3) else Nothing
letKeyword _ = Nothing

exprStart :: Expr Src a -> Maybe Position
exprStart (Note src _) = Just left
  where
    Range left _ = rangeFromDhall src
exprStart _ = Nothing

exprEnd :: Expr Src a -> Maybe Position
exprEnd (Note src _) = Just right
  where
    Range _ right = rangeFromDhall src
exprEnd _ = Nothing

inKeyword :: Text -> Position -> Position -> Maybe Position
inKeyword txt valueEnd bodyStart
    | Text.isPrefixOf "in" rest =
        Just (offsetToPosition txt (positionToOffset txt valueEnd + Text.length before))
    | otherwise =
        Nothing
  where
    gap = slice txt valueEnd bodyStart
    (before, rest) = Text.breakOn "in" gap

slice :: Text -> Position -> Position -> Text
slice txt startPos endPos =
    Text.take (max 0 (endOff - startOff)) (Text.drop startOff txt)
  where
    startOff = positionToOffset txt startPos
    endOff = positionToOffset txt endPos
