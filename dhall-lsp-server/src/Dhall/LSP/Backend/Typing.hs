module Dhall.LSP.Backend.Typing (typeAt, srcAt) where

import Dhall.Context (Context, insert, empty)
import Dhall.Core (Expr(..), Binding(..), subExpressions, normalize, shift, Var(..))
import Dhall.TypeCheck (typeWithA, X(..), TypeError(..))
import Dhall.Parser (Src(..))

import Data.List.NonEmpty (NonEmpty (..))
import Control.Lens (toListOf)
import qualified Data.Text as Text
import Control.Applicative ((<|>))

import Dhall.LSP.Backend.Diagnostics (Position, positionFromMegaparsec, offsetToPosition)

-- | Find the type of the subexpression at the given position. Assumes that the
--   input expression is well-typed.
typeAt :: Position -> Expr Src X -> Maybe (Expr Src X)
typeAt pos expr = case typeAt' pos empty expr of
  Right typ -> Just typ
  _        -> Nothing

typeAt' :: Position -> Context (Expr Src X) -> Expr Src X -> Either (TypeError Src X) (Expr Src X)
-- unfold lets to make things simpler
-- need to match on outer Note to recover the range
typeAt' pos ctx (Note src (Let (b :| (b' : bs)) e)) =
  typeAt' pos ctx (Note src (Let (b :| []) (Note src (Let (b' :| bs) e))))

-- only handle singleton lets explicitly
typeAt' pos ctx (Let (Binding x _ a :| []) (Note src e)) | pos `inside` src = do
  _A <- typeWithA absurd ctx a
  let ctx' = fmap (shift 1 (V x 0)) (insert x _A ctx)
  typeAt' pos ctx' e

typeAt' pos ctx (Lam x _A (Note src b)) | pos `inside` src = do
  let _A' = Dhall.Core.normalize _A
      ctx' = fmap (shift 1 (V x 0)) (insert x _A' ctx)
  typeAt' pos ctx' b

typeAt' pos ctx (Pi x _A  (Note src _B)) | pos `inside` src = do
  let _A' = Dhall.Core.normalize _A
      ctx' = fmap (shift 1 (V x 0)) (insert x _A' ctx)
  typeAt' pos ctx' _B

-- need to catch Notes since the catch-all would remove two layers at once
typeAt' pos ctx (Note _ expr) = typeAt' pos ctx expr

-- catch-all
typeAt' pos ctx expr = do
  let subExprs = toListOf subExpressions expr
  case [ e | (Note src e) <- subExprs, pos `inside` src ] of
    [] -> typeWithA absurd ctx expr  -- return type of whole subexpression
    (t:_) -> typeAt' pos ctx t  -- continue with leaf-expression


-- | Find the smallest Src annotation containing the given position.
srcAt :: Position -> Expr Src X -> Maybe Src
srcAt pos (Note src expr) = srcAt pos expr <|> Just src
srcAt pos expr =
  let subExprs = toListOf subExpressions expr
  in case [ (src, e) | (Note src e) <- subExprs, pos `inside` src ] of
    [] -> Nothing
    ((src, e) : _) -> srcAt pos e <|> Just src


-- Check if range lies completely inside a given subexpression.
-- This version takes trailing whitespace into account
-- (c.f. `sanitiseRange` from Backend.Diangostics).
inside :: Position -> Src -> Bool
inside pos (Src left _right txt) =
  let (x1,y1) = positionFromMegaparsec left
      txt' = Text.stripEnd txt
      (dx2,dy2) = (offsetToPosition txt . Text.length) txt'
      (x2,y2) | dx2 == 0 = (x1, y1 + dy2)
              | otherwise = (x1 + dx2, dy2)
  in (x1,y1) <= pos && pos < (x2,y2)
