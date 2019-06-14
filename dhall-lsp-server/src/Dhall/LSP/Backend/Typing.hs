module Dhall.LSP.Backend.Typing (typeAt, srcAt) where

import Dhall.Context (Context, insert, empty)
import Dhall.Core (Expr(..), Binding(..), subExpressions, normalize, shift, Var(..))
import Dhall.TypeCheck (typeWithA, X(..), TypeError(..))
import Dhall.Parser (Src(..))

import Data.List.NonEmpty (NonEmpty (..))
import Control.Lens (toListOf)
import qualified Data.Text as Text

import Dhall.LSP.Backend.Diagnostics (Position, positionFromMegaparsec, offsetToPosition, positionToOffset)

-- | Find the type of the subexpression at the given position.
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
-- we don't check the optional annotation for consistency
typeAt' pos ctx (Let (Binding x _ a :| []) (Note src e)) | pos `inside` src = do
  _A <- typeWithA absurd ctx a
  let ctx' = fmap (shift 1 (V x 0)) (insert x _A ctx)
  typeAt' pos ctx' e

typeAt' pos ctx (Lam x _A (Note src b)) | pos `inside` src = do
  _A' <- do _ <- typeWithA absurd ctx _A
            return (Dhall.Core.normalize _A)
  let ctx' = fmap (shift 1 (V x 0)) (insert x _A' ctx)
  typeAt' pos ctx' b

-- we don't check that the function type is itself well-formed
typeAt' pos ctx (Pi x _A  (Note src _B)) | pos `inside` src = do
  _A' <- do _ <- typeWithA absurd ctx _A
            return (Dhall.Core.normalize _A)
  let ctx' = fmap (shift 1 (V x 0)) (insert x _A' ctx)
  typeAt' pos ctx' _B

-- need to catch Notes since the catch-all would remove two layers at once
typeAt' pos ctx (Note _ expr) = typeAt' pos ctx expr

-- catch-all
typeAt' pos ctx expr = do
  let subExprs = toListOf subExpressions expr
  case [ e | (Note src e) <- subExprs, pos `inside` src ] of
    [] -> typeWithA absurd ctx expr
    (t:_) -> typeAt' pos ctx t


-- | Find the smallest Src annotation containing the given position.
srcAt :: Position -> Expr Src X -> Maybe Src
srcAt pos (Note src expr) = case srcAt pos expr of
  Just src' -> Just src'
  Nothing   -> Just src
srcAt pos expr =
  let subExprs = toListOf subExpressions expr
  in case [ (src, e) | (Note src e) <- subExprs, pos `inside` src ] of
    [] -> Nothing
    ((src, e) : _) -> case srcAt pos e of
                 Nothing -> Just src
                 Just src' -> Just src'


{-
inside :: Position -> Src -> Bool
inside pos (Src left' right' _) =
  positionFromMegaparsec left' <= pos && pos <= positionFromMegaparsec right'
-}
-- check if range lies completely inside a given subexpression
-- this version takes trailing whitespace into account
-- (c.f. `sanitiseRange` from Backend.Diangostics)
inside :: Position -> Src -> Bool
inside pos (Src left right txt) =
  let (x1,y1) = positionFromMegaparsec left
      (x2,y2) = positionFromMegaparsec right
      off | x1 == x2 = positionToOffset txt (0, y2 - y1)
          | otherwise = positionToOffset txt (x2 - x1, y2)
      (dx2,dy2) = (offsetToPosition txt . Text.length . Text.stripEnd
                   . Text.take off) txt
      (x2',y2') | dx2 == 0 = (x1, y1 + dy2)
                | otherwise = (x1 + dx2, dy2)
  in (x1,y1) <= pos && pos < (x2',y2')
