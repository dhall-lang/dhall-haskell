module Dhall.LSP.Backend.Typing (annotateLet, exprAt, srcAt, typeAt) where

import Dhall.Context (Context, insert, empty)
import Dhall.Core (Expr(..), Binding(..), subExpressions, normalize, shift, Var(..))
import Dhall.TypeCheck (typeWithA, X(..), TypeError(..))
import Dhall.Parser (Src(..))

import Data.List.NonEmpty (NonEmpty (..))
import Control.Lens (toListOf)
import qualified Data.Text as Text
import Control.Applicative ((<|>))
import Data.Functor.Identity (Identity(..))

import Dhall.LSP.Util (rightToMaybe)
import Dhall.LSP.Backend.Parsing (getLetInner)
import Dhall.LSP.Backend.Diagnostics (Position, positionFromMegaparsec, offsetToPosition)

-- | Find the type of the subexpression at the given position. Assumes that the
--   input expression is well-typed.
typeAt :: Position -> Expr Src X -> Maybe (Expr Src X)
typeAt pos expr = rightToMaybe (typeAt' pos empty (splitLets expr))

typeAt' :: Position -> Context (Expr Src X) -> Expr Src X -> Either (TypeError Src X) (Expr Src X)
-- the input only contains singleton lets
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


-- | Find the smallest Note-wrapped expression at the given position.
exprAt :: Position -> Expr Src a -> Maybe (Expr Src a)
exprAt pos e@(Note _ expr) = exprAt pos expr <|> Just e
exprAt pos expr =
  let subExprs = toListOf subExpressions expr
  in case [ (src, e) | (Note src e) <- subExprs, pos `inside` src ] of
    [] -> Nothing
    ((src,e) : _) -> exprAt pos e <|> Just (Note src e)


-- | Find the smallest Src annotation containing the given position.
srcAt :: Position -> Expr Src a -> Maybe Src
srcAt pos expr = do e <- exprAt pos expr
                    case e of
                      Note src _ -> Just src
                      _ -> Nothing


-- assume input to be well-typed; assume only singleton lets
annotateLet :: Position -> Expr Src X -> Maybe (Expr Src X)
annotateLet pos expr = rightToMaybe (annotateLet' pos empty (splitLets expr))

annotateLet' :: Position -> Context (Expr Src X) -> Expr Src X -> Either (TypeError Src X) (Expr Src X)
-- not yet annotated
annotateLet' pos ctx (Let (Binding x Nothing a :| []) (Note src e))
  | not (pos `inside` src) = do
    _A <- typeWithA absurd ctx a
    return (Let (Binding x (Just _A) a :| []) (Note src e))

-- replace existing annotation with normal form
annotateLet' pos ctx (Let (Binding x (Just (Note src _A)) a :| []) (Note src' e))
  | not (pos `inside` src) && not (pos `inside` src') = do
    _A' <- typeWithA absurd ctx a
    return (Let (Binding x (Just _A') a :| []) (Note src' e))

-- binders
annotateLet' pos ctx (Let (Binding x mA a :| []) (Note src e))
  | pos `inside` src = do
    _A <- typeWithA absurd ctx a
    let ctx' = fmap (shift 1 (V x 0)) (insert x _A ctx)
    e' <- annotateLet' pos ctx' e
    return (Let (Binding x mA a :| []) (Note src e'))

annotateLet' pos ctx (Lam x _A (Note src b))
  | pos `inside` src = do
    let _A' = Dhall.Core.normalize _A
        ctx' = fmap (shift 1 (V x 0)) (insert x _A' ctx)
    b' <- annotateLet' pos ctx' b
    return (Lam x _A (Note src b'))

annotateLet' pos ctx (Pi x _A (Note src _B))
  | pos `inside` src = do
    let _A' = Dhall.Core.normalize _A
        ctx' = fmap (shift 1 (V x 0)) (insert x _A' ctx)
    _B' <- annotateLet' pos ctx' _B
    return (Pi x _A (Note src _B'))

-- need to catch Notes since the catch-all would remove two layers at once
annotateLet' pos ctx (Note src expr) = do
  Note src <$> annotateLet' pos ctx expr

-- catch-all
annotateLet' pos ctx expr = subExpressions goInside expr
  where
    goInside (Note src e) | pos `inside` src = Note src <$> annotateLet' pos ctx e
    goInside e = return e


-- Split all multilets into single lets in an expression
splitLets :: Expr Src a -> Expr Src a
splitLets (Note src (Let (b :| (b' : bs)) e)) =
  splitLets (Note src (Let (b :| []) (Note src' (Let (b' :| bs) e))))
  where src' = case getLetInner src of
                 Just x -> x
                 Nothing -> error "The impossible happened: failed\
                                  \ to re-parse a Let expression."
splitLets expr = runIdentity (subExpressions (Identity . splitLets) expr)


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
