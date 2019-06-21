module Dhall.LSP.Backend.Typing (annotateLet, exprAt, srcAt, typeAt) where

import Dhall.Context (Context, insert, empty)
import Dhall.Core (Expr(..), Binding(..), Const(..), subExpressions, normalize, shift, subst, Var(..))
import Dhall.TypeCheck (typeWithA, X(..), TypeError(..))
import Dhall.Parser (Src(..))

import Data.List.NonEmpty (NonEmpty (..))
import Control.Lens (toListOf)
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Applicative ((<|>))
import Data.Bifunctor (first)

import Dhall.LSP.Backend.Parsing (getLetInner, getLetAnnot)
import Dhall.LSP.Backend.Diagnostics (Position, positionFromMegaparsec, offsetToPosition)

import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty
import Dhall.Pretty (CharacterSet(..), prettyCharacterSet)

-- | Find the type of the subexpression at the given position. Assumes that the
--   input expression is well-typed.
typeAt :: Position -> Expr Src X -> Either String (Expr Src X)
typeAt pos expr = do
  expr' <- case splitLets expr of
             Just e -> return e
             Nothing -> Left "The impossible happened: failed to split let\
                              \ blocks when preprocessing for typeAt'."
  first show $ typeAt' pos empty expr'

typeAt' :: Position -> Context (Expr Src X) -> Expr Src X -> Either (TypeError Src X) (Expr Src X)
-- the input only contains singleton lets
typeAt' pos ctx (Let (Binding x _ a :| []) e@(Note src _)) | pos `inside` src = do
  _A <- typeWithA absurd ctx a
  t <- fmap normalize (typeWithA absurd ctx _A)
  case t of
    Const Type -> do  -- we don't have types depending on values
      let ctx' = fmap (shift 1 (V x 0)) (insert x (normalize _A) ctx)
      _B <- typeAt' pos ctx' e
      return (shift (-1) (V x 0) _B)
    _ -> do  -- but we do have types depending on types
      let a' = shift 1 (V x 0) (normalize a)
      typeAt' pos ctx (shift (-1) (V x 0) (subst (V x 0) a' e))

typeAt' pos ctx (Lam x _A b@(Note src _)) | pos `inside` src = do
  let _A' = Dhall.Core.normalize _A
      ctx' = fmap (shift 1 (V x 0)) (insert x _A' ctx)
  typeAt' pos ctx' b

typeAt' pos ctx (Pi x _A  _B@(Note src _)) | pos `inside` src = do
  let _A' = Dhall.Core.normalize _A
      ctx' = fmap (shift 1 (V x 0)) (insert x _A' ctx)
  typeAt' pos ctx' _B

-- peel of a single Note constructor
typeAt' pos ctx (Note _ expr) = typeAt' pos ctx expr

-- catch-all
typeAt' pos ctx expr = do
  let subExprs = toListOf subExpressions expr
  case [ (src, e) | (Note src e) <- subExprs, pos `inside` src ] of
    [] -> typeWithA absurd ctx expr  -- return type of whole subexpression
    ((src, e):_) -> typeAt' pos ctx (Note src e)  -- continue with leaf-expression



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
srcAt pos expr = do Note src _ <- exprAt pos expr
                    return src


-- | Given a well-typed expression and a position find the let binder at that
--   position (if there is one) and return a textual update to the source code
--   that inserts the type annotation (or replaces the existing one). If
--   something goes wrong returns a textual error message.
annotateLet :: Position -> Expr Src X -> Either String (Src, Text)
annotateLet pos expr = do
  expr' <- case splitLets expr of
             Just e -> return e
             Nothing -> Left "The impossible happened: failed to split let\
                              \ blocks when preprocessing for annotateLet'."
  annotateLet' pos empty expr'

annotateLet' :: Position -> Context (Expr Src X) -> Expr Src X -> Either String (Src, Text)
-- the input only contains singleton lets
annotateLet' pos ctx (Note src e@(Let (Binding _ _ a :| []) _))
  | not $ any (pos `inside`) [ src' | Note src' _ <- toListOf subExpressions e ]
  = do _A <- first show $ typeWithA absurd ctx a
       srcAnnot <- case getLetAnnot src of
                     Just x -> return x
                     Nothing -> Left "The impossible happened: failed\
                                     \ to re-parse a Let expression."
       return (srcAnnot, ": " <> printExpr _A <> " ")

-- binders, see typeAt'
annotateLet' pos ctx (Let (Binding x _ a :| []) e@(Note src _)) | pos `inside` src = do
  _A <- first show $ typeWithA absurd ctx a
  t <- first show $ fmap normalize (typeWithA absurd ctx _A)
  case t of
    Const Type -> do  -- we don't have types depending on values
      let ctx' = fmap (shift 1 (V x 0)) (insert x (normalize _A) ctx)
      annotateLet' pos ctx' e
    _ -> do  -- but we do have types depending on types
      let a' = shift 1 (V x 0) (normalize a)
      annotateLet' pos ctx (shift (-1) (V x 0) (subst (V x 0) a' e))

annotateLet' pos ctx (Lam x _A b@(Note src _)) | pos `inside` src = do
  let _A' = Dhall.Core.normalize _A
      ctx' = fmap (shift 1 (V x 0)) (insert x _A' ctx)
  annotateLet' pos ctx' b

annotateLet' pos ctx (Pi x _A _B@(Note src _)) | pos `inside` src = do
  let _A' = Dhall.Core.normalize _A
      ctx' = fmap (shift 1 (V x 0)) (insert x _A' ctx)
  annotateLet' pos ctx' _B

-- we need to unfold Notes to make progress
annotateLet' pos ctx (Note _ expr) = do
  annotateLet' pos ctx expr

-- catch-all
annotateLet' pos ctx expr = do
  let subExprs = toListOf subExpressions expr
  case [ Note src e | (Note src e) <- subExprs, pos `inside` src ] of
    (e:[]) -> annotateLet' pos ctx e
    _ -> Left "You weren't pointing at a let binder!"


printExpr :: Pretty.Pretty b => Expr a b -> Text
printExpr expr = Pretty.renderStrict $ Pretty.layoutCompact (Pretty.unAnnotate (prettyCharacterSet Unicode expr))


-- Split all multilets into single lets in an expression
splitLets :: Expr Src a -> Maybe (Expr Src a)
splitLets (Note src (Let (b :| (b' : bs)) e)) = do
  src' <- getLetInner src
  splitLets (Note src (Let (b :| []) (Note src' (Let (b' :| bs) e))))
splitLets expr = subExpressions splitLets expr


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
