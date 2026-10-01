module Dhall.LSP.Backend.Typing (annotateLet, exprAt, letTypes, normalizedAt, scopedNormalize, srcAt, typeAt) where

import Dhall.Core
    ( Binding (..)
    , Const (..)
    , Expr (..)
    , FunctionBinding (..)
    , subExpressions
    )
import Dhall.Parser    (Src (..))
import Dhall.TypeCheck
    ( TypeError (..)
    , TypingContext
    , emptyTypingContext
    , extendBinder
    , extendLet
    , normalizeWithContext
    , normalizeWithContextBounded
    , typeWithContext
    )

import Control.Applicative ((<|>))
import Control.Lens        (toListOf)
import Data.Bifunctor      (first)
import Data.Text           (Text)
import Data.Void           (Void)

import qualified Data.Text as Text

import Dhall.LSP.Backend.Dhall       (WellTyped, fromWellTyped)
import Dhall.LSP.Backend.Diagnostics (Position, Range (..), rangeFromDhall)
import Dhall.LSP.Backend.Parsing
    ( getForallIdentifier
    , getLamIdentifier
    , getLetAnnot
    , getLetIdentifier
    , getLetInner
    )

-- | Find the type of the subexpression at the given position. Assumes that the
--   input expression is well-typed. Also returns the Src descriptor containing
--   that subexpression if possible.
typeAt :: Position -> WellTyped -> Either String (Maybe Src, Expr Src Void)
typeAt pos expr = do
  expr' <- case splitMultiLetSrc (fromWellTyped expr) of
             Just e -> return e
             Nothing -> Left "The impossible happened: failed to split let\
                              \ blocks when preprocessing for typeAt'."
  first show $ typeAt' pos emptyTypingContext expr'

-- The walk extends a 'TypingContext' as it enters a binder.  It does not
-- substitute or normalize the bound values.
typeAt' :: Position -> TypingContext Src -> Expr Src Void -> Either (TypeError Src Void) (Maybe Src, Expr Src Void)
-- the user hovered over the bound name in a let expression
typeAt' pos ctx (Note src (Let (Binding { value = a }) _)) | pos `inside` getLetIdentifier src = do
  typ <- typeWithContext ctx a
  return (Just $ getLetIdentifier src, typ)

-- "..." in a lambda expression
typeAt' pos _ctx (Note src (Lam _ FunctionBinding { functionBindingAnnotation = _A} _))
  | Just src' <- getLamIdentifier src
  , pos `inside` src' =
  return (Just src', _A)

-- "..." in a forall expression
typeAt' pos _ctx (Note src (Pi _ _ _A _)) | Just src' <- getForallIdentifier src
                                        , pos `inside` src' =
  return (Just src', _A)

typeAt' pos ctx (Note src (Let (Binding { variable = x, annotation = ann, value = a }) e))
  | coversAnn pos ann = typeAt' pos ctx (annotationExpr ann)
  | covers pos a = typeAt' pos ctx a
  | pos `inside` src = do
      ctx' <- extendLet x a ctx
      typeAt' pos ctx' e

typeAt' pos ctx (Note src (Lam _ FunctionBinding { functionBindingVariable = x, functionBindingAnnotation = _A} b))
  | covers pos _A = typeAt' pos ctx _A
  | pos `inside` src = do
      ctx' <- extendBinder x _A ctx
      typeAt' pos ctx' b

typeAt' pos ctx (Note src (Pi _ x _A _B))
  | covers pos _A = typeAt' pos ctx _A
  | pos `inside` src = do
      ctx' <- extendBinder x _A ctx
      typeAt' pos ctx' _B

-- peel off a single Note constructor
typeAt' pos ctx (Note _ expr) = typeAt' pos ctx expr

-- catch-all
typeAt' pos ctx expr = do
  let subExprs = toListOf subExpressions expr
  case [ (src, e) | (Note src e) <- subExprs, pos `inside` src ] of
    [] -> do typ <- typeWithContext ctx expr
             return (Nothing, typ)
    ((src, e):_) -> typeAt' pos ctx (Note src e)


-- | Find the smallest Note-wrapped expression at the given position.
exprAt :: Position -> Expr Src a -> Maybe (Expr Src a)
exprAt pos e = do e' <- splitMultiLetSrc e
                  exprAt' pos e'

exprAt' :: Position -> Expr Src a -> Maybe (Expr Src a)
exprAt' pos e@(Note _ expr) = exprAt pos expr <|> Just e
exprAt' pos expr =
  let subExprs = toListOf subExpressions expr
  in case [ (src, e) | (Note src e) <- subExprs, pos `inside` src ] of
    [] -> Nothing
    ((src,e) : _) -> exprAt' pos e <|> Just (Note src e)


-- | Find the smallest Src annotation containing the given position.
srcAt :: Position -> Expr Src a -> Maybe Src
srcAt pos expr = do Note src _ <- exprAt pos expr
                    return src


-- | Given a well-typed expression and a position find the let binder at that
--   position (if there is one) and return the type annotation to be inserted
--   (potentially replacing the existing one). If something goes wrong returns a
--   textual error message.
annotateLet :: Position -> WellTyped -> Either String (Src, Expr Src Void)
annotateLet pos expr = do
  expr' <- case splitMultiLetSrc (fromWellTyped expr) of
             Just e -> return e
             Nothing -> Left "The impossible happened: failed to split let\
                              \ blocks when preprocessing for annotateLet'."
  annotateLet' pos emptyTypingContext expr'


annotateLet' :: Position -> TypingContext Src -> Expr Src Void
             -> Either String (Src, Expr Src Void)
-- the input only contains singleton lets
annotateLet' pos ctx (Note src e@(Let (Binding { value = a }) _))
  | not $ any (pos `inside`) [ src' | Note src' _ <- toListOf subExpressions e ]
  = do _A <- first show $ typeWithContext ctx a
       srcAnnot <- case getLetAnnot src of
                     Just x -> return x
                     Nothing -> Left "The impossible happened: failed\
                                     \ to re-parse a Let expression."
       return (srcAnnot, _A)

annotateLet' pos ctx (Note src (Let (Binding { variable = x, annotation = ann, value = a }) e))
  | coversAnn pos ann = annotateLet' pos ctx (annotationExpr ann)
  | covers pos a = annotateLet' pos ctx a
  | pos `inside` src = do
      ctx' <- first show $ extendLet x a ctx
      annotateLet' pos ctx' e

annotateLet' pos ctx (Note src (Lam _ FunctionBinding{ functionBindingVariable = x, functionBindingAnnotation = _A } b))
  | covers pos _A = annotateLet' pos ctx _A
  | pos `inside` src = do
      ctx' <- first show $ extendBinder x _A ctx
      annotateLet' pos ctx' b

annotateLet' pos ctx (Note src (Pi _ x _A _B))
  | covers pos _A = annotateLet' pos ctx _A
  | pos `inside` src = do
      ctx' <- first show $ extendBinder x _A ctx
      annotateLet' pos ctx' _B

-- we need to unfold Notes to make progress
annotateLet' pos ctx (Note _ expr) =
  annotateLet' pos ctx expr

-- catch-all
annotateLet' pos ctx expr = do
  let subExprs = toListOf subExpressions expr
  case [ Note src e | (Note src e) <- subExprs, pos `inside` src ] of
    [e] -> annotateLet' pos ctx e
    _ -> Left "You weren't pointing at a let binder!"

-- | Expression and normalized type at this position.
--
--   Used when completing `(f x).` or `{ a = 1 }.`: the cursor sits on the
--   expression before the dot, inside a file that typechecks once that dot
--   is removed.  The expression is not normalized.  The type is, so a record
--   or union hidden by a type synonym still exposes its fields.
normalizedAt :: Position -> WellTyped -> Maybe (Expr Src Void, Expr Src Void)
normalizedAt pos expr = do
    expr' <- splitMultiLetSrc (fromWellTyped expr)
    either (const Nothing) Just (normalizedAt' pos emptyTypingContext expr')

normalizedAt' :: Position -> TypingContext Src -> Expr Src Void -> Either (TypeError Src Void) (Expr Src Void, Expr Src Void)
normalizedAt' pos ctx (Note src (Let (Binding { variable = x, annotation = ann, value = a }) e))
    | coversAnn pos ann = normalizedAt' pos ctx (annotationExpr ann)
    | covers pos a = normalizedAt' pos ctx a
    | pos `inside` src = do
        ctx' <- extendLet x a ctx
        normalizedAt' pos ctx' e
normalizedAt' pos ctx (Note src (Lam _ FunctionBinding { functionBindingVariable = x, functionBindingAnnotation = _A } b))
    | covers pos _A = normalizedAt' pos ctx _A
    | pos `inside` src = do
        ctx' <- extendBinder x _A ctx
        normalizedAt' pos ctx' b
normalizedAt' pos ctx (Note src (Pi _ x _A _B))
    | covers pos _A = normalizedAt' pos ctx _A
    | pos `inside` src = do
        ctx' <- extendBinder x _A ctx
        normalizedAt' pos ctx' _B
normalizedAt' pos ctx (Note _ expr) =
    normalizedAt' pos ctx expr
normalizedAt' pos ctx expr = do
    let subExprs = toListOf subExpressions expr
    case [ (src, e) | (Note src e) <- subExprs, pos `inside` src ] of
        [] -> do
            typ <- typeWithContext ctx expr
            return (expr, normalizeWithContext ctx typ)
        ((src, e) : _) ->
            normalizedAt' pos ctx (Note src e)

-- | Normalize a selected expression with the values of the let bindings
--   that enclose it: selecting @a + 1@ in @let a = 2 in a + 1@ gives @3@.
--
--   The selection is identified by the smallest subexpression containing
--   @pos@ whose source text equals @selected@ up to surrounding whitespace.
--   The 'Int' is the output byte budget for quoting; the returned 'Bool' is
--   'True' when the normal form was cut short.  'Nothing' when the selection
--   matches no subexpression or an enclosing binding does not typecheck.
scopedNormalize
    :: Int -> Position -> Text -> WellTyped -> Maybe (Expr Src Void, Bool)
scopedNormalize budget pos selected expr = do
    expr' <- splitMultiLetSrc (fromWellTyped expr)
    (ctx, found) <- scopedAt pos selected emptyTypingContext expr'
    return (normalizeWithContextBounded budget ctx found)

scopedAt
    :: Position
    -> Text
    -> TypingContext Src
    -> Expr Src Void
    -> Maybe (TypingContext Src, Expr Src Void)
scopedAt pos selected ctx expr@(Note src _)
    | Text.strip (srcText src) == Text.strip selected =
        Just (ctx, expr)
scopedAt pos selected ctx (Note src (Let (Binding { variable = x, annotation = ann, value = a }) e))
    | coversAnn pos ann = scopedAt pos selected ctx (annotationExpr ann)
    | covers pos a = scopedAt pos selected ctx a
    | pos `inside` src = do
        ctx' <- either (const Nothing) Just (extendLet x a ctx)
        scopedAt pos selected ctx' e
scopedAt pos selected ctx (Note src (Lam _ FunctionBinding { functionBindingVariable = x, functionBindingAnnotation = _A } b))
    | covers pos _A = scopedAt pos selected ctx _A
    | pos `inside` src = do
        ctx' <- either (const Nothing) Just (extendBinder x _A ctx)
        scopedAt pos selected ctx' b
scopedAt pos selected ctx (Note src (Pi _ x _A _B))
    | covers pos _A = scopedAt pos selected ctx _A
    | pos `inside` src = do
        ctx' <- either (const Nothing) Just (extendBinder x _A ctx)
        scopedAt pos selected ctx' _B
scopedAt pos selected ctx (Note _ expr) =
    scopedAt pos selected ctx expr
scopedAt pos selected ctx expr =
    case [ Note src e | (Note src e) <- toListOf subExpressions expr, pos `inside` src ] of
        (child : _) ->
            scopedAt pos selected ctx child
        [] ->
            Nothing

-- Make sure all lets in a multilet are annotated with their source information.
--
-- A resolved import is still wrapped in the source span of the import path.
-- That span is not a @let@ block, so 'getLetInner' cannot split it.  One such
-- node used to make this return 'Nothing' for the whole file.  Leave that
-- node in place and keep walking, so a binding in the importing file can
-- still be found.
splitMultiLetSrc :: Expr Src a -> Maybe (Expr Src a)
splitMultiLetSrc (Note src (Let b (Let b' e))) =
  case getLetInner src of
    Just src' ->
      splitMultiLetSrc (Note src (Let b (Note src' (Let b' e))))
    Nothing -> do
      inner <- subExpressions splitMultiLetSrc (Let b (Let b' e))
      return (Note src inner)
splitMultiLetSrc expr = subExpressions splitMultiLetSrc expr

-- Check if range lies completely inside a given subexpression.
-- This version takes trailing whitespace into account
-- (c.f. `sanitiseRange` from Backend.Diangostics).
inside :: Position -> Src -> Bool
inside pos src = left <= pos && pos < right
  where Range left right = rangeFromDhall src

covers :: Position -> Expr Src a -> Bool
covers pos (Note src _) = pos `inside` src
covers _ _ = False

coversAnn :: Position -> Maybe (Maybe Src, Expr Src a) -> Bool
coversAnn _ Nothing = False
coversAnn pos (Just (_, expr)) = covers pos expr

-- | Binder span and type of each unannotated @let@.
--
--   The walk uses the same context as 'typeAt''.  A file that does not
--   typecheck never reaches this function.
letTypes :: WellTyped -> [(Src, Expr Src Void)]
letTypes expr =
    case splitMultiLetSrc (fromWellTyped expr) of
        Just expr' ->
            either (const []) id (letTypes' emptyTypingContext expr')
        Nothing ->
            []

letTypes' :: TypingContext Src -> Expr Src Void -> Either (TypeError Src Void) [(Src, Expr Src Void)]
letTypes' ctx (Note _ (Let (Binding { variable = x, annotation = ann, bindingSrc1 = src, value = a }) e)) = do
    let here = case (ann, src) of
            (Nothing, Just binder) ->
                case typeWithContext ctx a of
                    Right ty -> [(binder, ty)]
                    Left _ -> []
            _ ->
                []
    nested <- letTypes' ctx a
    ctx' <- extendLet x a ctx
    rest <- letTypes' ctx' e
    return (here ++ nested ++ rest)
letTypes' ctx (Note _ (Lam _ FunctionBinding { functionBindingVariable = x, functionBindingAnnotation = _A } b)) = do
    nested <- letTypes' ctx _A
    ctx' <- extendBinder x _A ctx
    rest <- letTypes' ctx' b
    return (nested ++ rest)
letTypes' ctx (Note _ (Pi _ x _A _B)) = do
    nested <- letTypes' ctx _A
    ctx' <- extendBinder x _A ctx
    rest <- letTypes' ctx' _B
    return (nested ++ rest)
letTypes' ctx (Note _ expr) =
    letTypes' ctx expr
letTypes' ctx expr =
    fmap concat (mapM (letTypes' ctx) (toListOf subExpressions expr))

annotationExpr :: Maybe (Maybe Src, Expr Src Void) -> Expr Src Void
annotationExpr (Just (_, expr)) = expr
annotationExpr Nothing = Const Type
