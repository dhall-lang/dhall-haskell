module Dhall.LSP.Backend.Completion where

import Data.Text (Text)
import Dhall.LSP.Backend.Diagnostics (Position, positionToOffset)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, (</>))
import System.Environment (getEnvironment)

import Dhall.Context (empty, toList)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as Text
import Dhall.Context (Context, insert)
import Dhall.Core (Expr(..), Binding(..), Var(..), normalize, shift, Const(..), subst, pretty, reservedIdentifiers)
import Dhall.TypeCheck (X(..), typeWithA, typeOf)
import Dhall.Parser (Src, exprFromText)
import qualified Dhall.Map
import qualified Data.HashSet as HashSet

import Dhall.LSP.Backend.Parsing (holeExpr)

import Debug.Trace

-- | Given the cursor position construct the corresponding 'completion query'
-- consisting of the leadup, i.e. text leading up to the word prefix that is to
-- be completed, as well as the prefix that is to be completed.
completionQueryAt :: Text -> Position -> (Text, Text)
completionQueryAt text pos = (completionLeadup, completionPrefix)
 where
  off = positionToOffset text pos
  text' = Text.take off text
  breakEnd :: (Char -> Bool) -> Text -> (Text, Text)
  breakEnd p =
    (\(l,r) -> (Text.reverse l, Text.reverse r)) . Text.break p . Text.reverse
  (completionPrefix, completionLeadup) =
    breakEnd (`elem` (" \t\n[(,=+*&|}#?>" :: [Char])) text'

-- | A completion result, optionally annotated with type information.
data Completion =
  Completion {
    completeText :: Text,
    completeType :: Maybe (Expr Src X) }

-- | Complete file names.
completeLocalImport :: FilePath -> FilePath -> IO [Completion]
completeLocalImport relativeTo prefix = do
  let dir = takeDirectory relativeTo </> takeDirectory prefix
  exists <- doesDirectoryExist dir
  if not exists
  then return []
  else do
    files <- listDirectory dir
    return (map (\file -> Completion (Text.pack file) Nothing) files)

-- | Complete environment variables.
completeEnvironmentImport :: IO [Completion]
completeEnvironmentImport = do
  environment <- getEnvironment
  let environmentImports = map (Text.pack . fst) environment
  return $ map (\env -> Completion env Nothing) environmentImports


-- | A completion context, consisting of the (approximated) type-checking
-- context. We need to substitute 'dependent lets' later so we keep their values
-- around.
data CompletionContext =
  CompletionContext {
    context :: Context (Expr Src X),
    -- values to be substituted for 'dependent let' behaviour
    values :: Context (Expr Src X) }

-- | Given a 'binders expression' (with arbitrarily many 'holes') construct the
-- corresponding completion context.
buildCompletionContext :: Expr Src X -> CompletionContext
buildCompletionContext = buildCompletionContext' empty empty

buildCompletionContext' :: Context (Expr Src X) -> Context (Expr Src X)
  -> Expr Src X -> CompletionContext
buildCompletionContext' context values (Let (Binding x mA a :| []) e)
  -- We prefer the actual value over the annotated type in order to get
  -- 'dependent let' behaviour whenever possible.
  | Right _A <- typeWithA absurd context a
  , Right kind <- typeWithA absurd context _A =
    let _A' = normalize _A
        kind' = normalize kind

        context' = fmap (shift 1 (V x 0)) $ insert x _A' context

        a' = normalize a

        e' | Const k <- kind', k `elem` [Kind, Sort] =
             subst (V x 0) a' e  -- 'dependent let'
           | otherwise = e

        values' | Const k <- kind', k `elem` [Kind, Sort] =
                  fmap (shift 1 (V x 0)) $ insert x a' values
                | otherwise = fmap (shift 1 (V x 0)) $ insert x holeExpr values

    in buildCompletionContext' context' values' e'

  -- fall back to annotated type if body doesn't type check; can't do 'dependent let'
  | Just _A <- mA
  , Right _ <- typeWithA absurd context _A =
    let _A' = normalize _A
        context' = fmap (shift 1 (V x 0)) $ insert x _A' context
        values' = fmap (shift 1 (V x 0)) $ insert x holeExpr values

    in buildCompletionContext' context' values' e

  -- if nothing works, only remember the name (but bind to `holeExpr`)
  | otherwise =
    let context' = fmap (shift 1 (V x 0)) $ insert x holeExpr context
        values' = fmap (shift 1 (V x 0)) $ insert x holeExpr values

    in buildCompletionContext' context' values' e

buildCompletionContext' context values (Lam x _A b) =
  let _A' | Right _ <- typeWithA absurd context _A = normalize _A
          | otherwise = holeExpr

      context' = fmap (shift 1 (V x 0)) $ insert x _A' context
      values' = fmap (shift 1 (V x 0)) $ insert x holeExpr values

    in buildCompletionContext' context' values' b

buildCompletionContext' context values (Pi x _A b) =
  let _A' | Right _ <- typeWithA absurd context _A = normalize _A
          | otherwise = holeExpr

      context' = fmap (shift 1 (V x 0)) $ insert x _A' context
      values' = fmap (shift 1 (V x 0)) $ insert x holeExpr values

    in buildCompletionContext' context' values' b

-- catch-all
buildCompletionContext' context values _ = CompletionContext context values

-- Helper. Given `Dhall.Context.toList ctx` construct the corresponding variable
-- names.
contextToVariables :: [(Text, Expr Src X)] -> [Var]
contextToVariables  [] = []
contextToVariables ((name, _) : rest) =
  V name 0 : map (inc name) (contextToVariables rest)
 where inc x (V y i) | x == y = V x (i + 1)
                     | otherwise = V y i

-- | Complete identifiers from the given completion context.
completeFromContext :: CompletionContext -> [Completion]
completeFromContext (CompletionContext context _) =
  let context' = toList context
      completeReserved keyword
        | Right expr <- exprFromText "" keyword
        , Right typ <- typeOf (fmap undefined expr) =
          Completion keyword (Just typ)
        | otherwise = Completion keyword Nothing
      reserved = map completeReserved $ HashSet.toList reservedIdentifiers
  in [ Completion (pretty var) (if typ == holeExpr then Nothing else Just typ)
     | (var, (_, typ)) <- zip (contextToVariables context') (toList context) ]
     ++ reserved

-- | Complete union constructors and record projections.
completeProjections :: CompletionContext -> Expr Src X -> [Completion]
completeProjections (CompletionContext context values) expr =
  -- substitute 'dependent lets', necessary for completion of unions
  let values' = toList values
      subs = filter ((/= holeExpr) . snd) $ zip (contextToVariables values') (map snd values')
      expr' = foldl (\e (x,val) -> subst x val e) expr subs

  in case typeWithA absurd (traceShow (map fst $ toList context) $ context) expr' of
      Left _ -> []
      Right _A ->
        let expr'' = normalize expr'
        in completeUnion expr'' expr'' ++ completeRecord (normalize _A)

 where
  -- complete a union constructor by inspecting the union value
  completeUnion _A (Union m) =
    let constructor (k, Nothing) = Completion k (Just _A)
        constructor (k, Just v) = Completion k (Just (Pi k v _A))
     in map constructor (Dhall.Map.toList m)
  completeUnion _ _ = []


  -- complete a record projection by inspecting the record type
  completeRecord (Record m) =
    map (\(name, typ) -> Completion name (Just typ)) (Dhall.Map.toList m)
  completeRecord _ = []
