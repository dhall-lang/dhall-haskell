module Dhall.LSP.Backend.Completion where

import Data.List                     (foldl')
import Data.Text                     (Text)
import Data.Void                     (Void, absurd)
import Dhall.Context                 (Context, empty, insert, toList)
import Dhall.LSP.Backend.Diagnostics (Position, positionToOffset)
import Dhall.LSP.Backend.Parsing     (holeExpr)
import Dhall.Parser                  (Src, exprFromText)
import Dhall.Pretty                  (UnescapedLabel (..))
import Dhall.TypeCheck               (typeOf, typeWithA)
import System.Directory              (doesDirectoryExist, listDirectory)
import System.Environment            (getEnvironment)
import System.FilePath               (takeDirectory, (</>))
import System.Timeout                (timeout)

import Dhall.Core
    ( Binding (..)
    , Expr (..)
    , FunctionBinding (..)
    , RecordField (..)
    , Var (..)
    , normalize
    , pretty
    , shallowDenote
    , reservedIdentifiers
    , shift
    , subst
    )

import qualified Data.HashSet as HashSet
import qualified Data.Text    as Text
import qualified Dhall.Map
import qualified Dhall.Pretty

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
    breakEnd (`elem` (" \t\n[(,=+*&|}#?>" :: String)) text'

-- | The expression that ends at the end of the input, and the offset where
--   it starts.
--
--   Used to find the completion target before a dot while the surrounding
--   code does not parse yet.  The scan tracks strings, comments and bracket
--   nesting; the target starts after the last operator at bracket depth
--   zero, so @let x = { a = 1 }@ yields @{ a = 1 }@.  Returns 'Nothing' when
--   the trailing expression is empty or a bracket is still open.
expressionBefore :: Text -> Maybe (Int, Text)
expressionBefore raw
    | Text.null text = Nothing
    | not (null brackets) = Nothing
    | Text.null target = Nothing
    | otherwise = Just (start, target)
  where
    text = Text.stripEnd raw
    (brackets, boundary) = scan 0 Nothing Code [] 0 text
    dropped = Text.drop boundary text
    start = boundary + Text.length dropped - Text.length target
    target = Text.stripStart dropped

    -- Operators that end the trailing expression at bracket depth zero.
    -- Whitespace is not one: an application like @(f x) 1.@ completes from
    -- the whole application.
    separators = "=+*&|?#:,\x2192" :: String

    scan :: Int -> Maybe Char -> ScanMode -> [Char] -> Int -> Text -> ([Char], Int)
    scan index previous mode stack cut rest =
        case Text.uncons rest of
            Nothing ->
                (stack, cut)
            Just (c, cs) ->
                case mode of
                    Code
                        | c == '"' ->
                            scan (index + 1) (Just c) InString stack cut cs
                        | c == '\'', Just ('\'', cs') <- Text.uncons cs ->
                            scan (index + 2) (Just '\'') InMulti stack cut cs'
                        | c == '-', Just ('-', cs') <- Text.uncons cs ->
                            scan (index + 2) (Just '-') InLineComment stack cut cs'
                        | c == '{', Just ('-', cs') <- Text.uncons cs ->
                            scan (index + 2) (Just '-') (InBlockComment 1) stack cut cs'
                        | c == '{' ->
                            scan (index + 1) (Just c) Code ('}' : stack) cut cs
                        | c == '(' ->
                            scan (index + 1) (Just c) Code (')' : stack) cut cs
                        | c == '[' ->
                            scan (index + 1) (Just c) Code (']' : stack) cut cs
                        | c == '<' ->
                            scan (index + 1) (Just c) Code ('>' : stack) cut cs
                        | c == '>', previous == Just '-' ->
                            -- an arrow, not a union closer
                            scan (index + 1) (Just c) Code stack
                                (if null stack then index + 1 else cut) cs
                        | c `elem` ("})]>" :: String) ->
                            case stack of
                                top : inner
                                    | top == c ->
                                        scan (index + 1) (Just c) Code inner cut cs
                                [] ->
                                    -- a stray closer ends the trailing expression
                                    scan (index + 1) (Just c) Code stack (index + 1) cs
                                _ ->
                                    scan (index + 1) (Just c) Code stack cut cs
                        | null stack && c `elem` separators ->
                            scan (index + 1) (Just c) Code stack (index + 1) cs
                        | otherwise ->
                            scan (index + 1) (Just c) Code stack cut cs
                    InString
                        | c == '\\', Just (_, cs') <- Text.uncons cs ->
                            scan (index + 2) (Just 'x') InString stack cut cs'
                        | c == '"' ->
                            scan (index + 1) (Just c) Code stack cut cs
                        | otherwise ->
                            scan (index + 1) (Just c) InString stack cut cs
                    InMulti
                        | c == '\'', Just ('\'', cs') <- Text.uncons cs ->
                            case Text.uncons cs' of
                                -- ''' is an escaped '' inside a multi-line literal
                                Just ('\'', cs'') ->
                                    scan (index + 3) (Just '\'') InMulti stack cut cs''
                                _ ->
                                    scan (index + 2) (Just '\'') Code stack cut cs'
                        | otherwise ->
                            scan (index + 1) (Just c) InMulti stack cut cs
                    InLineComment
                        | c == '\n' ->
                            scan (index + 1) (Just c) Code stack cut cs
                        | otherwise ->
                            scan (index + 1) (Just c) InLineComment stack cut cs
                    InBlockComment depth
                        | c == '{', Just ('-', cs') <- Text.uncons cs ->
                            scan (index + 2) (Just '-') (InBlockComment (depth + 1)) stack cut cs'
                        | c == '-', Just ('}', cs') <- Text.uncons cs ->
                            let depth' = depth - 1
                                mode' = if depth' == 0 then Code else InBlockComment depth'
                            in scan (index + 2) (Just '}') mode' stack cut cs'
                        | otherwise ->
                            scan (index + 1) (Just c) (InBlockComment depth) stack cut cs

data ScanMode = Code | InString | InMulti | InLineComment | InBlockComment !Int

-- | A completion result, optionally annotated with type information.
data Completion =
  Completion {
    completeText :: Text,
    completeType :: Maybe (Expr Src Void) }

-- | Complete file names.
completeLocalImport :: FilePath -> FilePath -> IO [Completion]
completeLocalImport relativeTo prefix = do
  let dir = takeDirectory relativeTo </> takeDirectory prefix
  exists <- doesDirectoryExist dir
  if not exists
  then return []
  else do
    let second = 10 ^ (6 :: Int)
    mFiles <- timeout second (listDirectory dir)  -- 1s timeout
    case mFiles of
      Just files -> return (map (\file -> Completion (Text.pack file) Nothing) files)
      Nothing -> return []

-- | Complete environment variables.
completeEnvironmentImport :: IO [Completion]
completeEnvironmentImport = do
  environment <- getEnvironment

  let toCompletion (variable, _) = Completion escapedVariable Nothing
       where
          escapedVariable =
              Dhall.Pretty.escapeEnvironmentVariable (Text.pack variable)

  return (map toCompletion environment)

-- | A completion context, consisting of the (approximated) type-checking
-- context. We need to substitute 'dependent lets' later so we keep their values
-- around.
data CompletionContext =
  CompletionContext {
    context :: Context (Expr Src Void),
    -- values to be substituted for 'dependent let' behaviour
    values :: Context (Expr Src Void) }

-- | Given a 'binders expression' (with arbitrarily many 'holes') construct the
-- corresponding completion context.
buildCompletionContext :: Expr Src Void -> CompletionContext
buildCompletionContext = buildCompletionContext' empty empty

buildCompletionContext' :: Context (Expr Src Void) -> Context (Expr Src Void)
  -> Expr Src Void -> CompletionContext
buildCompletionContext' context values (Let (Binding { variable = x, annotation = mA, value = a }) e)
  -- We prefer the actual value over the annotated type in order to get
  -- 'dependent let' behaviour whenever possible.
  | Right _A <- typeWithA absurd context a =
    let _A' = normalize _A

        a' = normalize a
        e' = subst (V x 0) a' e

        context' = shift 1 (V x 0) <$> insert x _A' context
        values' = shift 1 (V x 0) <$> insert x a' values

    in buildCompletionContext' context' values' e'

  -- fall back to annotated type if body doesn't type check; bind to `holeExpr`
  | Just (_, _A) <- mA
  , Right _ <- typeWithA absurd context _A =
    let _A' = normalize _A

        context' = shift 1 (V x 0) <$> insert x _A' context
        values' = shift 1 (V x 0) <$> insert x holeExpr values

    in buildCompletionContext' context' values' e

  -- if nothing works, only remember the name (but bind to `holeExpr`)
  | otherwise =
    let context' = shift 1 (V x 0) <$> insert x holeExpr context
        values' = shift 1 (V x 0) <$> insert x holeExpr values

    in buildCompletionContext' context' values' e

buildCompletionContext' context values (Lam _ (FunctionBinding { functionBindingVariable = x, functionBindingAnnotation = _A}) b) =
  let _A' | Right _ <- typeWithA absurd context _A = normalize _A
          | otherwise = holeExpr

      context' = shift 1 (V x 0) <$> insert x _A' context
      values' = shift 1 (V x 0) <$> insert x holeExpr values

    in buildCompletionContext' context' values' b

buildCompletionContext' context values (Pi _ x _A b) =
  let _A' | Right _ <- typeWithA absurd context _A = normalize _A
          | otherwise = holeExpr

      context' = shift 1 (V x 0) <$> insert x _A' context
      values' = shift 1 (V x 0) <$> insert x holeExpr values

    in buildCompletionContext' context' values' b

-- catch-all
buildCompletionContext' context values _ = CompletionContext context values

-- Helper. Given `Dhall.Context.toList ctx` construct the corresponding variable
-- names.
contextToVariables :: [(Text, Expr Src Void)] -> [Var]
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
        , Right typ <- typeOf (do _ <- expr; holeExpr) =
          Completion keyword (Just typ)
        | otherwise = Completion keyword Nothing
      reserved = map completeReserved $ HashSet.toList reservedIdentifiers
  in [ Completion (pretty var) (if typ == holeExpr then Nothing else Just typ)
     | (var, (_, typ)) <- zip (contextToVariables context') context' ]
     ++ reserved

-- | Complete union constructors and record projections.
completeProjections :: CompletionContext -> Expr Src Void -> [Completion]
completeProjections (CompletionContext context values) expr =
  -- substitute 'dependent lets', necessary for completion of unions
  let values' = toList values
      subs = filter ((/= holeExpr) . snd) $ zip (contextToVariables values') (map snd values')
      expr' = foldl' (\e (x,val) -> subst x val e) expr subs

  in case typeWithA absurd context expr' of
      Left _ -> []
      Right _A ->
        -- Field and constructor names come from the type spine; full
        -- normalization of large records is unnecessary here.
        completionsFromNormal expr' (shallowDenote _A)

-- | Fields of a record type, and constructors of a union type.
--
--   The expression itself is not normalized.  A union written before the
--   dot is already a union, so its constructors come from that expression.
--   A record projection comes from the type.
completionsFromNormal :: Expr Src Void -> Expr Src Void -> [Completion]
completionsFromNormal value ty =
    let value' = shallowDenote value
        ty' = shallowDenote ty
    in completeRecord ty'
        ++ case value' of
            Union _ ->
                -- A union written before the dot.  Constructors inhabit that
                -- union, so the result type is the union itself.
                completeUnion value' value'
            _ ->
                completeUnion ty' ty'

-- complete a union constructor by inspecting the union value
completeUnion :: Expr Src Void -> Expr Src Void -> [Completion]
completeUnion _A (Union m) =
    let constructor (k, Nothing) =
            Completion (Dhall.Pretty.escapeLabel AnyLabelOrSome k) (Just _A)
        constructor (k, Just v) =
            Completion (Dhall.Pretty.escapeLabel AnyLabelOrSome k) (Just (Pi mempty k v _A))
     in map constructor (Dhall.Map.toList m)
completeUnion _ _ =
    []

-- complete a record projection by inspecting the record type
completeRecord :: Expr Src Void -> [Completion]
completeRecord (Record m) =
    map toCompletion (Dhall.Map.toList (recordFieldValue <$> m))
  where
    toCompletion (name_, typ) =
        Completion (Dhall.Pretty.escapeLabel AnyLabel name_) (Just typ)
completeRecord _ =
    []
