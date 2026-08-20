{-# LANGUAGE OverloadedStrings #-}
--
-- Evaluation benchmark harness for Dhall import/resolve/typecheck/normalize.
--
-- See benchmark/evaluation/README.md for how to read results. Three modes:
--
--   Mode A (phase): prep resolves with UseSemanticCache; timed resolve uses
--     IgnoreSemanticCache (semantic off, semisemantic still on); typecheck and
--     evaluation run on the pre-resolved AST from prep.
--
--   Mode B (resolve_cold_cache_on): parse-only prep; each sample uses a fresh
--     XDG_CACHE_HOME with caches enabled. Used for prep-sensitive Code large6
--     variants, prelude_import, and substitutions (library substitution path).
--
--   Mode D (end_to_end_cold): parse-only prep; each sample uses a fresh
--     XDG_CACHE_HOME and runs resolveWithSettings → typeOf → normalize. Used by
--     substitutions.composer_proxy (synthetic substitution-heavy end-to-end).
--
--   Mode C (implicit): some as Source large6 costs appear on evaluation, not
--     resolve. See large6/README.md matrix.
--
module Main where

import Control.Exception  (bracket, throw)
import Control.Monad      (foldM)
import Control.Monad.Trans.State.Strict (runState, state)
import Data.Char         (toLower)
import Data.List         (isInfixOf, isPrefixOf, isSuffixOf, mapAccumL, sort)
import Data.Maybe        (isNothing, listToMaybe, mapMaybe)
import Data.Text         (Text)
import Dhall.Core        (Binding (..), FunctionBinding (..), Var (..))
import Data.Time.Clock   (diffUTCTime, getCurrentTime)
import Data.Void         (Void)
import System.Environment (getArgs, lookupEnv, setEnv, unsetEnv)
import System.FilePath   ((</>), takeBaseName, takeDirectory)
import System.IO         (hFlush, stdout)
import Text.Printf       (printf)
import Test.Tasty.Bench

import qualified Data.ByteString      as StrictByteString
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Foldable.WithIndex as Foldable.WithIndex
import qualified Data.Functor.Const   as FunctorConst
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text.IO
import qualified Dhall
import qualified Dhall.Binary         as Binary
import qualified Dhall.Core           as Core
import qualified Dhall.Import         as Import
import qualified Dhall.Map
import qualified Dhall.Parser         as Parser
import qualified Dhall.Substitution
import qualified Dhall.TypeCheck      as TypeCheck
import qualified Lens.Micro
import           Lens.Micro           ((^.))
import qualified System.Directory     as Directory
import qualified System.IO.Temp       as Temp

type ParsedExpr = Core.Expr Parser.Src Core.Import
type ResolvedExpr = Core.Expr Parser.Src Void

-- | Shared @x + x@ tree for @semisemantic.nf_size_walk@. Unique nodes =
-- depth+1; a full walk visits @2^(depth+1)-1@ constructors (no DAG memo).
-- Depth 20 is ~2M visits, well above the 64KiB store-NF cutoff.
-- 'NOINLINE' keeps GHC from constant-folding the size walk out of the bench.
{-# NOINLINE sharedNaturalPlusTree #-}
sharedNaturalPlusTree :: Int -> Core.Expr Void Void
sharedNaturalPlusTree depth = go depth
  where
    go 0 = Core.NaturalLit 0
    go n = let t = go (n - 1) in Core.NaturalPlus t t

nfSizeWalkDepth :: Int
nfSizeWalkDepth = 20

nfSizeWalkThreshold :: Int
nfSizeWalkThreshold = 64 * 1024

-- | Full-tree size estimate (naive baseline for @semisemantic.nf_size_walk.full@).
-- Same metric as 'nfSizeWalkExceedsThreshold'; kept in the harness so this
-- group does not depend on Import internals.
nfSizeWalkEstimate :: Core.Expr s a -> Int
nfSizeWalkEstimate expression =
    case expression of
        Core.TextLit (Core.Chunks xys z) ->
            1
          + Text.length z
          + sum [ Text.length t + nfSizeWalkEstimate x | (t, x) <- xys ]
        Core.BytesLit bytes ->
            1 + StrictByteString.length bytes
        Core.Embed _ ->
            1
        _ ->
            let FunctorConst.Const childSizes =
                    Core.subExpressions
                        (\child -> FunctorConst.Const [nfSizeWalkEstimate child])
                        expression
            in  1 + sum childSizes

-- | Early-abort size check (optimized side of @semisemantic.nf_size_walk@).
nfSizeWalkExceedsThreshold :: Core.Expr s a -> Bool
nfSizeWalkExceedsThreshold expression =
    isNothing (spend nfSizeWalkThreshold expression)
  where
    debit budget cost
        | budget < cost = Nothing
        | otherwise     = Just (budget - cost)

    spend budget _
        | budget <= 0 =
            Nothing
    spend budget (Core.TextLit (Core.Chunks xys z)) = do
        b0 <- debit budget (1 + Text.length z)
        foldM
            (\b (t, x) -> debit b (Text.length t) >>= (`spend` x))
            b0
            xys
    spend budget (Core.BytesLit bytes) =
        debit budget (1 + StrictByteString.length bytes)
    spend budget (Core.Embed _) =
        debit budget 1
    spend budget expr = do
        b0 <- debit budget 1
        let FunctorConst.Const kids =
                Core.subExpressions
                    (\child -> FunctorConst.Const [child])
                    expr
        foldM spend b0 kids

-- | In-harness copies of the HEAD substitution algorithms used by
-- @substitutions.shift_cost@. Kept here so the first benchmark commit
-- compiles (those helpers do not exist yet) while still resolving the map
-- once and comparing @substituteManyNaive@ against @substituteManyFromRoot@.
data ShiftCostResolved s a = ShiftCostResolved
    { scrMap                :: Map.Map Var (Core.Expr s a)
    , scrKeyNames           :: Set.Set Text
    , scrValueFreeNames     :: Set.Set Text
    , scrValueFreeNamesByVar :: Map.Map Var (Set.Set Text)
    }

resolveShiftCost
    :: Dhall.Substitution.Substitutions s a -> ShiftCostResolved s a
resolveShiftCost substitutions =
    let step k v acc =
            Map.insert (V k 0) (shiftCostRaw acc v) acc

        resolvedMap' =
            Foldable.WithIndex.ifoldr step Map.empty substitutions

        freeByVar = Map.map shiftCostFreeVarNames resolvedMap'
    in  ShiftCostResolved
            { scrMap = resolvedMap'
            , scrKeyNames = Set.fromList [ k | V k _ <- Map.keys resolvedMap' ]
            , scrValueFreeNames = foldMap id freeByVar
            , scrValueFreeNamesByVar = freeByVar
            }

shiftCostRaw
    :: Map.Map Var (Core.Expr s a) -> Core.Expr s a -> Core.Expr s a
shiftCostRaw substitutions expression
     | Map.null substitutions = expression
shiftCostRaw substitutions (Core.Var v) =
     Map.findWithDefault (Core.Var v) v substitutions
shiftCostRaw substitutions (Core.Lam cs (FunctionBinding src0 y src1 src2 type_) body) =
     let type_' = shiftCostRaw substitutions type_
         body' = shiftCostRaw (shiftCostRawShift y substitutions) body
     in Core.Lam cs (FunctionBinding src0 y src1 src2 type_') body'
shiftCostRaw substitutions (Core.Pi cs y domain codomain) =
     let domain' = shiftCostRaw substitutions domain
         codomain' = shiftCostRaw (shiftCostRawShift y substitutions) codomain
     in Core.Pi cs y domain' codomain'
shiftCostRaw substitutions (Core.Let (Binding src0 f src1 type_ src2 replacement) body) =
     let type_' = fmap (fmap (shiftCostRaw substitutions)) type_
         replacement' = shiftCostRaw substitutions replacement
         body' = shiftCostRaw (shiftCostRawShift f substitutions) body
     in Core.Let (Binding src0 f src1 type_' src2 replacement') body'
shiftCostRaw substitutions expression =
     Lens.Micro.over Core.subExpressions (shiftCostRaw substitutions) expression

shiftCostRawShift
    :: Text -> Map.Map Var (Core.Expr s a) -> Map.Map Var (Core.Expr s a)
shiftCostRawShift name substitutions =
     let shiftKey (V k n) = if k == name then V k (n + 1) else V k n
         shiftValue = Core.shift 1 (V name 0)
         step k v = Map.insert (shiftKey k) (shiftValue v)
     in Map.foldrWithKey step Map.empty substitutions

shiftCostFreeVarNames :: Core.Expr s a -> Set.Set Text
shiftCostFreeVarNames = go Map.empty
  where
    go bound (Core.Var (V k n)) =
        case Map.lookup k bound of
            Just depth | n < depth -> Set.empty
            _                      -> Set.singleton k
    go bound (Core.Lam _ (FunctionBinding _ y _ _ type_) body) =
        go bound type_ <> go (Map.insertWith (+) y 1 bound) body
    go bound (Core.Pi _ y domain codomain) =
        go bound domain <> go (Map.insertWith (+) y 1 bound) codomain
    go bound (Core.Let (Binding _ f _ type_ _ replacement) body) =
           foldMap (go bound . snd) type_
        <> go bound replacement
        <> go (Map.insertWith (+) f 1 bound) body
    go bound expression =
        Lens.Micro.foldMapOf Core.subExpressions (go bound) expression

shiftCostSubstituteMany
    :: ShiftCostResolved s a -> Core.Expr s a -> Core.Expr s a
shiftCostSubstituteMany substitutions expression
     | Map.null (scrMap substitutions) = expression
shiftCostSubstituteMany substitutions (Core.Var v) =
     Map.findWithDefault (Core.Var v) v (scrMap substitutions)
shiftCostSubstituteMany substitutions (Core.Lam cs (FunctionBinding src0 y src1 src2 type_) body) =
     let type_' = shiftCostSubstituteMany substitutions type_
         body' = shiftCostSubstituteMany (shiftCostShift y substitutions) body
     in Core.Lam cs (FunctionBinding src0 y src1 src2 type_') body'
shiftCostSubstituteMany substitutions (Core.Pi cs y domain codomain) =
     let domain' = shiftCostSubstituteMany substitutions domain
         codomain' = shiftCostSubstituteMany (shiftCostShift y substitutions) codomain
     in Core.Pi cs y domain' codomain'
shiftCostSubstituteMany substitutions (Core.Let (Binding src0 f src1 type_ src2 replacement) body) =
     let type_' = fmap (fmap (shiftCostSubstituteMany substitutions)) type_
         replacement' = shiftCostSubstituteMany substitutions replacement
         body' = shiftCostSubstituteMany (shiftCostShift f substitutions) body
     in Core.Let (Binding src0 f src1 type_' src2 replacement') body'
shiftCostSubstituteMany substitutions expression =
     Lens.Micro.over Core.subExpressions (shiftCostSubstituteMany substitutions) expression

-- | Pre-plan-(1) walker: @Map.map shift@ on every value at a non-key binder.
-- No root-shift memo (plan (2)).
shiftCostNaiveWalk
    :: ShiftCostResolved s a -> Core.Expr s a -> Core.Expr s a
shiftCostNaiveWalk substitutions expression
     | Map.null (scrMap substitutions) = expression
shiftCostNaiveWalk substitutions (Core.Var v) =
     Map.findWithDefault (Core.Var v) v (scrMap substitutions)
shiftCostNaiveWalk substitutions (Core.Lam cs (FunctionBinding src0 y src1 src2 type_) body) =
     let type_' = shiftCostNaiveWalk substitutions type_
         body' = shiftCostNaiveWalk (shiftCostNaiveShift y substitutions) body
     in Core.Lam cs (FunctionBinding src0 y src1 src2 type_') body'
shiftCostNaiveWalk substitutions (Core.Pi cs y domain codomain) =
     let domain' = shiftCostNaiveWalk substitutions domain
         codomain' = shiftCostNaiveWalk (shiftCostNaiveShift y substitutions) codomain
     in Core.Pi cs y domain' codomain'
shiftCostNaiveWalk substitutions (Core.Let (Binding src0 f src1 type_ src2 replacement) body) =
     let type_' = fmap (fmap (shiftCostNaiveWalk substitutions)) type_
         replacement' = shiftCostNaiveWalk substitutions replacement
         body' = shiftCostNaiveWalk (shiftCostNaiveShift f substitutions) body
     in Core.Let (Binding src0 f src1 type_' src2 replacement') body'
shiftCostNaiveWalk substitutions expression =
     Lens.Micro.over Core.subExpressions (shiftCostNaiveWalk substitutions) expression

shiftCostNaiveShift
    :: Text -> ShiftCostResolved s a -> ShiftCostResolved s a
shiftCostNaiveShift name substitutions
    | Set.notMember name (scrKeyNames substitutions) =
        substitutions
            { scrMap =
                Map.map
                    (Core.shift 1 (V name 0))
                    (scrMap substitutions)
            }
    | otherwise =
        shiftCostShift name substitutions

shiftCostShift
    :: Text -> ShiftCostResolved s a -> ShiftCostResolved s a
shiftCostShift name substitutions
    | Set.notMember name (scrKeyNames substitutions)
    , Set.notMember name (scrValueFreeNames substitutions) =
        substitutions
    | Set.notMember name (scrKeyNames substitutions) =
        substitutions { scrMap = shiftCostMatchingValues name substitutions }
    | otherwise =
        let shiftKey (V k n) = if k == name then V k (n + 1) else V k n
            shiftValue = Core.shift 1 (V name 0)
            step k v = Map.insert (shiftKey k) (shiftValue v)
            shiftedMap = Map.foldrWithKey step Map.empty (scrMap substitutions)
        in  substitutions
                { scrMap = shiftedMap
                , scrValueFreeNamesByVar =
                    Map.mapKeys shiftKey (scrValueFreeNamesByVar substitutions)
                }

shiftCostMatchingValues
    :: Text -> ShiftCostResolved s a -> Map.Map Var (Core.Expr s a)
shiftCostMatchingValues name substitutions =
    let shiftValue = Core.shift 1 (V name 0)
        go k v acc =
            if maybe False (Set.member name) (Map.lookup k (scrValueFreeNamesByVar substitutions))
                then Map.insert k (shiftValue v) acc
                else acc
    in  Map.foldrWithKey go (scrMap substitutions) (scrMap substitutions)

shiftCostFromRoot
    :: Map.Map Text (ShiftCostResolved s a)
    -> ShiftCostResolved s a
    -> Core.Expr s a
    -> (Core.Expr s a, Map.Map Text (ShiftCostResolved s a))
shiftCostFromRoot cache root expression
    | Map.null (scrMap root) = (expression, cache)
shiftCostFromRoot cache root expression =
    runState (goRoot expression) cache
  where
    goRoot (Core.Var v) =
        pure (Map.findWithDefault (Core.Var v) v (scrMap root))
    goRoot (Core.Lam cs (FunctionBinding src0 y src1 src2 type_) body) = do
        type_' <- goRoot type_
        shifted <- cachedShift y
        let body' = shiftCostSubstituteMany shifted body
        pure (Core.Lam cs (FunctionBinding src0 y src1 src2 type_') body')
    goRoot (Core.Pi cs y domain codomain) = do
        domain' <- goRoot domain
        shifted <- cachedShift y
        let codomain' = shiftCostSubstituteMany shifted codomain
        pure (Core.Pi cs y domain' codomain')
    goRoot (Core.Let (Binding src0 f src1 type_ src2 replacement) body) = do
        type_' <- traverse (traverse goRoot) type_
        replacement' <- goRoot replacement
        shifted <- cachedShift f
        let body' = shiftCostSubstituteMany shifted body
        pure (Core.Let (Binding src0 f src1 type_' src2 replacement') body')
    goRoot other =
        Lens.Micro.traverseOf Core.subExpressions goRoot other

    cachedShift name = state $ \c ->
        case Map.lookup name c of
            Just shifted ->
                (shifted, c)
            Nothing ->
                let shifted = shiftCostShift name root
                in  (shifted, Map.insert name shifted c)

shiftCostFromRootEach
    :: ShiftCostResolved s a -> [Core.Expr s a] -> [Core.Expr s a]
shiftCostFromRootEach root =
    snd
        . mapAccumL
            (\cache expression ->
                let (expression', cache') = shiftCostFromRoot cache root expression
                in  (cache', expression')
            )
            mempty

large1Directory :: FilePath
large1Directory = "benchmark/evaluation/large1"

large1MainPath :: FilePath
large1MainPath = large1Directory </> "main.dhall"

large1Settings :: Dhall.InputSettings
large1Settings =
    Lens.Micro.set Dhall.sourceName large1MainPath
        (Lens.Micro.set Dhall.rootDirectory large1Directory Dhall.defaultInputSettings)

large2Directory :: FilePath
large2Directory = "benchmark/evaluation/large2"

large2MainPath :: FilePath
large2MainPath = large2Directory </> "main.dhall"

large3Directory :: FilePath
large3Directory = "benchmark/evaluation/large3"

large3PipelinePath :: FilePath
large3PipelinePath = large3Directory </> "pipeline.dhall"

large4Directory :: FilePath
large4Directory = "benchmark/evaluation/large4"

large4PipelinePath :: FilePath
large4PipelinePath = large4Directory </> "generate-example.dhall"

large4SourcePipelinePath :: FilePath
large4SourcePipelinePath = large4Directory </> "generate-example-source.dhall"

large5Directory :: FilePath
large5Directory = "benchmark/evaluation/large5"

large5CodePipelinePath :: FilePath
large5CodePipelinePath = large5Directory </> "pipeline-code.dhall"

large5SourcePipelinePath :: FilePath
large5SourcePipelinePath = large5Directory </> "pipeline-source.dhall"

large6Directory :: FilePath
large6Directory = "benchmark/evaluation/large6"

preludeImportDirectory :: FilePath
preludeImportDirectory = "benchmark/evaluation/prelude_import"

substitutionsDirectory :: FilePath
substitutionsDirectory = "benchmark/evaluation/substitutions"

k8sDirectory :: FilePath
k8sDirectory = "benchmark/evaluation/k8s"

say :: String -> IO ()
say msg = putStrLn msg >> hFlush stdout

formatDuration :: Double -> String
formatDuration seconds
    | seconds < 0.001 = printf "%.0f μs" (seconds * 1e6)
    | seconds < 1     = printf "%.1f ms" (seconds * 1e3)
    | otherwise       = printf "%.2f s" seconds

-- | Informational wall-clock timing for preparation steps (not a Criterion bench).
timed :: String -> IO a -> IO a
timed label action = do
    say $ "  " <> label <> "…"
    start <- getCurrentTime
    result <- action
    end <- getCurrentTime
    let seconds = realToFrac (diffUTCTime end start) :: Double
    say $ "  " <> label <> ": " <> formatDuration seconds
    pure result

-- | Resolve imports using the normal semantic / semisemantic disk caches.
-- Used for fixture preparation (Mode A) so broken fixtures fail early and
-- typecheck/evaluation benches have a resolved AST. See ../README.md.
resolveWithCache :: Dhall.InputSettings -> ParsedExpr -> IO ResolvedExpr
resolveWithCache settings parsed =
    Import.loadWithStatus
        ( Dhall.emptyStatusWithSettings
            (settings ^. Dhall.evaluateSettings)
            (settings ^. Dhall.rootDirectory)
        )
        Import.UseSemanticCache
        parsed

-- | Resolve imports with the semantic disk cache disabled (as with
-- @dhall --no-cache@). Semisemantic cache (@dhall-haskell-v2/@) is still read.
-- Used for Mode A timed @resolve@ benches.
resolveWithoutCache :: Dhall.InputSettings -> ParsedExpr -> IO ResolvedExpr
resolveWithoutCache settings parsed =
    Import.loadWithStatus
        ( Dhall.emptyStatusWithSettings
            (settings ^. Dhall.evaluateSettings)
            (settings ^. Dhall.rootDirectory)
        )
        Import.IgnoreSemanticCache
        parsed

-- | Mode B: resolve with caches on under a fresh @XDG_CACHE_HOME@ per sample.
resolveWithColdCache :: Dhall.InputSettings -> ParsedExpr -> IO ResolvedExpr
resolveWithColdCache settings parsed =
    withFreshCacheHome (resolveWithCache settings parsed)

-- | Mode B using the library substitution path ('Dhall.resolveWithSettings'),
-- matching @inputExprWithSettings@ rather than @Import.loadWithStatus@ alone.
resolveWithSettingsCold :: Dhall.InputSettings -> ParsedExpr -> IO ResolvedExpr
resolveWithSettingsCold settings parsed =
    withFreshCacheHome (Dhall.resolveWithSettings settings parsed)

-- | Mode D: cold library resolve, then typecheck and normalize under a fresh
-- cache. Synthetic substitution-heavy end-to-end path (import + typecheck + NF).
resolveTypecheckNormalizeCold
    :: Dhall.InputSettings -> ParsedExpr -> IO ResolvedExpr
resolveTypecheckNormalizeCold settings parsed =
    withFreshCacheHome $ do
        resolved <- Dhall.resolveWithSettings settings parsed
        _ <- either throw pure (TypeCheck.typeOf resolved)
        pure (Core.normalize resolved)

withFreshCacheHome :: IO a -> IO a
withFreshCacheHome action = do
    originalCacheHome <- lookupEnv "XDG_CACHE_HOME"

    Temp.withSystemTempDirectory "dhall-evaluation-bench" $ \cacheHome ->
        bracket
            (setEnv "XDG_CACHE_HOME" cacheHome)
            (\() -> restoreCacheHome originalCacheHome)
            (\() -> action)

restoreCacheHome :: Maybe String -> IO ()
restoreCacheHome =
    maybe (unsetEnv "XDG_CACHE_HOME") (setEnv "XDG_CACHE_HOME")

-- | Abort if the expression is ill-typed.  Used at load time so fixtures never
-- report OK timings when type-checking fails.
ensureWellTyped :: ResolvedExpr -> IO ()
ensureWellTyped expression =
    either throw (\_ -> pure ()) (TypeCheck.typeOf expression)

-- | Best-effort extract of @-p@ / @--pattern@ from the benchmark argv, so we can
-- skip loading fixtures that tasty-bench would filter out anyway.
patternFromArgs :: [String] -> Maybe String
patternFromArgs args =
    listToMaybe $ mapMaybe match (zip args (drop 1 args)) ++ naked
  where
    match ("-p", value)        = Just value
    match ("--pattern", value) = Just value
    match _                    = Nothing

    naked =
        [ drop (length prefix) arg
        | arg <- args
        , prefix <- ["--pattern=", "-p="]
        , prefix `isPrefixOf` arg
        ]

-- | Whether a tasty-style label path could match the user pattern (infix, like
-- tasty's default).  @Nothing@ means no pattern → load everything.
couldMatch :: Maybe String -> String -> Bool
couldMatch Nothing _ = True
couldMatch (Just pat) label =
    map toLower pat `isInfixOf` map toLower label

normalizeLabels :: String -> [String]
normalizeLabels name =
    [ "normalize." <> name
    , "normalize." <> name <> ".typecheck"
    , "normalize." <> name <> ".evaluation"
    ]

large1Labels :: [String]
large1Labels =
    [ "large1"
    , "large1.parse"
    , "large1.resolve"
    , "large1.typecheck"
    , "large1.evaluation"
    ]

large2Labels :: [String]
large2Labels =
    [ "large2"
    , "large2.normalize"
    , "large2.cbor.encode"
    , "large2.cbor.decode"
    ]

phaseLabels :: String -> [String]
phaseLabels prefix =
    [ prefix
    , prefix <> ".resolve"
    , prefix <> ".typecheck"
    , prefix <> ".evaluation"
    ]

-- | Bench name for Mode B cold resolve (see ../README.md).
coldResolveBenchName :: String
coldResolveBenchName = "resolve_cold_cache_on"

coldResolveLabels :: String -> [String]
coldResolveLabels prefix =
    [ prefix
    , prefix <> "." <> coldResolveBenchName
    ]

large3Labels :: [String]
large3Labels = phaseLabels "large3"

large3SourceLabels :: [String]
large3SourceLabels = phaseLabels "large3.source"

large3GetConfigLabels :: [String]
large3GetConfigLabels = phaseLabels "large3.get_config.code"

large3GetConfigAsSourceLabels :: [String]
large3GetConfigAsSourceLabels = phaseLabels "large3.get_config.source"

large4Labels :: [String]
large4Labels = phaseLabels "large4"

large4SourceLabels :: [String]
large4SourceLabels = phaseLabels "large4.source"

large5CodeLabels :: [String]
large5CodeLabels = phaseLabels "large5.code"

large5SourceLabels :: [String]
large5SourceLabels = phaseLabels "large5.source"

-- | Pattern labels for all large6 groups. Code eval/typecheck/normalize/multi
-- use coldResolveLabels; other variants use phaseLabels. Matrix in
-- large6/README.md.
large6Labels :: [String]
large6Labels =
    "large6"
        : concat
            [ phaseLabels "large6.slow_parse.as_code"
            , phaseLabels "large6.slow_parse.as_source"
            , coldResolveLabels "large6.slow_eval.as_code"
            , phaseLabels "large6.slow_eval.as_source"
            , coldResolveLabels "large6.slow_typecheck.as_code"
            , phaseLabels "large6.slow_typecheck.as_source"
            , coldResolveLabels "large6.slow_normalize.as_code"
            , phaseLabels "large6.slow_normalize.as_source"
            , coldResolveLabels "large6.slow_multi.as_code"
            , phaseLabels "large6.slow_multi.as_source"
            -- Structural-walk probe: large import-free List Natural.
            -- Measures whether as Source pays a second denote/walk after
            -- Code hash-check (should be near-parity after denoted reuse).
            , phaseLabels "large6.slow_walk.as_code"
            , phaseLabels "large6.slow_walk.as_source"
            ]

preludeImportCodeLabels :: [String]
preludeImportCodeLabels = coldResolveLabels "prelude_import.code"

preludeImportSourceLabels :: [String]
preludeImportSourceLabels = coldResolveLabels "prelude_import.source"

substitutionsCodeLabels :: [String]
substitutionsCodeLabels = coldResolveLabels "substitutions.as_code"

substitutionsSourceLabels :: [String]
substitutionsSourceLabels = coldResolveLabels "substitutions.as_source"

substitutionsManyFilesCodeLabels :: [String]
substitutionsManyFilesCodeLabels =
    coldResolveLabels "substitutions.many_files.as_code"

substitutionsManyFilesSourceLabels :: [String]
substitutionsManyFilesSourceLabels =
    coldResolveLabels "substitutions.many_files.as_source"

-- | Synthetic substitution-heavy end-to-end proxy: many fat imports, large
-- Haskell-API substitution map, cold resolve+typecheck+NF.
composerProxyEndToEndBenchName :: String
composerProxyEndToEndBenchName = "end_to_end_cold"

composerProxyCodeLabels :: [String]
composerProxyCodeLabels =
    [ "substitutions.composer_proxy.as_code"
    , "substitutions.composer_proxy.as_code." <> composerProxyEndToEndBenchName
    ]

composerProxySourceLabels :: [String]
composerProxySourceLabels =
    [ "substitutions.composer_proxy.as_source"
    , "substitutions.composer_proxy.as_source." <> composerProxyEndToEndBenchName
    ]

-- | Pure (1)+(2) probe: same map and @let a@ / @let x@ shape as many_files
-- modules, but no import/typecheck. Compare @naive@ (shift every value, no
-- root memo) vs @optimized@ (per-value shift + root-shift cache).
substitutionsShiftCostLabels :: [String]
substitutionsShiftCostLabels =
    [ "substitutions.shift_cost"
    , "substitutions.shift_cost.naive"
    , "substitutions.shift_cost.optimized"
    ]

-- | Pure probe for the semisemantic NF size check: full walk vs early abort
-- on a shared binary tree whose visit count exceeds the 64KiB threshold.
-- Implemented in this harness (not via Import internals) so the group compiles
-- before those functions exist. Existing import benches do not show this
-- (warm cache, tiny NFs, or TextLit).
nfSizeWalkLabels :: [String]
nfSizeWalkLabels =
    [ "semisemantic.nf_size_walk"
    , "semisemantic.nf_size_walk.full"
    , "semisemantic.nf_size_walk.early_abort"
    ]

-- | Haskell-API substitution map for the nested-let identity-path probe:
-- 100 type-like values whose names do not collide with the nested @xᵢ@
-- binders in @substitutions/package.dhall@, and whose bodies are closed.
--
-- This is *not* the many-import colliding-map shape. Use
-- 'manyCollidingSubstitutions' with @substitutions/many_files@ to see
-- as-code resolveSubstitutions cost.
manyUserSubstitutions :: Dhall.Substitution.Substitutions Parser.Src Void
manyUserSubstitutions =
    Dhall.Map.fromList
        [ (name, userTypeValue)
        | i <- [0 .. 99 :: Int]
        , let name = "UserType" <> Text.pack (printf "%03d" i)
        ]

-- | Shared injected type: large enough that @Syntax.shift@ on values is not
-- free, but closed so the identity fast path can still skip it when enabled.
userTypeValue :: Core.Expr Parser.Src Void
userTypeValue =
    Core.Lam
        mempty
        (Core.makeFunctionBinding "a" (Core.Const Core.Type))
        (Core.Record
            (Dhall.Map.fromList
                [ ("x", Core.makeRecordField (Core.Var "a"))
                , ("y", Core.makeRecordField Core.Natural)
                , ("z", Core.makeRecordField Core.Text)
                , ("w", Core.makeRecordField (Core.App Core.List Core.Natural))
                ]
            )
        )

withManyUserSubstitutions :: Dhall.InputSettings -> Dhall.InputSettings
withManyUserSubstitutions =
    Lens.Micro.set Dhall.substitutions manyUserSubstitutions

-- | Many-import colliding map: many keys, large values. Every 10th value has a
-- free @a@ (must shift at @let a@); the rest are closed (plan (1) must
-- skip them). Values are large so skipping / memoizing shift is visible;
-- the previous 5-field records were cheaper than tasty-bench noise.
manyCollidingSubstitutions :: Dhall.Substitution.Substitutions Parser.Src Void
manyCollidingSubstitutions =
    Dhall.Map.fromList
        [ (name, largeUserTypeValue (i `mod` 10 == 0))
        | i <- [0 .. manyFilesModuleCount - 1]
        , let name = "UserType" <> Text.pack (printf "%03d" i)
        ]

-- | ~64-field record. @True@: every field is free @a@ (collides with
-- module binders). @False@: closed (@Natural@), so @let a@ must not shift it.
largeUserTypeValue :: Bool -> Core.Expr Parser.Src Void
largeUserTypeValue mentionA =
    Core.Record
        (Dhall.Map.fromList
            [ (fieldName, Core.makeRecordField field)
            | i <- [0 .. largeUserTypeFieldCount - 1]
            , let fieldName = "f" <> Text.pack (printf "%03d" i)
                  field =
                      if mentionA
                          then Core.Var "a"
                          else Core.Natural
            ]
        )

largeUserTypeFieldCount :: Int
largeUserTypeFieldCount = 64

withManyCollidingSubstitutions :: Dhall.InputSettings -> Dhall.InputSettings
withManyCollidingSubstitutions =
    Lens.Micro.set Dhall.substitutions manyCollidingSubstitutions

-- | How many imported modules (and substitution keys) the many-files probe
-- uses. Must stay a many-import tree: a single large file would only call
-- @substitute@ a couple of times and miss the as-code regression.
manyFilesModuleCount :: Int
manyFilesModuleCount = 200

-- | Write the many-files package into @root@ (temp dir). Shared by the Code
-- and Source pipelines; not timed as part of @resolve_cold_cache_on@.
writeManyFilesFixture :: FilePath -> IO ()
writeManyFilesFixture root = do
    let modsDir = root </> "mods"
    Directory.createDirectoryIfMissing True modsDir
    mapM_ writeModule [0 .. manyFilesModuleCount - 1]
    Text.IO.writeFile (root </> "package.dhall") packageSource
    Text.IO.writeFile (root </> "pipeline-code.dhall") "./package.dhall\n"
    Text.IO.writeFile
        (root </> "pipeline-source.dhall")
        "./package.dhall as Source\n"
  where
    writeModule i = do
        let path = root </> "mods" </> printf "m%03d.dhall" i
        Text.IO.writeFile path $
            Text.unlines
                [ "let a = " <> Text.pack (show i)
                , "let x = 1"
                , "in  a + x"
                ]

    packageSource =
        let rows =
                [ "    ./mods/" <> Text.pack (printf "m%03d.dhall" (i :: Int))
                | i <- [0 .. manyFilesModuleCount - 1]
                ]
        in  "[\n" <> Text.intercalate ",\n" rows <> "\n]\n"

-- | Keep the generated tree alive through timed resolve samples, then delete it.
withOptionalManyFilesTree :: Bool -> (Maybe FilePath -> IO a) -> IO a
withOptionalManyFilesTree False k =
    k Nothing
withOptionalManyFilesTree True k =
    Temp.withSystemTempDirectory "dhall-substitutions-many-files" $ \dir -> do
        timed "substitutions.many_files: generate" (writeManyFilesFixture dir)
        k (Just dir)

-- | Module / key count for the composer_proxy fixture.
composerProxyModuleCount :: Int
composerProxyModuleCount = 400

-- | Fat record width so @Dhall.Map@ / denote / typecheck dominate over tiny
-- @let a = i@ modules.
composerProxyFieldCount :: Int
composerProxyFieldCount = 64

-- | Closed @UserType*@ map: large record *types* referenced by each module.
-- Values are closed so the fixture stays well-typed after substitution (unlike
-- @manyCollidingSubstitutions@, which injects free @a@ for shift-cost probes).
composerProxySubstitutions :: Dhall.Substitution.Substitutions Parser.Src Void
composerProxySubstitutions =
    Dhall.Map.fromList
        [ (name, composerProxyRecordType)
        | i <- [0 .. composerProxyModuleCount - 1]
        , let name = "UserType" <> Text.pack (printf "%03d" i)
        ]

composerProxyRecordType :: Core.Expr Parser.Src Void
composerProxyRecordType =
    Core.Record
        (Dhall.Map.fromList
            [ (fieldName, Core.makeRecordField Core.Natural)
            | i <- [0 .. composerProxyFieldCount - 1]
            , let fieldName = "f" <> Text.pack (printf "%03d" i)
            ]
        )

withComposerProxySubstitutions :: Dhall.InputSettings -> Dhall.InputSettings
withComposerProxySubstitutions =
    Lens.Micro.set Dhall.substitutions composerProxySubstitutions

-- | Write the composer_proxy package into @root@ (temp dir). Not timed.
--
-- Each module is a fat Natural record under a shared @UserType000@ binder so
-- the package is a well-typed homogeneous list. The Haskell-API map still has
-- one key per module (fingerprint / @resolveSubstitutions@ size).
writeComposerProxyFixture :: FilePath -> IO ()
writeComposerProxyFixture root = do
    let modsDir = root </> "mods"
    Directory.createDirectoryIfMissing True modsDir
    mapM_ writeModule [0 .. composerProxyModuleCount - 1]
    Text.IO.writeFile (root </> "package.dhall") packageSource
    Text.IO.writeFile (root </> "pipeline-code.dhall") "./package.dhall\n"
    Text.IO.writeFile
        (root </> "pipeline-source.dhall")
        "./package.dhall as Source\n"
  where
    writeModule i = do
        let path = root </> "mods" </> printf "m%03d.dhall" i
            -- Shared binder type so @package.dhall@ is a homogeneous list.
            fields =
                [ "  , f" <> Text.pack (printf "%03d" (j :: Int)) <> " = 0"
                | j <- [1 .. composerProxyFieldCount - 1]
                ]
            body =
                Text.unlines $
                    "\\(_ : UserType000) ->"
                        : ("  { f000 = " <> Text.pack (show i))
                        : fields
                        ++ ["  }"]
        Text.IO.writeFile path body

    packageSource =
        let rows =
                [ "    ./mods/" <> Text.pack (printf "m%03d.dhall" (i :: Int))
                | i <- [0 .. composerProxyModuleCount - 1]
                ]
        in  "[\n" <> Text.intercalate ",\n" rows <> "\n]\n"

withOptionalComposerProxyTree :: Bool -> (Maybe FilePath -> IO a) -> IO a
withOptionalComposerProxyTree False k =
    k Nothing
withOptionalComposerProxyTree True k =
    Temp.withSystemTempDirectory "dhall-substitutions-composer-proxy" $ \dir -> do
        timed "substitutions.composer_proxy: generate" (writeComposerProxyFixture dir)
        k (Just dir)

k8sLabels :: String -> [String]
k8sLabels name =
    [ "k8s." <> name
    , "k8s." <> name <> ".resolve"
    , "k8s." <> name <> ".typecheck"
    , "k8s." <> name <> ".evaluation"
    ]

loadExamples :: Maybe String -> IO [(String, ResolvedExpr)]
loadExamples mPattern = do
    files <- sort <$> Directory.listDirectory normalizeDirectory
    let paths =
            [ normalizeDirectory </> file
            | file <- files
            , ".dhall" `isSuffixOf` file
            , let name = takeBaseName file
            , any (couldMatch mPattern) (normalizeLabels name)
            ]
    if null paths
        then do
            say "Skipping normalize fixtures (do not match pattern)"
            pure []
        else do
            say $ "Loading normalize fixtures (" <> show (length paths) <> " file(s))…"
            traverse loadExample paths
  where
    normalizeDirectory = "benchmark/evaluation/normalize"

loadExample :: FilePath -> IO (String, ResolvedExpr)
loadExample path = do
    let name = takeBaseName path
    let prefix = "normalize/" <> name

    text <- timed (prefix <> ": read") (Text.IO.readFile path)

    parsed <- timed (prefix <> ": parse") $
        either throw pure (Parser.exprFromText path text)

    let settings =
            Lens.Micro.set Dhall.sourceName path
                (Lens.Micro.set Dhall.rootDirectory (takeDirectory path) Dhall.defaultInputSettings)

    resolved <- timed (prefix <> ": resolve (cache on)") $
        resolveWithCache settings parsed

    timed (prefix <> ": typecheck") (ensureWellTyped resolved)
    say $ "  " <> prefix <> ": ready"

    pure (name, resolved)

encodeNormalized :: ResolvedExpr -> ByteString.ByteString
encodeNormalized = Binary.encodeExpression . Core.denote

decodeNormalized :: ByteString.ByteString -> Core.Expr Void Void
decodeNormalized =
    either throw id . Binary.decodeExpression

data PipelineBench = PipelineBench
    { pbGroupLabel :: String
    , pbSettings :: Dhall.InputSettings
    , pbParsed :: ParsedExpr
    , pbResolved :: ResolvedExpr
    }
    -- ^ Mode A fixture: needs pre-resolved AST for typecheck/evaluation benches.

data ColdResolveBench = ColdResolveBench
    { crbGroupLabel :: String
    , crbSettings :: Dhall.InputSettings
    , crbParsed :: ParsedExpr
    }
    -- ^ Mode B fixture: parse-only prep; only cold resolve is measured.

pipelineSettings :: FilePath -> FilePath -> Dhall.InputSettings
pipelineSettings directory path =
    Lens.Micro.set Dhall.sourceName path
        (Lens.Micro.set Dhall.rootDirectory directory Dhall.defaultInputSettings)

-- | Mode A loader: parse, cache-warming resolve, validity typecheck.
loadPipelineBench :: String -> FilePath -> FilePath -> IO PipelineBench
loadPipelineBench groupLabel directory relativePath = do
    let path = directory </> relativePath
    let prefix = groupLabel

    text <- timed (prefix <> ": read") (Text.IO.readFile path)

    parsed <- timed (prefix <> ": parse") $
        either throw pure (Parser.exprFromText path text)

    let settings = pipelineSettings directory path

    resolved <- timed (prefix <> ": resolve (cache on)") $
        resolveWithCache settings parsed

    timed (prefix <> ": typecheck") (ensureWellTyped resolved)
    say $ "  " <> prefix <> ": ready"

    pure
        PipelineBench
            { pbGroupLabel = groupLabel
            , pbSettings = settings
            , pbParsed = parsed
            , pbResolved = resolved
            }

-- | Mode B loader: parse only (no cache-warming resolve).
loadColdResolveBench :: String -> FilePath -> FilePath -> IO ColdResolveBench
loadColdResolveBench groupLabel directory relativePath =
    loadColdResolveBenchWithSettings groupLabel directory relativePath id

loadColdResolveBenchWithSettings
    :: String
    -> FilePath
    -> FilePath
    -> (Dhall.InputSettings -> Dhall.InputSettings)
    -> IO ColdResolveBench
loadColdResolveBenchWithSettings groupLabel directory relativePath tweakSettings = do
    let path = directory </> relativePath
    let prefix = groupLabel

    text <- timed (prefix <> ": read") (Text.IO.readFile path)

    parsed <- timed (prefix <> ": parse") $
        either throw pure (Parser.exprFromText path text)

    let settings = tweakSettings (pipelineSettings directory path)

    say $ "  " <> prefix <> ": ready (explicit cold resolve benchmark)"

    pure
        ColdResolveBench
            { crbGroupLabel = groupLabel
            , crbSettings = settings
            , crbParsed = parsed
            }

loadLarge6PhaseVariants :: Maybe String -> IO [PipelineBench]
loadLarge6PhaseVariants mPattern = do
    -- Mode A large6 rows (see large6/README.md matrix).
    let candidates =
            [ ("large6.slow_parse.as_code", "pipeline-code-long-parse.dhall")
            , ("large6.slow_parse.as_source", "pipeline-source-long-parse.dhall")
            , ("large6.slow_eval.as_source", "pipeline-source-long-eval.dhall")
            , ("large6.slow_typecheck.as_source", "pipeline-source-long-typecheck.dhall")
            , ("large6.slow_normalize.as_source", "pipeline-source-long-normalize.dhall")
            , ("large6.slow_multi.as_source", "pipeline-source-long-multi.dhall")
            -- See large6Labels: second Source walk after Code validation.
            , ("large6.slow_walk.as_code", "pipeline-code-long-walk.dhall")
            , ("large6.slow_walk.as_source", "pipeline-source-long-walk.dhall")
            ]
        selected =
            [ entry
            | entry@(label, _) <- candidates
            , any (couldMatch mPattern) (phaseLabels label)
            ]
    if null selected
        then do
            say "Skipping large6 fixtures (do not match pattern)"
            pure []
        else do
            say $ "Loading large6 fixtures (" <> show (length selected) <> " file(s))…"
            traverse (\(label, file) -> loadPipelineBench label large6Directory file) selected

loadLarge6ColdResolveVariants :: Maybe String -> IO [ColdResolveBench]
loadLarge6ColdResolveVariants mPattern = do
    -- Mode B large6 Code rows where prep would hide resolve cost.
    let candidates =
            [ ("large6.slow_eval.as_code", "pipeline-code-long-eval.dhall")
            , ("large6.slow_typecheck.as_code", "pipeline-code-long-typecheck.dhall")
            , ("large6.slow_normalize.as_code", "pipeline-code-long-normalize.dhall")
            , ("large6.slow_multi.as_code", "pipeline-code-long-multi.dhall")
            ]
        selected =
            [ entry
            | entry@(label, _) <- candidates
            , any (couldMatch mPattern) (coldResolveLabels label)
            ]
    if null selected
        then do
            say "Skipping large6 cold-resolve fixtures (do not match pattern)"
            pure []
        else do
            say $ "Loading large6 cold-resolve fixtures (" <> show (length selected) <> " file(s))…"
            traverse (\(label, file) -> loadColdResolveBench label large6Directory file) selected

-- | Load large1 inputs for the existing per-phase benchmarks.
--
-- Import resolution for prep uses 'resolveWithCache'. The timed @resolve@
-- bench re-runs resolution with 'resolveWithoutCache' via 'nfAppIO'.
loadLarge1 :: IO (Text, ParsedExpr, ResolvedExpr)
loadLarge1 = do
    say "Loading large1…"

    text <- timed "large1: read" (Text.IO.readFile large1MainPath)

    parsed <- timed "large1: parse" $
        either throw pure (Parser.exprFromText large1MainPath text)

    resolved <- timed "large1: resolve (cache on)" $
        resolveWithCache large1Settings parsed

    timed "large1: typecheck" (ensureWellTyped resolved)

    say "  large1: ready"

    pure (text, parsed, resolved)

-- | Load large2 for CBOR benchmarking: parse, resolve, typecheck, then
-- pre-encode once to obtain decode input and report CBOR size.
loadLarge2 :: IO (ResolvedExpr, ResolvedExpr, ByteString.ByteString)
loadLarge2 = do
    say "Loading large2…"

    text <- timed "large2: read" (Text.IO.readFile large2MainPath)

    parsed <- timed "large2: parse" $
        either throw pure (Parser.exprFromText large2MainPath text)

    resolved <- timed "large2: resolve (assert no imports)" $
        Import.assertNoImports parsed

    timed "large2: typecheck" (ensureWellTyped resolved)

    normalized <- timed "large2: normalize (for encode/decode fixture)" $
        pure (Core.normalize resolved)

    encoded <- timed "large2: encode CBOR (for decode fixture)" $
        pure (encodeNormalized normalized)

    let cborBytes = ByteString.length encoded
    say $ "  large2: CBOR size: " <> show cborBytes <> " bytes"

    say "  large2: ready"

    pure (resolved, normalized, encoded)

k8sSettings :: FilePath -> Dhall.InputSettings
k8sSettings sourceName =
    Lens.Micro.set Dhall.sourceName sourceName
        (Lens.Micro.set Dhall.rootDirectory k8sDirectory Dhall.defaultInputSettings)

loadK8sExample :: (String, String) -> IO (String, Dhall.InputSettings, ParsedExpr, ResolvedExpr)
loadK8sExample (name, expressionText) = do
    let sourceName = k8sDirectory </> name <> ".dhall"
    let settings = k8sSettings sourceName
    let prefix = "k8s/" <> name

    parsed <- timed (prefix <> ": parse") $
        either throw pure (Parser.exprFromText sourceName (Text.pack expressionText))

    resolved <- timed (prefix <> ": resolve (cache on)") $
        resolveWithCache settings parsed

    timed (prefix <> ": typecheck") (ensureWellTyped resolved)
    say $ "  " <> prefix <> ": ready"

    pure (name, settings, parsed, resolved)

loadK8sExamples :: Maybe String -> IO [(String, Dhall.InputSettings, ParsedExpr, ResolvedExpr)]
loadK8sExamples mPattern = do
    let candidates =
            [ ( "file3", "(./file3.dhall).mkPod" )
            , ( "file4", "(./file4.dhall).mkPod" )
            ]
        selected =
            [ entry
            | entry@(name, _) <- candidates
            , any (couldMatch mPattern) (k8sLabels name)
            ]
    if null selected
        then do
            say "Skipping k8s fixtures (do not match pattern)"
            pure []
        else do
            say $ "Loading k8s fixtures (" <> show (length selected) <> " file(s))…"
            traverse loadK8sExample selected

-- Prep uses disk caches for Mode A validity + phase benches; Mode B fixtures
-- skip cache-warming resolve. See benchmark/evaluation/README.md.
main :: IO ()
main = do
    args <- getArgs
    let mPattern = patternFromArgs args
    case mPattern of
        Nothing  -> say "Preparing benchmarks (loading fixtures; prep cache on)…"
        Just pat -> say $ "Preparing benchmarks matching " <> show pat <> " (prep cache on)…"

    examples <- loadExamples mPattern

    let wantLarge1 = any (couldMatch mPattern) large1Labels
    large1 <-
        if wantLarge1
            then Just <$> loadLarge1
            else do
                say "Skipping large1 (does not match pattern)"
                pure Nothing

    let wantLarge2 = any (couldMatch mPattern) large2Labels
    large2 <-
        if wantLarge2
            then Just <$> loadLarge2
            else do
                say "Skipping large2 (does not match pattern)"
                pure Nothing

    k8sExamples <- loadK8sExamples mPattern

    let wantLarge3 = any (couldMatch mPattern) large3Labels
    large3 <-
        if wantLarge3
            then Just <$> loadPipelineBench "large3" large3Directory "pipeline.dhall"
            else do
                say "Skipping large3 (does not match pattern)"
                pure Nothing

    let wantLarge3Source = any (couldMatch mPattern) large3SourceLabels
    large3Source <-
        if wantLarge3Source
            then
                Just
                    <$> loadPipelineBench
                        "large3.source"
                        large3Directory
                        "pipeline-source.dhall"
            else do
                say "Skipping large3.source (does not match pattern)"
                pure Nothing

    let wantLarge3GetConfig = any (couldMatch mPattern) large3GetConfigLabels
    large3GetConfig <-
        if wantLarge3GetConfig
            then
                Just
                    <$> loadPipelineBench
                        "large3.get_config.code"
                        large3Directory
                        "get_config.dhall"
            else do
                say "Skipping large3.get_config.code (does not match pattern)"
                pure Nothing

    let wantLarge3GetConfigAsSource =
            any (couldMatch mPattern) large3GetConfigAsSourceLabels
    large3GetConfigAsSource <-
        if wantLarge3GetConfigAsSource
            then
                Just
                    <$> loadPipelineBench
                        "large3.get_config.source"
                        large3Directory
                        "get_config_as_source.dhall"
            else do
                say "Skipping large3.get_config.source (does not match pattern)"
                pure Nothing

    let wantLarge4 = any (couldMatch mPattern) large4Labels
    large4 <-
        if wantLarge4
            then Just <$> loadPipelineBench "large4" large4Directory "generate-example.dhall"
            else do
                say "Skipping large4 (does not match pattern)"
                pure Nothing

    let wantLarge4Source = any (couldMatch mPattern) large4SourceLabels
    large4Source <-
        if wantLarge4Source
            then
                Just
                    <$> loadPipelineBench
                        "large4.source"
                        large4Directory
                        "generate-example-source.dhall"
            else do
                say "Skipping large4.source (does not match pattern)"
                pure Nothing

    let wantLarge5Code = any (couldMatch mPattern) large5CodeLabels
    large5Code <-
        if wantLarge5Code
            then
                Just
                    <$> loadPipelineBench
                        "large5.code"
                        large5Directory
                        "pipeline-code.dhall"
            else do
                say "Skipping large5.code (does not match pattern)"
                pure Nothing

    let wantLarge5Source = any (couldMatch mPattern) large5SourceLabels
    large5Source <-
        if wantLarge5Source
            then
                Just
                    <$> loadPipelineBench
                        "large5.source"
                        large5Directory
                        "pipeline-source.dhall"
            else do
                say "Skipping large5.source (does not match pattern)"
                pure Nothing

    large6Variants <- loadLarge6PhaseVariants mPattern
    large6ColdResolveVariants <- loadLarge6ColdResolveVariants mPattern

    let wantPreludeImportCode = any (couldMatch mPattern) preludeImportCodeLabels
    preludeImportCode <-
        if wantPreludeImportCode
            then
                Just
                    <$> loadColdResolveBench
                        "prelude_import.code"
                        preludeImportDirectory
                        "prelude-code.dhall"
            else do
                say "Skipping prelude_import.code (does not match pattern)"
                pure Nothing

    let wantPreludeImportSource = any (couldMatch mPattern) preludeImportSourceLabels
    preludeImportSource <-
        if wantPreludeImportSource
            then
                Just
                    <$> loadColdResolveBench
                        "prelude_import.source"
                        preludeImportDirectory
                        "prelude-source.dhall"
            else do
                say "Skipping prelude_import.source (does not match pattern)"
                pure Nothing

    let wantSubstitutionsCode = any (couldMatch mPattern) substitutionsCodeLabels
    substitutionsCode <-
        if wantSubstitutionsCode
            then
                Just
                    <$> loadColdResolveBenchWithSettings
                        "substitutions.as_code"
                        substitutionsDirectory
                        "pipeline-code.dhall"
                        withManyUserSubstitutions
            else do
                say "Skipping substitutions.as_code (does not match pattern)"
                pure Nothing

    let wantSubstitutionsSource = any (couldMatch mPattern) substitutionsSourceLabels
    substitutionsSource <-
        if wantSubstitutionsSource
            then
                Just
                    <$> loadColdResolveBenchWithSettings
                        "substitutions.as_source"
                        substitutionsDirectory
                        "pipeline-source.dhall"
                        withManyUserSubstitutions
            else do
                say "Skipping substitutions.as_source (does not match pattern)"
                pure Nothing

    let wantSubstitutionsManyFilesCode =
            any (couldMatch mPattern) substitutionsManyFilesCodeLabels
    let wantSubstitutionsManyFilesSource =
            any (couldMatch mPattern) substitutionsManyFilesSourceLabels
    let wantComposerProxyCode =
            any (couldMatch mPattern) composerProxyCodeLabels
    let wantComposerProxySource =
            any (couldMatch mPattern) composerProxySourceLabels
    let wantShiftCost =
            any (couldMatch mPattern) substitutionsShiftCostLabels
    let wantNfSizeWalk =
            any (couldMatch mPattern) nfSizeWalkLabels

    withOptionalManyFilesTree
        (wantSubstitutionsManyFilesCode || wantSubstitutionsManyFilesSource)
        $ \manyFilesRoot ->
            withOptionalComposerProxyTree
                (wantComposerProxyCode || wantComposerProxySource)
                $ \composerProxyRoot -> do
                    substitutionsManyFilesCode <-
                        case (wantSubstitutionsManyFilesCode, manyFilesRoot) of
                            (True, Just dir) ->
                                Just
                                    <$> loadColdResolveBenchWithSettings
                                        "substitutions.many_files.as_code"
                                        dir
                                        "pipeline-code.dhall"
                                        withManyCollidingSubstitutions
                            _ -> do
                                say "Skipping substitutions.many_files.as_code (does not match pattern)"
                                pure Nothing

                    substitutionsManyFilesSource <-
                        case (wantSubstitutionsManyFilesSource, manyFilesRoot) of
                            (True, Just dir) ->
                                Just
                                    <$> loadColdResolveBenchWithSettings
                                        "substitutions.many_files.as_source"
                                        dir
                                        "pipeline-source.dhall"
                                        withManyCollidingSubstitutions
                            _ -> do
                                say "Skipping substitutions.many_files.as_source (does not match pattern)"
                                pure Nothing

                    composerProxyCode <-
                        case (wantComposerProxyCode, composerProxyRoot) of
                            (True, Just dir) ->
                                Just
                                    <$> loadColdResolveBenchWithSettings
                                        "substitutions.composer_proxy.as_code"
                                        dir
                                        "pipeline-code.dhall"
                                        withComposerProxySubstitutions
                            _ -> do
                                say "Skipping substitutions.composer_proxy.as_code (does not match pattern)"
                                pure Nothing

                    composerProxySource <-
                        case (wantComposerProxySource, composerProxyRoot) of
                            (True, Just dir) ->
                                Just
                                    <$> loadColdResolveBenchWithSettings
                                        "substitutions.composer_proxy.as_source"
                                        dir
                                        "pipeline-source.dhall"
                                        withComposerProxySubstitutions
                            _ -> do
                                say "Skipping substitutions.composer_proxy.as_source (does not match pattern)"
                                pure Nothing

                    say "Starting tasty-bench…"

                    defaultMain $ concat
                        [ [ bgroup
                              "normalize"
                              [ bgroup
                                  name
                                  [ bench "typecheck" (nf typecheckResolvedExpr expression)
                                  , bench "evaluation" (nf normalizeResolvedExpr expression)
                                  ]
                              | (name, expression) <- examples
                              ]
                          ]
                        , [ bgroup "large1"
                              [ bench "parse" (nf (parseLarge1 large1MainPath) large1Text)
                              , bench "resolve" (nfAppIO (resolveWithoutCache large1Settings) large1Parsed)
                              , bench "typecheck" (nf typecheckResolvedExpr large1Resolved)
                              , bench "evaluation" (nf normalizeResolvedExpr large1Resolved)
                              ]
                          | Just (large1Text, large1Parsed, large1Resolved) <- [large1]
                          ]
                        , [ bgroup "large2"
                              [ bench "normalize" (nf normalizeResolvedExpr large2Resolved)
                              , bgroup
                                    "cbor"
                                    [ bench "encode" (nf encodeNormalized large2Normalized)
                                    , bench "decode" (nf decodeNormalized large2Encoded)
                                    ]
                              ]
                          | Just (large2Resolved, large2Normalized, large2Encoded) <- [large2]
                          ]
                        , [ bgroup
                              "k8s"
                              [ bgroup
                                  name
                                  [ bench "resolve" (nfAppIO (resolveWithoutCache settings) parsed)
                                  , bench "typecheck" (nf typecheckResolvedExpr resolved)
                                  , bench "evaluation" (nf normalizeResolvedExpr resolved)
                                  ]
                              | (name, settings, parsed, resolved) <- k8sExamples
                              ]
                          | not (null k8sExamples)
                          ]
                        , [ pipelineBenchGroup large3Bench
                          | Just large3Bench <- [large3]
                          ]
                        , [ pipelineBenchGroup large3SourceBench
                          | Just large3SourceBench <- [large3Source]
                          ]
                        , [ pipelineBenchGroup large3GetConfigBench
                          | Just large3GetConfigBench <- [large3GetConfig]
                          ]
                        , [ pipelineBenchGroup large3GetConfigAsSourceBench
                          | Just large3GetConfigAsSourceBench <- [large3GetConfigAsSource]
                          ]
                        , [ pipelineBenchGroup large4Bench
                          | Just large4Bench <- [large4]
                          ]
                        , [ pipelineBenchGroup large4SourceBench
                          | Just large4SourceBench <- [large4Source]
                          ]
                        , [ pipelineBenchGroup large5CodeBench
                          | Just large5CodeBench <- [large5Code]
                          ]
                        , [ pipelineBenchGroup large5SourceBench
                          | Just large5SourceBench <- [large5Source]
                          ]
                        , map pipelineBenchGroup large6Variants
                        , map coldResolveBenchGroup large6ColdResolveVariants
                        , [ coldResolveBenchGroup preludeImportCodeBench
                          | Just preludeImportCodeBench <- [preludeImportCode]
                          ]
                        , [ coldResolveBenchGroup preludeImportSourceBench
                          | Just preludeImportSourceBench <- [preludeImportSource]
                          ]
                        , [ substitutionsColdResolveBenchGroup substitutionsCodeBench
                          | Just substitutionsCodeBench <- [substitutionsCode]
                          ]
                        , [ substitutionsColdResolveBenchGroup substitutionsSourceBench
                          | Just substitutionsSourceBench <- [substitutionsSource]
                          ]
                        , [ substitutionsColdResolveBenchGroup substitutionsManyFilesCodeBench
                          | Just substitutionsManyFilesCodeBench <- [substitutionsManyFilesCode]
                          ]
                        , [ substitutionsColdResolveBenchGroup substitutionsManyFilesSourceBench
                          | Just substitutionsManyFilesSourceBench <- [substitutionsManyFilesSource]
                          ]
                        , [ composerProxyEndToEndBenchGroup composerProxyCodeBench
                          | Just composerProxyCodeBench <- [composerProxyCode]
                          ]
                        , [ composerProxyEndToEndBenchGroup composerProxySourceBench
                          | Just composerProxySourceBench <- [composerProxySource]
                          ]
                        , [ shiftCostBenchGroup | wantShiftCost ]
                        , [ nfSizeWalkBenchGroup | wantNfSizeWalk ]
                        ]
 where
   -- These helpers reduce polymorphism in TypeCheck.typeOf and Core.normalize.
   -- Type-check failures must throw so tasty-bench reports FAIL instead of OK.
   typecheckResolvedExpr :: ResolvedExpr -> Core.Expr Parser.Src Void
   typecheckResolvedExpr = either throw id . TypeCheck.typeOf

   -- Pure NbE; disk caches are irrelevant here. Ill-typed fixtures are rejected
   -- at load time via 'ensureWellTyped'.
   normalizeResolvedExpr :: ResolvedExpr -> ResolvedExpr
   normalizeResolvedExpr = Core.normalize

   pipelineBenchGroup :: PipelineBench -> Benchmark
   pipelineBenchGroup fixture =
       bgroup (pbGroupLabel fixture)
           [ bench "resolve" (nfAppIO (resolveWithoutCache (pbSettings fixture)) (pbParsed fixture))
           , bench "typecheck" (nf typecheckResolvedExpr (pbResolved fixture))
           , bench "evaluation" (nf normalizeResolvedExpr (pbResolved fixture))
           ]

   coldResolveBenchGroup :: ColdResolveBench -> Benchmark
   coldResolveBenchGroup fixture =
       bgroup (crbGroupLabel fixture)
           [ bench coldResolveBenchName
               (nfAppIO (resolveWithColdCache (crbSettings fixture)) (crbParsed fixture))
           ]

   substitutionsColdResolveBenchGroup :: ColdResolveBench -> Benchmark
   substitutionsColdResolveBenchGroup fixture =
       bgroup (crbGroupLabel fixture)
           [ bench coldResolveBenchName
               (nfAppIO (resolveWithSettingsCold (crbSettings fixture)) (crbParsed fixture))
           ]

   composerProxyEndToEndBenchGroup :: ColdResolveBench -> Benchmark
   composerProxyEndToEndBenchGroup fixture =
       bgroup (crbGroupLabel fixture)
           [ bench composerProxyEndToEndBenchName
               (nfAppIO
                   (resolveTypecheckNormalizeCold (crbSettings fixture))
                   (crbParsed fixture)
               )
           ]

   -- | Same substitution map and @let a@/@let x@ shape as many_files modules,
   -- without import I/O. Resolves the map once. @naive@ is the pre-(1)/(2)
   -- walker (@Map.map shift@ every value, no root memo); @optimized@ is
   -- @substituteManyFromRoot@ (per-value shift + root-shift memo).
   shiftCostBenchGroup :: Benchmark
   shiftCostBenchGroup =
       bgroup "substitutions.shift_cost"
           [ bench "naive" (nf (map (shiftCostNaiveWalk resolved)) exprs)
           , bench "optimized" (nf (shiftCostFromRootEach resolved) exprs)
           ]
     where
       resolved =
           resolveShiftCost manyCollidingSubstitutions

       exprs =
           replicate manyFilesModuleCount shiftCostExpr

       shiftCostExpr :: ResolvedExpr
       shiftCostExpr =
           Core.Let
               (Core.makeBinding "a" (Core.NaturalLit 0))
               (Core.Let
                   (Core.makeBinding "x" (Core.NaturalLit 1))
                   (Core.NaturalPlus (Core.Var "a") (Core.Var "x"))
               )

   -- | @full@ walks the whole tree; @early_abort@ stops at 64KiB. Both live in
   -- this harness so the group does not depend on Import internals.
   nfSizeWalkBenchGroup :: Benchmark
   nfSizeWalkBenchGroup =
       bgroup "semisemantic.nf_size_walk"
           [ bench "full"
               (nf (nfSizeWalkEstimate . sharedNaturalPlusTree) nfSizeWalkDepth)
           , bench "early_abort"
               (nf (nfSizeWalkExceedsThreshold . sharedNaturalPlusTree) nfSizeWalkDepth)
           ]

   parseLarge1 :: FilePath -> Text -> ParsedExpr
   parseLarge1 path text =
       either throw id (Parser.exprFromText path text)
