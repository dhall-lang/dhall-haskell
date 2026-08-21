{-# LANGUAGE OverloadedStrings #-}
-- | Substitution probes: nested-let, many_files, composer_proxy, shift_cost.
module Bench.Substitutions
    ( benchmarks
    ) where

import Control.Monad.Trans.State.Strict (runState, state)
import Data.List (mapAccumL)
import Data.Text (Text)
import Data.Void (Void)
import Text.Printf (printf)
import Test.Tasty.Bench

import Bench.Common
    ( ResolvedExpr
    , couldMatch
    , coldResolveLabels
    , endToEndColdBenchGroup
    , endToEndColdBenchName
    , loadColdResolveBenchWithSettings
    , say
    , substitutionsColdResolveBenchGroup
    , timed
    )

import Dhall.Core (Binding (..), FunctionBinding (..), Var (..))

import qualified Data.Foldable.WithIndex as Foldable.WithIndex
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Dhall
import qualified Dhall.Core as Core
import qualified Dhall.Map
import qualified Dhall.Parser as Parser
import qualified Dhall.Substitution
import qualified Lens.Micro
import qualified System.Directory as Directory
import qualified System.IO.Temp as Temp
import System.FilePath ((</>))

substitutionsDirectory :: FilePath
substitutionsDirectory = "benchmark/evaluation/substitutions"

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

-- | Synthetic substitution-heavy end-to-end proxy: many wide-record imports,
-- large Haskell-API substitution map, cold resolve+typecheck+NF.
composerProxyCodeLabels :: [String]
composerProxyCodeLabels =
    [ "substitutions.composer_proxy.as_code"
    , "substitutions.composer_proxy.as_code." <> endToEndColdBenchName
    ]

composerProxySourceLabels :: [String]
composerProxySourceLabels =
    [ "substitutions.composer_proxy.as_source"
    , "substitutions.composer_proxy.as_source." <> endToEndColdBenchName
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

-- | How many @let a@/@let x@ expressions @substitutions.shift_cost@ walks.
-- Independent of @manyFilesModuleCount@ so import benches stay unchanged.
-- ~20× the old 200-expr probe (~0.7 ms naive) → a few milliseconds.
shiftCostExprCount :: Int
shiftCostExprCount = 4000

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
--
-- The bracket must wrap the Criterion run, not only construction of the
-- 'Benchmark' list: Mode B samples import @package.dhall@ from this directory
-- after prep returns.
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

-- | Record field count so @Dhall.Map@ / denote / typecheck / NF node count
-- dominate over tiny @let a = i@ modules. Source text stays short to parse.
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
-- Each module is a wide Natural record (many fields) under a shared
-- @UserType000@ binder so the package is a well-typed homogeneous list. The
-- Haskell-API map still has one key per module (fingerprint /
-- @resolveSubstitutions@ size).
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

-- Same lifetime rule as 'withOptionalManyFilesTree': Mode D samples import
-- from this directory after prep returns.
withOptionalComposerProxyTree :: Bool -> (Maybe FilePath -> IO a) -> IO a
withOptionalComposerProxyTree False k =
    k Nothing
withOptionalComposerProxyTree True k =
    Temp.withSystemTempDirectory "dhall-substitutions-composer-proxy" $ \dir -> do
        timed "substitutions.composer_proxy: generate" (writeComposerProxyFixture dir)
        k (Just dir)


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
        replicate shiftCostExprCount shiftCostExpr

    shiftCostExpr :: ResolvedExpr
    shiftCostExpr =
        Core.Let
            (Core.makeBinding "a" (Core.NaturalLit 0))
            (Core.Let
                (Core.makeBinding "x" (Core.NaturalLit 1))
                (Core.NaturalPlus (Core.Var "a") (Core.Var "x"))
            )

-- | Build substitution benches. The continuation runs while generated
-- many_files / composer_proxy trees still exist (see 'withOptionalManyFilesTree').
benchmarks :: Maybe String -> ([Benchmark] -> IO a) -> IO a
benchmarks mPattern k = do
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

                    k $ concat
                        [ [ substitutionsColdResolveBenchGroup fixture
                          | Just fixture <- [substitutionsCode]
                          ]
                        , [ substitutionsColdResolveBenchGroup fixture
                          | Just fixture <- [substitutionsSource]
                          ]
                        , [ substitutionsColdResolveBenchGroup fixture
                          | Just fixture <- [substitutionsManyFilesCode]
                          ]
                        , [ substitutionsColdResolveBenchGroup fixture
                          | Just fixture <- [substitutionsManyFilesSource]
                          ]
                        , [ endToEndColdBenchGroup fixture
                          | Just fixture <- [composerProxyCode]
                          ]
                        , [ endToEndColdBenchGroup fixture
                          | Just fixture <- [composerProxySource]
                          ]
                        , [ shiftCostBenchGroup | wantShiftCost ]
                        ]
