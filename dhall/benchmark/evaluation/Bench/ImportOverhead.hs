{-# LANGUAGE OverloadedStrings #-}
-- | Import-resolution overhead fixtures.
--
-- Generated trees are built during prep. The projection / force-all pair is
-- rewritten under @import_overhead/projection@ on every run, including when
-- those files are already present. That directory is not checked in. The CLI
-- timings in its README use the same path.
module Bench.ImportOverhead
    ( benchmarks
    ) where

import Control.Exception (throw)
import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Text.Printf (printf)
import Test.Tasty.Bench

import Bench.Common

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Dhall
import qualified Dhall.Context
import qualified Dhall.Core as Core
import qualified Dhall.Import as Import
import qualified Dhall.Map
import qualified Dhall.Parser as Parser
import qualified Lens.Micro
import qualified System.Directory as Directory
import qualified System.IO.Temp as Temp
import System.FilePath ((</>))

customNormalizerCount :: Int
customNormalizerCount = 700

importAltCount :: Int
importAltCount = 300

contextBinderCount :: Int
contextBinderCount = 100

contextFileCount :: Int
contextFileCount = 370

projectionCount :: Int
projectionCount = 200

projectionDirectory :: FilePath
projectionDirectory = "benchmark/evaluation/import_overhead/projection"

allLabels :: [String]
allLabels =
    [ "import_overhead.custom_normalizer.cold.default"
    , "import_overhead.custom_normalizer.cold.custom"
    , "import_overhead.custom_normalizer.warm.default"
    , "import_overhead.custom_normalizer.warm.custom"
    , "import_overhead.import_alt.cold"
    , "import_overhead.import_alt.warm"
    , "import_overhead.import_alt.typecheck"
    , "import_overhead.root_substitutions.resolve"
    , "import_overhead.starting_context.cold"
    , "import_overhead.starting_context.warm"
    , "import_overhead.projection." <> endToEndColdBenchName
    , "import_overhead.force_all." <> endToEndColdBenchName
    ]

-- | Build import-overhead benches. Generated import trees remain until the
-- continuation returns.
benchmarks :: Maybe String -> ([Benchmark] -> IO a) -> IO a
benchmarks mPattern k
    | not (any (couldMatch mPattern) allLabels) = do
        say "Skipping import_overhead (does not match pattern)"
        k []
    | otherwise =
        Temp.withSystemTempDirectory "dhall-import-overhead" $ \root -> do
            custom <- customNormalizerBenches mPattern (root </> "custom-normalizer")
            alt <- importAltBenches mPattern (root </> "import-alt")
            subst <- pure (rootSubstitutionBenches mPattern)
            context <- startingContextBenches mPattern (root </> "starting-context")
            projection <- projectionBenches mPattern
            k (concat [custom, alt, subst, context, projection])

customNormalizerBenches :: Maybe String -> FilePath -> IO [Benchmark]
customNormalizerBenches mPattern dir
    | not (any (active mPattern) customLabels) =
        pure []
    | otherwise = do
        timed "import_overhead.custom_normalizer: generate" $
            writeCustomNormalizer dir
        (settings, parsed) <- loadMain dir "main.dhall"
        let customSettings = Lens.Micro.set Dhall.normalizer (Just zeroToSeven) settings
        warmDefault <- warmIf (active mPattern customWarmDefault) dir "default" settings parsed
        warmCustom <- warmIf (active mPattern customWarmCustom) dir "custom" customSettings parsed
        let cold =
                [ bench "default"
                    (nfAppIO (resolveTypecheckNormalizeCold settings) parsed)
                | active mPattern customColdDefault
                ]
                    ++
                [ bench "custom"
                    (nfAppIO (resolveTypecheckNormalizeCold customSettings) parsed)
                | active mPattern customColdCustom
                ]
            warm =
                [ bench "default"
                    (nfAppIO (resolveTypecheckNormalizeAtCache cache settings) parsed)
                | active mPattern customWarmDefault
                , Just cache <- [warmDefault]
                ]
                    ++
                [ bench "custom"
                    (nfAppIO (resolveTypecheckNormalizeAtCache cache customSettings) parsed)
                | active mPattern customWarmCustom
                , Just cache <- [warmCustom]
                ]
        pure $
            [ bgroup "import_overhead.custom_normalizer" $
                concat
                    [ [ bgroup "cold" cold | not (null cold) ]
                    , [ bgroup "warm" warm | not (null warm) ]
                    ]
            ]
  where
    customLabels =
        [ customColdDefault
        , customColdCustom
        , customWarmDefault
        , customWarmCustom
        ]
    customColdDefault = "import_overhead.custom_normalizer.cold.default"
    customColdCustom = "import_overhead.custom_normalizer.cold.custom"
    customWarmDefault = "import_overhead.custom_normalizer.warm.default"
    customWarmCustom = "import_overhead.custom_normalizer.warm.custom"

-- | Rewrite a literal @0@ to @7@. This is the custom-normalizer path, which
-- normalizes the inlined tree instead of the shared one.
zeroToSeven :: Core.ReifiedNormalizer Void
zeroToSeven = Core.ReifiedNormalizer rewrite
  where
    rewrite (Core.NaturalLit 0) = pure (Just (Core.NaturalLit 7))
    rewrite _ = pure Nothing

writeCustomNormalizer :: FilePath -> IO ()
writeCustomNormalizer root = do
    Directory.createDirectoryIfMissing True root
    mapM_ writeOne [0 .. customNormalizerCount - 1]
    Text.IO.writeFile (root </> "main.dhall") mainSource
  where
    writeOne i =
        Text.IO.writeFile (root </> ("n" <> Text.unpack (pad3 i) <> ".dhall")) "0\n"

    mainSource =
        "[ "
            <> Text.intercalate "\n, "
                [ "{ a = ./n" <> pad3 i <> ".dhall, b = ./n" <> pad3 i <> ".dhall }"
                | i <- [0 .. customNormalizerCount - 1]
                ]
            <> "\n]\n"

importAltBenches :: Maybe String -> FilePath -> IO [Benchmark]
importAltBenches mPattern dir
    | not (any (active mPattern) altLabels) =
        pure []
    | otherwise = do
        timed "import_overhead.import_alt: generate" $
            writeImportAlt dir
        (settings, parsed) <- loadMain dir "main.dhall"
        warmCache <- warmIf (active mPattern altWarm) dir "import_alt" settings parsed
        resolved <-
            if active mPattern altTypecheck
                then fmap Just $
                    timed "import_overhead.import_alt: resolve for typecheck" $
                        resolveWithCache settings parsed
                else pure Nothing
        let cold =
                [ bench "cold"
                    (nfAppIO (resolveTypecheckNormalizeCold settings) parsed)
                | active mPattern altCold
                ]
            warm =
                [ bench "warm"
                    (nfAppIO (resolveTypecheckNormalizeAtCache cache settings) parsed)
                | active mPattern altWarm
                , Just cache <- [warmCache]
                ]
            typecheck =
                [ bench "typecheck" (nf typecheckResolvedExpr expr)
                | active mPattern altTypecheck
                , Just expr <- [resolved]
                ]
        pure [bgroup "import_overhead.import_alt" (cold ++ warm ++ typecheck)]
  where
    altLabels = [altCold, altWarm, altTypecheck]
    altCold = "import_overhead.import_alt.cold"
    altWarm = "import_overhead.import_alt.warm"
    altTypecheck = "import_overhead.import_alt.typecheck"

writeImportAlt :: FilePath -> IO ()
writeImportAlt root = do
    Directory.createDirectoryIfMissing True root
    Text.IO.writeFile (root </> "real.dhall") "0\n"
    mapM_ writeOne [0 .. importAltCount - 1]
    Text.IO.writeFile (root </> "main.dhall") mainSource
  where
    hashed =
        "missing "
            <> Import.hashExpressionToCode
                (Core.alphaNormalize (Core.NaturalLit 0 :: Core.Expr Void Void))
            <> " ? ./real.dhall\n"

    writeOne i =
        Text.IO.writeFile (root </> ("a" <> Text.unpack (pad3 i) <> ".dhall")) hashed

    mainSource =
        "[ "
            <> Text.intercalate "\n, "
                [ "./a" <> pad3 i <> ".dhall"
                | i <- [0 .. importAltCount - 1]
                ]
            <> "\n]\n"

rootSubstitutionBenches :: Maybe String -> [Benchmark]
rootSubstitutionBenches mPattern
    | not (active mPattern "import_overhead.root_substitutions.resolve") =
        []
    | otherwise =
        [ bgroup "import_overhead.root_substitutions"
            [ bench "resolve"
                (nfAppIO (resolveWithSettingsCold settings) parsed)
            ]
        ]
  where
    parsed = Core.Var "used"

    unused =
        [ ( "k" <> pad3 i
          , Core.NaturalLit (fromIntegral i)
          )
        | i <- [0 .. 498 :: Int]
        ]

    settings =
        Lens.Micro.set
            Dhall.substitutions
            (Dhall.Map.fromList (("used", Core.NaturalLit 1) : unused))
            (pipelineSettings "." "(root-substitutions)")

startingContextBenches :: Maybe String -> FilePath -> IO [Benchmark]
startingContextBenches mPattern dir
    | not (any (active mPattern) contextLabels) =
        pure []
    | otherwise = do
        timed "import_overhead.starting_context: generate" $
            writeStartingContext dir
        (plainSettings, parsed) <- loadMain dir "main.dhall"
        let settings =
                Lens.Micro.set Dhall.startingContext overheadContext plainSettings
        warmCache <- warmIf (active mPattern contextWarm) dir "starting_context" settings parsed
        let cold =
                [ bench "cold"
                    (nfAppIO (resolveTypecheckNormalizeCold settings) parsed)
                | active mPattern contextCold
                ]
            warm =
                [ bench "warm"
                    (nfAppIO (resolveTypecheckNormalizeAtCache cache settings) parsed)
                | active mPattern contextWarm
                , Just cache <- [warmCache]
                ]
        pure [bgroup "import_overhead.starting_context" (cold ++ warm)]
  where
    contextLabels = [contextCold, contextWarm]
    contextCold = "import_overhead.starting_context.cold"
    contextWarm = "import_overhead.starting_context.warm"

overheadContext :: Dhall.Context.Context (Core.Expr Parser.Src Void)
overheadContext =
    foldl
        (\ctx i -> Dhall.Context.insert (contextBinder i) Core.Natural ctx)
        Dhall.Context.empty
        [0 .. contextBinderCount - 1]

contextBinder :: Int -> Text
contextBinder i = "ctx" <> pad3 i

writeStartingContext :: FilePath -> IO ()
writeStartingContext root = do
    Directory.createDirectoryIfMissing True root
    mapM_ writeOne [0 .. contextFileCount - 1]
    Text.IO.writeFile (root </> "main.dhall") mainSource
  where
    writeOne i =
        Text.IO.writeFile (root </> ("f" <> Text.unpack (pad3 i) <> ".dhall")) "ctx000\n"

    mainSource =
        "[ "
            <> Text.intercalate "\n, "
                [ "./f" <> pad3 i <> ".dhall"
                | i <- [0 .. contextFileCount - 1]
                ]
            <> "\n]\n"

projectionBenches :: Maybe String -> IO [Benchmark]
projectionBenches mPattern
    | not (active mPattern projectionLabel || active mPattern forceAllLabel) =
        pure []
    | otherwise = do
        timed "import_overhead.projection: generate" $
            writeProjection projectionDirectory
        projection <- endToEnd "import_overhead.projection" "projection.dhall"
        forceAll <- endToEnd "import_overhead.force_all" "force_all.dhall"
        pure (projection ++ forceAll)
  where
    projectionLabel = "import_overhead.projection." <> endToEndColdBenchName
    forceAllLabel = "import_overhead.force_all." <> endToEndColdBenchName
    endToEnd group relative
        | not (active mPattern (group <> "." <> endToEndColdBenchName)) =
            pure []
        | otherwise = do
            (settings, parsed) <- loadMain projectionDirectory relative
            pure
                [ bgroup group
                    [ bench endToEndColdBenchName
                        (nfAppIO (resolveTypecheckNormalizeCold settings) parsed)
                    ]
                ]

-- | Always rewrite the projection tree. A leftover from a previous count or
-- field width would silently change the timed cost.
writeProjection :: FilePath -> IO ()
writeProjection root = do
    Directory.createDirectoryIfMissing True root
    mapM_ writeOne [0 .. projectionCount - 1]
    Text.IO.writeFile (root </> "force_all.dhall") forceAllSource
    Text.IO.writeFile (root </> "projection.dhall") projectionSource
  where
    writeOne i =
        Text.IO.writeFile
            (root </> ("r" <> Text.unpack (pad3 i) <> ".dhall"))
            (recordBody i)

    recordBody i =
        "{ "
            <> Text.intercalate "\n, "
                [ "f" <> pad2 j <> " = " <> Text.pack (show (if j == 0 then i else j))
                | j <- [0 .. 7]
                ]
            <> "\n}\n"

    names = [ "r" <> pad3 i | i <- [0 .. projectionCount - 1] ]

    forceAllSource =
        "{ "
            <> Text.intercalate "\n, " [ name <> " = ./" <> name <> ".dhall" | name <- names ]
            <> "\n}\n"

    projectionSource =
        "let rs =\n      { "
            <> Text.intercalate "\n      , " [ name <> " = ./" <> name <> ".dhall" | name <- names ]
            <> "\n      }\n\nin  Natural/show rs.r000.f00\n"

pad2 :: Int -> Text
pad2 i = Text.pack (printf "%02d" i)

loadMain :: FilePath -> FilePath -> IO (Dhall.InputSettings, ParsedExpr)
loadMain directory relative = do
    let path = directory </> relative
    text <- Text.IO.readFile path
    parsed <- either throw pure (Parser.exprFromText path text)
    pure (pipelineSettings directory path, parsed)

warmIf
    :: Bool
    -> FilePath
    -> String
    -> Dhall.InputSettings
    -> ParsedExpr
    -> IO (Maybe FilePath)
warmIf False _ _ _ _ =
    pure Nothing
warmIf True parent label settings parsed = do
    let cache = parent </> ("cache-" <> label)
    Directory.createDirectoryIfMissing True cache
    timed ("import_overhead: warm " <> label) $
        void $ resolveTypecheckNormalizeAtCache cache settings parsed
    pure (Just cache)

active :: Maybe String -> String -> Bool
active = couldMatch

pad3 :: Int -> Text
pad3 i = Text.pack (printf "%03d" i)
