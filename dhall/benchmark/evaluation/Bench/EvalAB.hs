{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Config A/B for normalize and typecheck, plus a one-shot 'EvalStats' dump.
--
-- Timed rows use 'normalizeWithConfig' / 'typeOfWithConfig'. A @.stats.@ row
-- is printed once during prep and is not sampled by tasty-bench; the bench
-- entry only records that the dump ran. Filter with a pattern such as
-- @evalab.ChurchEval.evaluation.default@ or
-- @evalab.ChurchEval.evaluation.stats.default@.
module Bench.EvalAB
    ( benchmarks
    ) where

import Control.Exception (throw, throwIO)
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Void (Void)
import Test.Tasty.Bench
import Text.Printf (printf)

import Bench.Common

import qualified Data.Text.IO as Text.IO
import qualified Dhall
import qualified Dhall.Core as Core
import qualified Dhall.Parser as Parser
import qualified Dhall.TypeCheck as TypeCheck
import qualified Lens.Micro
import qualified System.FilePath as FilePath

data Phase = Evaluation | Typecheck

phaseName :: Phase -> String
phaseName Evaluation = "evaluation"
phaseName Typecheck = "typecheck"

data Config = Config
    { configName :: String
    , configValue :: Core.EvalConfig
    }

configs :: [Config]
configs =
    [ Config "default" Core.defaultEvalConfig
    , Config "noIntern" Core.defaultEvalConfig { Core.internOnClosure = False }
    , Config "noIds" Core.defaultEvalConfig { Core.assignIds = False }
    , Config "noInternNoIds" Core.defaultEvalConfig
        { Core.internOnClosure = False
        , Core.assignIds = False
        }
    , Config "cacheOn" Core.cacheOnConfig
    ]

-- | Same knobs as the timed rows, plus 'countMkVal'. The timed rows leave
-- that off so the extra atomic is not part of the measurement.
statsConfig :: Config -> Core.EvalConfig
statsConfig Config { configValue } =
    configValue { Core.countMkVal = True }

data Fixture = Fixture
    { fixtureName :: String
    , fixturePhase :: Phase
    , fixtureExpr :: ResolvedExpr
    }

rowLabel :: Fixture -> String -> String
rowLabel Fixture { fixtureName, fixturePhase } config =
    "evalab." <> fixtureName <> "." <> phaseName fixturePhase <> "." <> config

statsLabel :: Fixture -> String -> String
statsLabel fixture config = rowLabel fixture ("stats." <> config)

fixtureLabels :: Fixture -> [String]
fixtureLabels fixture =
    [ rowLabel fixture (configName c) | c <- configs ]
        ++ [ statsLabel fixture (configName c) | c <- configs ]

loadNormalizeFile :: String -> IO ResolvedExpr
loadNormalizeFile name = do
    let path = FilePath.joinPath ["benchmark", "evaluation", "normalize", name <> ".dhall"]
    text <- timed ("evalab/" <> name <> ": read") (Text.IO.readFile path)
    parsed <- timed ("evalab/" <> name <> ": parse") $
        either throw pure (Parser.exprFromText path text)
    let settings =
            Lens.Micro.set Dhall.sourceName path
                (Lens.Micro.set
                    Dhall.rootDirectory
                    (FilePath.takeDirectory path)
                    Dhall.defaultInputSettings)
    resolved <- timed ("evalab/" <> name <> ": resolve (cache on)") $
        resolveWithCache settings parsed
    timed ("evalab/" <> name <> ": typecheck") (ensureWellTyped resolved)
    pure resolved

loadPipelineExpr :: String -> FilePath -> FilePath -> IO ResolvedExpr
loadPipelineExpr label directory relative = do
    loaded <- loadPipelineBench label directory relative
    pure (pbResolved loaded)

loadK8sFile3 :: IO ResolvedExpr
loadK8sFile3 = do
    let directory = FilePath.joinPath ["benchmark", "evaluation", "k8s"]
        sourceName = directory FilePath.</> "file3.dhall"
        settings =
            Lens.Micro.set Dhall.sourceName sourceName
                (Lens.Micro.set Dhall.rootDirectory directory Dhall.defaultInputSettings)
    parsed <- timed "evalab/k8s.file3: parse" $
        either throw pure (Parser.exprFromText sourceName "(./file3.dhall).mkPod")
    resolved <- timed "evalab/k8s.file3: resolve (cache on)" $
        resolveWithCache settings parsed
    timed "evalab/k8s.file3: typecheck" (ensureWellTyped resolved)
    pure resolved

-- | Inlined large7 expression. Prep does not typecheck it: that is the bench.
loadLarge7Inlined :: IO ResolvedExpr
loadLarge7Inlined = do
    let directory = FilePath.joinPath ["benchmark", "evaluation", "large7"]
        path = directory FilePath.</> "big.dhall"
    text <- timed "evalab/large7: read" (Text.IO.readFile path)
    parsed <- timed "evalab/large7: parse" $
        either throwIO pure (Parser.exprFromText path text)
    let settings =
            Lens.Micro.set Dhall.sourceName path
                (Lens.Micro.set Dhall.rootDirectory directory Dhall.defaultInputSettings)
    (resolved, _, _) <- timed "evalab/large7: resolve" $
        Dhall.resolveSharedWithSettings settings parsed
    pure resolved

formatStats :: Core.EvalStats -> String
formatStats s = printf
    "internBody=%d valIds=%d envIds=%d mkVal=%d evalHits=%d evalMisses=%d quoteHits=%d quoteMisses=%d convHits=%d convMisses=%d inferInserts=%d evalNanos=%d quoteNanos=%d"
    (Core.internBodyCalls s)
    (Core.valIdsIssued s)
    (Core.envIdsIssued s)
    (Core.mkValCalls s)
    (Core.evalHits s)
    (Core.evalMisses s)
    (Core.quoteHits s)
    (Core.quoteMisses s)
    (Core.convHits s)
    (Core.convMisses s)
    (Core.cacheInserts s)
    (Core.evalNanos s)
    (Core.quoteNanos s)

dumpStats :: Fixture -> Config -> IO ()
dumpStats fixture cfgSpec = do
    let cfg = statsConfig cfgSpec
        label = statsLabel fixture (configName cfgSpec)
    say $ "  " <> label <> " …"
    line <- case fixturePhase fixture of
        Evaluation -> do
            (_expr, stats) <- Core.normalizeWithStats cfg (fixtureExpr fixture)
            pure (formatStats stats)
        Typecheck -> do
            (result, stats) <- TypeCheck.typeOfWithStats cfg (fixtureExpr fixture)
            case result of
                Left err -> throwIO err
                Right _ -> pure (formatStats stats)
    say $ "  " <> label <> ": " <> line

benchFixture :: Fixture -> Config -> Benchmark
benchFixture fixture Config { configName, configValue } =
    bench configName $ case fixturePhase fixture of
        Evaluation -> nf (normalizeWith configValue) (fixtureExpr fixture)
        Typecheck -> nf (typecheckWith configValue) (fixtureExpr fixture)

normalizeWith :: Core.EvalConfig -> ResolvedExpr -> Core.Expr Void Void
normalizeWith cfg = Core.normalizeWithConfig cfg

typecheckWith :: Core.EvalConfig -> ResolvedExpr -> Core.Expr Parser.Src Void
typecheckWith cfg expr =
    either throw id (TypeCheck.typeOfWithConfig cfg expr)

-- | One group per fixture. Configs that the pattern does not name are omitted,
-- so a single config can be timed on its own.
fixtureGroup :: Maybe String -> Fixture -> IO Benchmark
fixtureGroup mPattern fixture = do
    let timedConfigs =
            [ c | c <- configs, couldMatch mPattern (rowLabel fixture (configName c)) ]
        dumped =
            [ c | c <- configs, couldMatch mPattern (statsLabel fixture (configName c)) ]
    mapM_ (dumpStats fixture) dumped
    let statBenches =
            [ bench ("stats." <> configName c) (nfIO (pure ()))
            | c <- dumped
            ]
    pure $ bgroup (fixtureName fixture <> "." <> phaseName (fixturePhase fixture))
        (map (benchFixture fixture) timedConfigs ++ statBenches)

-- | Absent from the default suite. An unfiltered run would time every config,
-- including 'cacheOn' typecheck, which blackholes on some fixtures.
wantsEvalAB :: Maybe String -> Bool
wantsEvalAB Nothing = False
wantsEvalAB (Just pat) = "evalab" `isInfixOf` map toLower pat

benchmarks :: Maybe String -> IO [Benchmark]
benchmarks mPattern
    | not (wantsEvalAB mPattern) = do
        say "Skipping evalab fixtures (pass a pattern containing evalab)"
        pure []
benchmarks mPattern = do
    let candidates = candidateSpecs
        selected = [ spec | spec <- candidates, any (couldMatch mPattern) (specLabels spec) ]
    if null selected
        then do
            say "Skipping evalab fixtures (do not match pattern)"
            pure []
        else do
            say $ "Loading evalab fixtures (" <> show (length selected) <> ")…"
            loaded <- mapM loadSpec selected
            groups <- mapM (fixtureGroup mPattern) loaded
            pure [bgroup "evalab" groups]

data Spec = Spec
    { specName :: String
    , specPhase :: Phase
    , specLoad :: IO ResolvedExpr
    }

specLabels :: Spec -> [String]
specLabels Spec { specName, specPhase } =
    fixtureLabels Fixture
        { fixtureName = specName
        , fixturePhase = specPhase
        , fixtureExpr = unusedExpr
        }
  where
    unusedExpr = error "evalab: label check forced a fixture expression"

loadSpec :: Spec -> IO Fixture
loadSpec Spec { specName, specPhase, specLoad } = do
    expr <- specLoad
    pure Fixture
        { fixtureName = specName
        , fixturePhase = specPhase
        , fixtureExpr = expr
        }

candidateSpecs :: [Spec]
candidateSpecs =
    [ fileEval "ChurchEval"
    , fileEval "FunCompose"
    , fileEval "ListBenchAlt"
    , pipelineEval "large1" "benchmark/evaluation/large1" "main.dhall"
    , Spec "k8s.file3" Evaluation loadK8sFile3
    , Spec "k8s.file3" Typecheck loadK8sFile3
    , Spec "large7.nf_inlined" Evaluation loadLarge7Inlined
    , Spec "large7.type_inlined" Typecheck loadLarge7Inlined
    , pipelineCheck "large3" "benchmark/evaluation/large3" "pipeline.dhall"
    , pipelineCheck "large5" "benchmark/evaluation/large5" "pipeline-code.dhall"
    ]
  where
    fileEval name = Spec name Evaluation (loadNormalizeFile name)
    pipelineEval name directory file =
        Spec name Evaluation (loadPipelineExpr name directory file)
    pipelineCheck name directory file =
        Spec name Typecheck (loadPipelineExpr name directory file)
