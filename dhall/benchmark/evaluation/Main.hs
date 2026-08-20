{-# LANGUAGE OverloadedStrings #-}
--
-- Evaluation benchmark harness for Dhall import/resolve/typecheck/normalize.
--
-- See benchmark/evaluation/README.md for how to read results. Modes:
--
--   Mode A (phase): prep resolves with UseSemanticCache; timed resolve uses
--     IgnoreSemanticCache (semantic off, semisemantic still on); typecheck and
--     evaluation run on the pre-resolved AST from prep.
--
--   Mode B (resolve_cold_cache_on): parse-only prep; each sample uses a fresh
--     XDG_CACHE_HOME with caches enabled.
--
--   Mode D (end_to_end_cold): parse-only prep; each sample uses a fresh
--     XDG_CACHE_HOME and runs resolveWithSettings → typeOf → normalize.
--
--   Mode C (implicit): some as Source large6 costs appear on evaluation, not
--     resolve. See large6/README.md matrix.
--
-- Suites live in Bench.* modules; this file only loads and assembles them.
--
module Main where

import System.Environment (getArgs)
import Test.Tasty.Bench (defaultMain)

import Bench.Common (patternFromArgs, say)
import qualified Bench.ImportTrees as ImportTrees
import qualified Bench.Semisemantic as Semisemantic
import qualified Bench.Substitutions as Substitutions

-- Prep uses disk caches for Mode A validity + phase benches; Mode B/D fixtures
-- skip cache-warming resolve. See benchmark/evaluation/README.md.
main :: IO ()
main = do
    args <- getArgs
    let mPattern = patternFromArgs args
    case mPattern of
        Nothing  -> say "Preparing benchmarks (loading fixtures; prep cache on)…"
        Just pat -> say $ "Preparing benchmarks matching " <> show pat <> " (prep cache on)…"

    importTreeBenches <- ImportTrees.benchmarks mPattern
    substitutionBenches <- Substitutions.benchmarks mPattern
    semisemanticBenches <- Semisemantic.benchmarks mPattern

    say "Starting tasty-bench…"

    defaultMain $ concat
        [ importTreeBenches
        , substitutionBenches
        , semisemanticBenches
        ]
