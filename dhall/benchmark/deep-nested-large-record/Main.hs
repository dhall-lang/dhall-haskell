{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Gauge(defaultMain)

import qualified Data.Sequence as Seq
import qualified Dhall.Core as Core
import qualified Dhall.Import as Import
import qualified Dhall.TypeCheck as TypeCheck
import qualified Gauge

dhallPreludeImport :: Core.Import
dhallPreludeImport = Core.Import
  { Core.importMode = Core.Code
  , Core.importHashed = Core.ImportHashed
    { Core.hash = Nothing
    , Core.importType = Core.Local Core.Here $ Core.File
      { Core.directory = Core.Directory ["deep-nested-large-record", "benchmark"]
      , Core.file = "prelude.dhall"
      }
    }
  }

issue412 :: Core.Expr s TypeCheck.X -> Gauge.Benchmarkable
issue412 prelude = Gauge.whnf TypeCheck.typeOf expr
  where
    expr
      = Core.Let (pure (Core.Binding "prelude" Nothing prelude))
      $ Core.ListLit Nothing
      $ Seq.replicate 5
      $ Core.Var (Core.V "prelude" 0) `Core.Field` "types" `Core.Field` "Little" `Core.Field` "Foo"

unionPerformance :: Core.Expr s TypeCheck.X -> Gauge.Benchmarkable
unionPerformance prelude = Gauge.whnf TypeCheck.typeOf expr
  where
    innerBinding =
        Core.Binding "big" Nothing
            (prelude `Core.Field` "types" `Core.Field` "Big")

    outerBinding =
        Core.Binding "x" Nothing
            (Core.Let (pure innerBinding) (Core.Prefer "big" "big"))

    expr = Core.Let (pure outerBinding) "x"

main :: IO ()
main = do
  prelude <- Import.load (Core.Embed dhallPreludeImport)
  defaultMain
    [ Gauge.bench "issue 412" (issue412 prelude)
    , Gauge.bench "union performance" (unionPerformance prelude)
    ]
