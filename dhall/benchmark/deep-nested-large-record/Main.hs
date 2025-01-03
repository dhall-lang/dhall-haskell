{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Void        (Void)
import Test.Tasty.Bench

import qualified Data.Sequence   as Seq
import qualified Dhall.Core      as Core
import qualified Dhall.Import    as Import
import qualified Dhall.TypeCheck as TypeCheck

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

issue412 :: Core.Expr s Void -> Benchmarkable
issue412 prelude = whnf TypeCheck.typeOf expr
  where
    expr
      = Core.Let (Core.Binding Nothing "prelude" Nothing Nothing Nothing prelude)
      $ Core.ListLit Nothing
      $ Seq.replicate 5
      $ Core.Var (Core.V "prelude" 0) `Core.Field` types `Core.Field` little `Core.Field` foo
    types = Core.makeFieldSelection "types"
    little = Core.makeFieldSelection "little"
    foo = Core.makeFieldSelection "Foo"

unionPerformance :: Core.Expr s Void -> Benchmarkable
unionPerformance prelude = whnf TypeCheck.typeOf expr
  where
    expr =
        Core.Let
            (Core.Binding
                Nothing
                "x"
                Nothing
                Nothing
                Nothing
                (Core.Let
                    (Core.Binding
                        Nothing
                        "big"
                        Nothing
                        Nothing
                        Nothing
                        (prelude `Core.Field` types `Core.Field` big)
                    )
                    (Core.Prefer mempty Core.PreferFromSource "big" "big")
                )
            )
            "x"
    types = Core.makeFieldSelection "types"
    big = Core.makeFieldSelection "Big"

main :: IO ()
main =
  defaultMain
    [ env prelude $ \p ->
      bgroup "Prelude"
        [ bench "issue 412" (issue412 p)
        , bench "union performance" (unionPerformance p)
        ]
    ]
  where prelude = Import.load (Core.Embed dhallPreludeImport)
