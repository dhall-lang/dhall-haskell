{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (throw)
import Control.Monad     (forM)
import Data.Map          (Map)
import Data.Text         (Text)
import Data.Void         (Void)
import Test.Tasty.Bench

import qualified Data.ByteString.Lazy
import qualified Data.Map             as Map
import qualified Data.Text            as Text
import qualified Data.Text.IO
import qualified Dhall.Binary
import qualified Dhall.Core           as Dhall
import qualified Dhall.Parser         as Dhall
import qualified System.Directory     as Directory

type PreludeFiles = Map FilePath Text

loadPreludeFiles :: IO PreludeFiles
loadPreludeFiles = loadDirectory "./dhall-lang/Prelude"
    where
        loadDirectory :: FilePath -> IO PreludeFiles
        loadDirectory dir =
            Directory.withCurrentDirectory dir $ do
                files <- Directory.getCurrentDirectory >>= Directory.listDirectory
                results <- forM files $ \file -> do
                    file' <- Directory.makeAbsolute file
                    doesExist <- Directory.doesFileExist file'
                    if doesExist
                       then loadFile file'
                       else loadDirectory file'
                pure $ Map.unions results

        loadFile :: FilePath -> IO PreludeFiles
        loadFile path = Map.singleton path <$> Data.Text.IO.readFile path

benchParser :: PreludeFiles -> Benchmark
benchParser =
      bgroup "exprFromText"
    . Map.foldrWithKey (\name expr -> (benchExprFromText name expr :)) []

benchExprFromText :: String -> Text -> Benchmark
benchExprFromText name !expr =
    bench name $ whnf (Dhall.exprFromText "(input)") expr

benchExprFromBytes :: String -> Data.ByteString.Lazy.ByteString -> Benchmark
benchExprFromBytes name bs = bench name (nf f bs)
  where
    f bytes =
        case Dhall.Binary.decodeExpression bytes of
            Left  exception  -> error (show exception)
            Right expression -> expression :: Dhall.Expr Void Dhall.Import

benchNfExprFromText :: String -> Text -> Benchmark
benchNfExprFromText name !expr =
    bench name $ nf (either throw id . Dhall.exprFromText "(input)") expr

main :: IO ()
main = do
    prelude <- loadPreludeFiles
    defaultMain
        [ env issues $ \ ~(it, ib) ->
            bgroup "Issue #108"
                [ benchExprFromText  "Text"   it
                , benchExprFromBytes "Binary" ib
                ]
        , env kubernetesExample $
            benchExprFromBytes "Kubernetes/Binary"
        , benchExprFromText "Long variable names" (Text.replicate 1000000 "x")
        , benchExprFromText "Large number of function arguments" (Text.replicate 10000 "x ")
        , benchExprFromText "Long double-quoted strings" ("\"" <> Text.replicate 1000000 "x" <> "\"")
        , benchExprFromText "Long single-quoted strings" ("''" <> Text.replicate 1000000 "x" <> "''")
        , benchExprFromText "Whitespace" (Text.replicate 1000000 " " <> "x")
        , benchExprFromText "Line comment" ("x -- " <> Text.replicate 1000000 " ")
        , benchExprFromText "Block comment" ("x {- " <> Text.replicate 1000000 " " <> "-}")
        , benchExprFromText "Deeply nested parentheses" "((((((((((((((((x))))))))))))))))"
        , benchParser prelude
        , env cpkgExample $
            benchNfExprFromText "CPkg/Text"
        ]
    where
        cpkgExample = Data.Text.IO.readFile "benchmark/parser/examples/cpkg.dhall"
        issue108Text = Data.Text.IO.readFile "benchmark/parser/examples/issue108.dhall"
        issue108Bytes = Data.ByteString.Lazy.readFile "benchmark/parser/examples/issue108.dhallb"
        issues = (,) <$> issue108Text <*> issue108Bytes
        kubernetesExample = Data.ByteString.Lazy.readFile "benchmark/parser/examples/kubernetes.dhallb"
