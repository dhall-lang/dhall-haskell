{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (throw)
import Data.Text         (Text)
import Data.Void         (Void)
import Test.Tasty.Bench

import qualified Data.ByteString.Lazy
import qualified Data.Text            as Text
import qualified Data.Text.IO
import qualified Dhall.Binary
import qualified Dhall.Core           as Dhall
import qualified Dhall.Parser         as Dhall

-- Note: do not use !expr below, to avoid the bug mentioned here https://github.com/UnkindPartition/tasty/issues/401
benchExprFromText :: String -> Text -> Benchmark
benchExprFromText name expr =
    bench name $ whnf (Dhall.exprFromText "(input)") expr

benchExprFromBytes :: String -> Data.ByteString.Lazy.ByteString -> Benchmark
benchExprFromBytes name bs = bench name (nf f bs)
  where
    f bytes =
        case Dhall.Binary.decodeExpression bytes of
            Left  exception  -> error (show exception)
            Right expression -> expression :: Dhall.Expr Void Dhall.Import

-- Note: do not use !expr below, to avoid the bug mentioned here https://github.com/UnkindPartition/tasty/issues/401
benchNfExprFromText :: String -> Text -> Benchmark
benchNfExprFromText name expr =
    bench name $ nf (either throw id . Dhall.exprFromText "(input)") expr

-- Parse a natural literal and `nf` the `Natural` itself, not the surrounding `Expr`.
benchParsedNatural :: String -> Text -> Benchmark
benchParsedNatural name expr =
    bench name $ nf parsedNatural expr
  where
    parsedNatural text =
        case Dhall.exprFromText "(input)" text of
            Left err -> throw err
            Right e  -> go e

    go (Dhall.Note _ e)      = go e
    go (Dhall.NaturalLit n)  = n
    go other                 =
        error ("expected a natural literal, got: " <> take 80 (show other))

main :: IO ()
main =
    defaultMain
        [ env issues $ \ ~(it, ib) ->
            bgroup "Issue #108"
                [ benchExprFromText  "Text"   it
                , benchExprFromBytes "Binary" ib
                ]
        , env kubernetesExample $
            benchExprFromBytes "Kubernetes/Binary"
        , benchExprFromText "Deeply nested parentheses" (Text.replicate 1000 "(" <> "x" <> Text.replicate 1000 ")")
        , benchExprFromText "Deeply nested brackets" (Text.replicate 1000 "[" <> " 0 " <> Text.replicate 1000 "]")
        , benchExprFromText "Long variable names" (Text.replicate 1000000 "x")
        , benchExprFromText "Large number of function arguments" (Text.replicate 10000 "x ")
        , benchExprFromText "Long double-quoted strings (10M chars)" ("\"" <> Text.replicate 10000000 "x" <> "\"")
        , benchExprFromText "Long single-quoted strings (10M chars)" ("''\n" <> Text.replicate 10000000 "x" <> "\n''")
        , benchExprFromText "Large natural number literal (10M digits)" (Text.replicate 10000000 "1")
        , benchExprFromText "Large hex number literal (10M digits)" ("0x" <> Text.replicate 10000000 "1")
        , benchExprFromText "Large binary number literal (10M digits)" ("0b" <> Text.replicate 10000000 "1")
        , benchParsedNatural "Large natural number literal (10M digits, forced)" (Text.replicate 10000000 "1")
        , benchParsedNatural "Large hex number literal (10M digits, forced)" ("0x" <> Text.replicate 10000000 "1")
        , benchParsedNatural "Large binary number literal (10M digits, forced)" ("0b" <> Text.replicate 10000000 "1")
        , benchExprFromText "Whitespace" (Text.replicate 1000000 " " <> "x")
        , benchExprFromText "Line comment" ("x -- " <> Text.replicate 1000000 " ")
        , benchExprFromText "Block comment" ("x {- " <> Text.replicate 1000000 " " <> "-}")
        , env cpkgExample $ \cpkg ->
            bgroup "CPkg"
                [ bench "parse" $ whnf parsePhase cpkg
                , benchNfExprFromText "Text" cpkg
                ]
        ]
    where
        cpkgExample = Data.Text.IO.readFile "benchmark/parser/examples/cpkg.dhall"
        parsePhase text =
            case Dhall.exprFromText "(input)" text of
                Left err -> throw err
                Right expr -> expr
        issue108Text = Data.Text.IO.readFile "benchmark/parser/examples/issue108.dhall"
        issue108Bytes = Data.ByteString.Lazy.readFile "benchmark/parser/examples/issue108.dhallb"
        issues = (,) <$> issue108Text <*> issue108Bytes
        kubernetesExample = Data.ByteString.Lazy.readFile "benchmark/parser/examples/kubernetes.dhallb"
