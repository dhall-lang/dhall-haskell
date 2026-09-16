{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.CSE (tests) where

import Data.Void (Void)
import Dhall.Core
    ( Expr (..)
    , Var (..)
    , CseOptions (..)
    , cse
    , cseWith
    , defaultCseOptions
    , denote
    , judgmentallyEqual
    , makeFunctionBinding
    , normalize
    , pretty
    )
import Dhall.Pretty (ChooseCharacterSet (..))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@?=))

import qualified Data.Text
import qualified Dhall.Parser as Parser
import qualified Dhall.TypeCheck as TypeCheck
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

tests :: TestTree
tests =
    Tasty.testGroup "CSE"
        [ HUnit.testCase "factors a repeated union type" $ do
            let e = parse
                    "[ < A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z >.A\
                    \, < A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z >.B ]"
                actual = cse e
            HUnit.assertBool "introduces a let"
                ("let" `Data.Text.isInfixOf` pretty actual)
            semanticallyPreserved e actual

        , HUnit.testCase "does not extract trivial literals" $ do
            let e = NaturalPlus (NaturalLit 1) (NaturalLit 1) :: Expr Void Void
            cse e @?= denote e

        , HUnit.testCase "skips unprofitable small duplicates" $ do
            let e = parse "[ True, True ]"
            cse e @?= denote e

        , HUnit.testCase "extracts same-binder open terms" $ do
            let e = parse
                    "\\(x : Natural) -> [ [ x, x, x, x, x, x, x, x ], [ x, x, x, x, x, x, x, x ] ]"
                forced = cseWith aggressive e
            HUnit.assertBool "binds inside the lambda"
                ("λ(x : Natural) →" `Data.Text.isInfixOf` pretty forced
                    || "\\(x : Natural) ->" `Data.Text.isInfixOf` pretty forced)
            HUnit.assertBool "introduces a let"
                ("let" `Data.Text.isInfixOf` pretty forced)
            semanticallyPreserved e forced

        , HUnit.testCase "does not merge shadowed binders" $ do
            let e = parse
                    "\\(x : Natural) ->\
                    \  { a = [ [ x, x, x, x, x, x, x, x ], [ x, x, x, x, x, x, x, x ] ]\
                    \  , b = \\(x : Natural) ->\
                    \      [ [ x, x, x, x, x, x, x, x ], [ x, x, x, x, x, x, x, x ] ]\
                    \  }"
                forced = cseWith aggressive e
            semanticallyPreserved e forced
            let lets = length (Data.Text.breakOnAll "let" (pretty forced))
            HUnit.assertBool "keeps distinct lets for each x" (lets >= 2)

        , HUnit.testCase "avoids clashing with existing _1" $ do
            let e = parse
                    "let _1 = 0 in [ < A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z >.A\
                    \, < A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z >.B ]"
                actual = cse e
            HUnit.assertBool "does not rebind _1"
                ("let _1 =" `Data.Text.isInfixOf` pretty actual)
            HUnit.assertBool "uses a later fresh name"
                ("let _2 =" `Data.Text.isInfixOf` pretty actual)
            semanticallyPreserved e actual

        , HUnit.testCase "does not extract Embed" $ do
            let e = parse "[ 1, 1 ]"
            cse e @?= denote e

        , HUnit.testCase "nested overlap prefers the outer candidate first" $ do
            let e = parse
                    "[ < A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z >.A\
                    \, < A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z >.B\
                    \, < A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z >.A ]"
                actual = cse e
            semanticallyPreserved e actual
            cse actual @?= actual

        , HUnit.testCase "lambda annotation is not lost" $ do
            let e = Lam AutoInferCharSet (makeFunctionBinding "x" Natural)
                        (NaturalPlus (Var (V "x" 0)) (NaturalLit 1))
                      :: Expr Void Void
            cse e @?= denote e
        ]

aggressive :: CseOptions
aggressive = defaultCseOptions { cseMinSaving = minBound }

parse :: Data.Text.Text -> Expr Void Void
parse text =
    case Parser.exprFromText mempty text of
        Left err -> error (show err)
        Right e  -> fmap unexpectedImport (denote e)
  where
    unexpectedImport imp = error ("unexpected import: " <> show imp)

semanticallyPreserved :: Expr Void Void -> Expr Void Void -> IO ()
semanticallyPreserved original compacted = do
    HUnit.assertBool "judgmentallyEqual" (judgmentallyEqual original compacted)
    (denote (normalize original) :: Expr Void Void)
        @?= denote (normalize compacted)
    typed original
    typed compacted
  where
    typed e =
        case TypeCheck.typeOf e of
            Left err -> HUnit.assertFailure (show err)
            Right _  -> pure ()
