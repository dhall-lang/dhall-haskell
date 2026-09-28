{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.TypingContext where

import Data.Text (Text)
import Dhall.Core
    ( Const (..)
    , Expr (..)
    , Var (..)
    , judgmentallyEqual
    , makeBinding
    , wrapInLets
    )
import Data.Void (Void)
import Dhall.TypeCheck
    ( TypeError
    , TypingContext
    , emptyTypingContext
    , extendBinder
    , extendLet
    , extendOpaque
    , typeWith
    , typeWithContext
    )

import qualified Dhall.Context
import qualified Dhall.Core       as Core
import qualified Test.Tasty       as Tasty
import qualified Test.Tasty.HUnit as Tasty.HUnit

tests :: Tasty.TestTree
tests =
    Tasty.testGroup "TypingContext"
        [ Tasty.HUnit.testCase "extendLet agrees with wrapInLets" letAgreement
        , Tasty.HUnit.testCase "shadowed lets agree with wrapInLets" shadowAgreement
        , Tasty.HUnit.testCase "extendBinder agrees with typeWith" binderAgreement
        , Tasty.HUnit.testCase "extendOpaque agrees with extendLet" opaqueAgreement
        , Tasty.HUnit.testCase "extendLet rejects an ill-typed value" illTypedLet
        ]

letAgreement :: Tasty.HUnit.Assertion
letAgreement =
    assertSameType
        (must (extendLet "x" one emptyTypingContext))
        (NaturalPlus (var "x" 0) one)
        [makeBinding "x" one]

shadowAgreement :: Tasty.HUnit.Assertion
shadowAgreement =
    let ctx =
            must
                ( do
                    ctx1 <- extendLet "x" one emptyTypingContext
                    extendLet "x" yes ctx1
                )
        body = BoolIf (var "x" 0) (var "x" 1) (NaturalLit 0)
     in assertSameType ctx body [makeBinding "x" one, makeBinding "x" yes]

binderAgreement :: Tasty.HUnit.Assertion
binderAgreement = do
    let body = App List (var "a" 0)
        ctx = must (extendBinder "a" (Const Type) emptyTypingContext)
    fromContext <- mustIO (typeWithContext ctx body)
    fromTypeWith <-
        mustIO
            (typeWith (Dhall.Context.insert "a" (Const Type) Dhall.Context.empty) body)
    assertTypes fromContext fromTypeWith

opaqueAgreement :: Tasty.HUnit.Assertion
opaqueAgreement = do
    let typ = must (typeWith Dhall.Context.empty one)
        ctx = extendOpaque "x" (Core.denote typ) (Core.denote one) emptyTypingContext
    fromOpaque <- mustIO (typeWithContext ctx (var "x" 0))
    fromLet <-
        mustIO (extendLet "x" one emptyTypingContext >>= flip typeWithContext (var "x" 0))
    assertTypes fromOpaque fromLet

illTypedLet :: Tasty.HUnit.Assertion
illTypedLet =
    case extendLet "x" (NaturalPlus one (BoolLit True)) emptyTypingContext of
        Left _ -> return ()
        Right _ -> Tasty.HUnit.assertFailure "ill-typed let was accepted"

assertSameType
    :: TypingContext ()
    -> Expr () Void
    -> [Core.Binding () Void]
    -> Tasty.HUnit.Assertion
assertSameType ctx body bindings = do
    fromContext <- mustIO (typeWithContext ctx body)
    fromLets <- mustIO (typeWith Dhall.Context.empty (wrapInLets bindings body))
    assertTypes fromContext fromLets

assertTypes :: Expr () Void -> Expr () Void -> Tasty.HUnit.Assertion
assertTypes got expected =
    Tasty.HUnit.assertBool
        (show (Core.pretty got) <> " /= " <> show (Core.pretty expected))
        (judgmentallyEqual got expected)

must :: Either (TypeError () Void) a -> a
must (Right x) = x
must (Left err) = error (show err)

mustIO :: Either (TypeError () Void) a -> IO a
mustIO (Right x) = return x
mustIO (Left err) = Tasty.HUnit.assertFailure (show err)

one :: Expr () Void
one = NaturalLit 1

yes :: Expr () Void
yes = BoolLit True

var :: Text -> Int -> Expr s a
var name index = Var (V name index)
