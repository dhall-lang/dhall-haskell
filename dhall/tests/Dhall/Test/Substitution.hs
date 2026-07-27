{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Substitution where

import Control.Exception (throwIO)
import Data.Void         (Void)
import Dhall.Core        (Binding (..), Expr (..), Var (..))
import Dhall.Src         (Src)

import qualified Data.Either.Validation
import qualified Dhall
import qualified Dhall.Core                 as Core
import qualified Dhall.Map
import qualified Dhall.Substitution
import qualified Lens.Micro                 as Lens
import qualified Test.Tasty                 as Tasty
import qualified Test.Tasty.HUnit           as Tasty.HUnit

import Test.Tasty.HUnit ((@?=))

data Result = Failure Integer | Success String
    deriving (Eq, Dhall.Generic, Show)

instance Dhall.FromDhall Result

substituteResult :: FilePath -> IO Result
substituteResult fp = do
    rt <- resultType
    let evaluateSettings = Lens.over Dhall.substitutions (Dhall.Map.insert "Result" rt) Dhall.defaultEvaluateSettings
    Dhall.inputFileWithSettings evaluateSettings resultDecoder fp

resultDecoder :: Dhall.Decoder Result
resultDecoder = Dhall.auto

resultType :: IO (Expr Src Void)
resultType = case Dhall.expected resultDecoder of
    Data.Either.Validation.Failure e -> throwIO e
    Data.Either.Validation.Success x -> return x

substituteFoo :: FilePath -> IO Bool
substituteFoo fp = let
    evaluateSettings = Lens.set Dhall.substitutions (Dhall.Map.fromList [("Foo", Var "Bar"), ("Bar", BoolLit True)]) Dhall.defaultEvaluateSettings
    in Dhall.inputFileWithSettings evaluateSettings Dhall.auto fp

tests :: Tasty.TestTree
tests =
    Tasty.testGroup "Substitution"
        [ noSubstitutionsIsIdentity
        , freeVariableIsSubstituted
        , unrelatedVariableIsUntouched
        , shadowedVariableInLamIsUntouched
        , shadowedVariableInPiIsUntouched
        , shadowedVariableInLetIsUntouched
        , variableReferringPastLamShadowIsSubstituted
        , letBindingValueIsSubstituted
        , substitutionAvoidsCapture
        , multipleIndependentSubstitutionsAreApplied
        , dependentSubstitutionsChain
        , dependentSubstitutionsChainThreeLevels
        , dependentSubstitutionsOnlyChainForward
        , letAnnotationIsSubstituted
        , selfReferentialSubstitutionIsANoop
        ]

noSubstitutionsIsIdentity :: Tasty.TestTree
noSubstitutionsIsIdentity = Tasty.HUnit.testCase "No substitutions is the identity" $ do
    let expr = Var "x" :: Expr Void Void

    Dhall.Substitution.substitute expr Dhall.Substitution.empty @?= expr

freeVariableIsSubstituted :: Tasty.TestTree
freeVariableIsSubstituted = Tasty.HUnit.testCase "A free variable is substituted" $ do
    let expr = Var "x" :: Expr Void Void

    let substitutions = Dhall.Map.fromList [ ("x", NaturalLit 1) ]

    Dhall.Substitution.substitute expr substitutions @?= NaturalLit 1

unrelatedVariableIsUntouched :: Tasty.TestTree
unrelatedVariableIsUntouched = Tasty.HUnit.testCase "An unrelated variable is untouched" $ do
    let expr = Var "y" :: Expr Void Void

    let substitutions = Dhall.Map.fromList [ ("x", NaturalLit 1) ]

    Dhall.Substitution.substitute expr substitutions @?= expr

shadowedVariableInLamIsUntouched :: Tasty.TestTree
shadowedVariableInLamIsUntouched = Tasty.HUnit.testCase "A variable shadowed by a Lam is untouched" $ do
    --  \(x : Natural) -> x
    let expr = Lam mempty (Core.makeFunctionBinding "x" Natural) (Var "x") :: Expr Void Void

    let substitutions = Dhall.Map.fromList [ ("x", NaturalLit 1) ]

    Dhall.Substitution.substitute expr substitutions @?= expr

shadowedVariableInPiIsUntouched :: Tasty.TestTree
shadowedVariableInPiIsUntouched = Tasty.HUnit.testCase "A variable shadowed by a Pi is untouched" $ do
    --  forall (x : Type) -> x
    let expr = Pi mempty "x" (Const Core.Type) (Var "x") :: Expr Void Void

    let substitutions = Dhall.Map.fromList [ ("x", Bool) ]

    Dhall.Substitution.substitute expr substitutions @?= expr

shadowedVariableInLetIsUntouched :: Tasty.TestTree
shadowedVariableInLetIsUntouched = Tasty.HUnit.testCase "A variable shadowed by a Let is untouched" $ do
    --  let x = 5 in x
    let expr = Let (Core.makeBinding "x" (NaturalLit 5)) (Var "x") :: Expr Void Void

    let substitutions = Dhall.Map.fromList [ ("x", NaturalLit 999) ]

    Dhall.Substitution.substitute expr substitutions @?= expr

variableReferringPastLamShadowIsSubstituted :: Tasty.TestTree
variableReferringPastLamShadowIsSubstituted = Tasty.HUnit.testCase "A variable referring past a Lam shadow is substituted" $ do
    --  \(x : Natural) -> x@1
    let expr = Lam mempty (Core.makeFunctionBinding "x" Natural) (Var (V "x" 1)) :: Expr Void Void

    --  \(x : Natural) -> 1
    let expected = Lam mempty (Core.makeFunctionBinding "x" Natural) (NaturalLit 1)

    let substitutions = Dhall.Map.fromList [ ("x", NaturalLit 1) ]

    Dhall.Substitution.substitute expr substitutions @?= expected

letBindingValueIsSubstituted :: Tasty.TestTree
letBindingValueIsSubstituted = Tasty.HUnit.testCase "A Let's bound value is substituted" $ do
    --  let y = x in y
    let expr = Let (Core.makeBinding "y" (Var "x")) (Var "y") :: Expr Void Void

    --  let y = 5 in y
    let expected = Let (Core.makeBinding "y" (NaturalLit 5)) (Var "y")

    let substitutions = Dhall.Map.fromList [ ("x", NaturalLit 5) ]

    Dhall.Substitution.substitute expr substitutions @?= expected

substitutionAvoidsCapture :: Tasty.TestTree
substitutionAvoidsCapture = Tasty.HUnit.testCase "Substitution avoids variable capture" $ do
    --  \(y : Natural) -> x
    let expr = Lam mempty (Core.makeFunctionBinding "y" Natural) (Var "x") :: Expr Void Void

    --  \(y : Natural) -> y@1
    let expected = Lam mempty (Core.makeFunctionBinding "y" Natural) (Var (V "y" 1))

    let substitutions = Dhall.Map.fromList [ ("x", Var "y") ]

    Dhall.Substitution.substitute expr substitutions @?= expected

multipleIndependentSubstitutionsAreApplied :: Tasty.TestTree
multipleIndependentSubstitutionsAreApplied = Tasty.HUnit.testCase "Multiple independent substitutions are all applied" $ do
    --  \(a : Natural) -> \(b : Natural) -> x + y
    let expr =
            Lam mempty (Core.makeFunctionBinding "a" Natural)
                (Lam mempty (Core.makeFunctionBinding "b" Natural)
                    (NaturalPlus (Var "x") (Var "y"))
                ) :: Expr Void Void

    --  \(a : Natural) -> \(b : Natural) -> 2 + 3
    let expected =
            Lam mempty (Core.makeFunctionBinding "a" Natural)
                (Lam mempty (Core.makeFunctionBinding "b" Natural)
                    (NaturalPlus (NaturalLit 2) (NaturalLit 3))
                )

    let substitutions = Dhall.Map.fromList [ ("x", NaturalLit 2), ("y", NaturalLit 3) ]

    Dhall.Substitution.substitute expr substitutions @?= expected

dependentSubstitutionsChain :: Tasty.TestTree
dependentSubstitutionsChain = Tasty.HUnit.testCase "Dependent substitutions chain" $ do
    let expr = Var "Foo" :: Expr Void Void

    let substitutions = Dhall.Map.fromList [ ("Foo", Var "Bar"), ("Bar", BoolLit True) ]

    Dhall.Substitution.substitute expr substitutions @?= BoolLit True

dependentSubstitutionsChainThreeLevels :: Tasty.TestTree
dependentSubstitutionsChainThreeLevels = Tasty.HUnit.testCase "Dependent substitutions chain through multiple levels" $ do
    let expr = Var "A" :: Expr Void Void

    let substitutions = Dhall.Map.fromList [ ("A", Var "B"), ("B", Var "C"), ("C", BoolLit True) ]

    Dhall.Substitution.substitute expr substitutions @?= BoolLit True

dependentSubstitutionsOnlyChainForward :: Tasty.TestTree
dependentSubstitutionsOnlyChainForward = Tasty.HUnit.testCase "Dependent substitutions only chain forward" $ do
    let expr = Var "A" :: Expr Void Void

    let substitutions = Dhall.Map.fromList [ ("C", BoolLit True), ("B", Var "C"), ("A", Var "B") ]

    Dhall.Substitution.substitute expr substitutions @?= Var "B"

letAnnotationIsSubstituted :: Tasty.TestTree
letAnnotationIsSubstituted = Tasty.HUnit.testCase "A Let's type annotation is substituted" $ do
    --  let y : x = 5 in y
    let expr = Let (Binding Nothing "y" Nothing (Just (Nothing, Var "x")) Nothing (NaturalLit 5)) (Var "y") :: Expr Void Void

    --  let y : Natural = 5 in y
    let expected = Let (Binding Nothing "y" Nothing (Just (Nothing, Natural)) Nothing (NaturalLit 5)) (Var "y")

    let substitutions = Dhall.Map.fromList [ ("x", Natural) ]

    Dhall.Substitution.substitute expr substitutions @?= expected

selfReferentialSubstitutionIsANoop :: Tasty.TestTree
selfReferentialSubstitutionIsANoop = Tasty.HUnit.testCase "A self-referential substitution terminates as a no-op" $ do
    let expr = Var "Foo" :: Expr Void Void

    let substitutions = Dhall.Map.fromList [ ("Foo", Var "Foo") ]

    Dhall.Substitution.substitute expr substitutions @?= Var "Foo"
