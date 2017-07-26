{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Normalization (normalizationTests) where

import Data.Monoid ((<>))
import Dhall.Core
import Dhall.Context
import Test.Tasty
import Test.Tasty.HUnit
import Util 

normalizationTests :: TestTree
normalizationTests = testGroup "normalization" [ constantFolding
                                               , conversions
                                               , fusion
                                               , customization
                                               ]

constantFolding :: TestTree
constantFolding = testGroup "folding of constants" [ naturalPlus
                                                   , optionalFold
                                                   , optionalBuild
                                                   ]

conversions :: TestTree
conversions = testGroup "conversions" [ naturalShow
                                      , integerShow
                                      , doubleShow
                                      , naturalToInteger
                                      ]

customization :: TestTree
customization = testGroup "customization"
                 [simpleCustomization
                 ,nestedReduction]

simpleCustomization :: TestTree
simpleCustomization = testCase "simpleCustomization" $ do
  let tyCtx  = insert "min" (Pi "_" Natural (Pi "_" Natural Natural)) empty 
      valCtx e = case e of
                    (App (App (Var (V "min" 0)) (NaturalLit x)) (NaturalLit y)) -> Just (NaturalLit (min x y))
                    _ -> Nothing
  e <- codeWith tyCtx "min (min +11 +12) +8 + +1" 
  assertNormalizesToWith valCtx e "+9"

nestedReduction :: TestTree
nestedReduction = testCase "doubleReduction" $ do
  minType        <- insert "min"        <$> code "Natural → Natural → Natural"
  fiveorlessType <- insert "fiveorless" <$> code "Natural → Natural"
  wurbleType     <- insert "wurble"     <$> code "Natural → Integer"
  let tyCtx = minType . fiveorlessType . wurbleType $ empty
      valCtx e = case e of
                    (App (App (Var (V "min" 0)) (NaturalLit x)) (NaturalLit y)) -> Just (NaturalLit (min x y))
                    (App (Var (V "wurble" 0)) (NaturalLit x)) -> Just
                        (App (Var (V "fiveorless" 0)) (NaturalPlus (NaturalLit x) (NaturalLit 2))) 
                    (App (Var (V "fiveorless" 0)) (NaturalLit x)) -> Just
                        (App (App (Var (V "min" 0)) (NaturalLit x)) (NaturalPlus (NaturalLit 3) (NaturalLit 2)))
                    _ -> Nothing
  e <- codeWith tyCtx "wurble +6"
  assertNormalizesToWith valCtx e "+5"

naturalPlus :: TestTree
naturalPlus = testCase "natural plus" $ do
  e <- code "+1 + +2"
  e `assertNormalizesTo` "+3"

naturalToInteger :: TestTree
naturalToInteger = testCase "Natural/toInteger" $ do
  e <- code "Natural/toInteger +1"
  isNormalized e @?= False
  normalize' e @?= "1"

naturalShow :: TestTree
naturalShow = testCase "Natural/show" $ do
  e <- code "Natural/show +42"
  e `assertNormalizesTo` "\"+42\""

integerShow :: TestTree
integerShow = testCase "Integer/show" $ do
  e <- code "[Integer/show 1337, Integer/show -42, Integer/show 0]"
  e `assertNormalizesTo` "[\"1337\", \"-42\", \"0\"]"

doubleShow :: TestTree
doubleShow = testCase "Double/show" $ do
  e <- code "[Double/show -0.42, Double/show 13.37]"
  e `assertNormalizesTo` "[\"-0.42\", \"13.37\"]"

optionalFold :: TestTree
optionalFold = testGroup "Optional/fold" [ just, nothing ]
  where test label inp out = testCase label $ do
             e <- code ("Optional/fold Text ([" <> inp <> "] : Optional Text) Natural (λ(j : Text) → +1) +2")
             e `assertNormalizesTo` out
        just = test "just" "\"foo\"" "+1"
        nothing = test "nothing" "" "+2"

optionalBuild :: TestTree
optionalBuild = testGroup "Optional/build" [ optionalBuild1
                                           , optionalBuildShadowing
                                           , optionalBuildIrreducible
                                           ]

optionalBuild1 :: TestTree
optionalBuild1 = testCase "reducible" $ do
  e <- code
    "Optional/build                  \n\
    \Natural                         \n\
    \(   λ(optional : Type)          \n\
    \→   λ(just : Natural → optional)\n\
    \→   λ(nothing : optional)       \n\
    \→   just +1                     \n\
    \)                               \n"
  e `assertNormalizesTo` "[+1] : Optional Natural"

optionalBuildShadowing :: TestTree
optionalBuildShadowing = testCase "handles shadowing" $ do
  e <- code
    "Optional/build               \n\
    \Integer                      \n\
    \(   λ(optional : Type)       \n\
    \→   λ(x : Integer → optional)\n\
    \→   λ(x : optional)          \n\
    \→   x@1 1                    \n\
    \)                            \n"
  e `assertNormalizesTo` "[1] : Optional Integer"

optionalBuildIrreducible :: TestTree
optionalBuildIrreducible = testCase "irreducible" $ do
  e <- code
    "    λ(id : ∀(a : Type) → a → a)  \n\
    \→   Optional/build               \n\
    \    Bool                         \n\
    \    (   λ(optional : Type)       \n\
    \    →   λ(just : Bool → optional)\n\
    \    →   λ(nothing : optional)    \n\
    \    →   id optional (just True)  \n\
    \    )                            \n"
  assertNormalized e

fusion :: TestTree
fusion = testGroup "Optional build/fold fusion" [ fuseOptionalBF
                                                , fuseOptionalFB
                                                ]

fuseOptionalBF :: TestTree
fuseOptionalBF = testCase "fold . build" $ do
  e0 <- code
    "    λ(  f                        \n\
    \    :   ∀(optional : Type)       \n\
    \    →   ∀(just : Text → optional)\n\
    \    →   ∀(nothing : optional)    \n\
    \    →   optional                 \n\
    \    )                            \n\
    \→   Optional/fold                \n\
    \    Text                         \n\
    \    (   Optional/build           \n\
    \        Text                     \n\
    \        f                        \n\
    \    )                            \n"
  e1 <- code
    "    λ(  f                        \n\
    \    :   ∀(optional : Type)       \n\
    \    →   ∀(just : Text → optional)\n\
    \    →   ∀(nothing : optional)    \n\
    \    →   optional                 \n\
    \    )                            \n\
    \→   f                            \n"
  e0 `assertNormalizesTo` (Dhall.Core.pretty e1)

fuseOptionalFB :: TestTree
fuseOptionalFB = testCase "build . fold" $ do
  test <- code
    "Optional/build                 \n\
    \Text                           \n\
    \(   Optional/fold              \n\
    \    Text                       \n\
    \    ([\"foo\"] : Optional Text)\n\
    \)                              \n"
  test `assertNormalizesTo` "[\"foo\"] : Optional Text"
