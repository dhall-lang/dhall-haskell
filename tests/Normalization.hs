{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Normalization (normalizationTests) where

import           Dhall.Core
import qualified NeatInterpolation
import           Test.Tasty
import           Test.Tasty.HUnit
import           Util (code, normalize', assertNormalizesTo, assertNormalized)

normalizationTests :: TestTree
normalizationTests = testGroup "normalization" [ constantFolding
                                               , conversions
                                               , fusion
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
             e <- code [NeatInterpolation.text|
                         Optional/fold Text ([$inp] : Optional Text) Natural (λ(j : Text) → +1) +2
                       |]
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
  e <- code [NeatInterpolation.text|
Optional/build
Natural
(   λ(optional : Type)
→   λ(just : Natural → optional)
→   λ(nothing : optional)
→   just +1
)
|]
  e `assertNormalizesTo` "[+1] : Optional Natural"

optionalBuildShadowing :: TestTree
optionalBuildShadowing = testCase "handles shadowing" $ do
  e <- code [NeatInterpolation.text|
Optional/build
Integer
(   λ(optional : Type)
→   λ(x : Integer → optional)
→   λ(x : optional)
→   x@1 1
)
|]
  e `assertNormalizesTo` "[1] : Optional Integer"

optionalBuildIrreducible :: TestTree
optionalBuildIrreducible = testCase "irreducible" $ do
  e <- code [NeatInterpolation.text|
    λ(id : ∀(a : Type) → a → a)
→   Optional/build
    Bool
    (   λ(optional : Type)
    →   λ(just : Bool → optional)
    →   λ(nothing : optional)
    →   id optional (just True)
    )
|]
  assertNormalized e

fusion :: TestTree
fusion = testGroup "Optional build/fold fusion" [ fuseOptionalBF
                                                , fuseOptionalFB
                                                ]

fuseOptionalBF :: TestTree
fuseOptionalBF = testCase "fold . build" $ do
  e0 <- code [NeatInterpolation.text|
    λ(  f
    :   ∀(optional : Type)
    →   ∀(just : Text → optional)
    →   ∀(nothing : optional)
    →   optional
    )
→   Optional/fold
    Text
    (   Optional/build
        Text
        f
    )
|]
  e1 <- code [NeatInterpolation.text|
    λ(  f
    :   ∀(optional : Type)
    →   ∀(just : Text → optional)
    →   ∀(nothing : optional)
    →   optional
    )
→   f
|]
  e0 `assertNormalizesTo` (Dhall.Core.pretty e1)

fuseOptionalFB :: TestTree
fuseOptionalFB = testCase "build . fold" $ do
  test <- code [NeatInterpolation.text|
Optional/build
Text
(   Optional/fold
    Text
    (["foo"] : Optional Text)
)
|]
  test `assertNormalizesTo` "[\"foo\"] : Optional Text"
