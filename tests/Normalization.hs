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
                                               , fusion
                                               ]

constantFolding :: TestTree
constantFolding = testGroup "folding of constants" [ naturalPlus, optionalFold, optionalBuild ]

naturalPlus :: TestTree
naturalPlus = testCase "natural plus" $ do
  e <- code "+1 + +2"
  e `assertNormalizesTo` "+3"

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
  j <- test "just \"foo\""
  j `assertNormalizesTo` "+42"
  n <- test "nothing"
  n `assertNormalizesTo` "+2"
  where
    test e = code [NeatInterpolation.text|
Optional/fold
Text
(   Optional/build
    Text
    (   λ(optional : Type)
    →   λ(just : Text → optional)
    →   λ(nothing : optional)
    →   $e
    )
)
Natural
(λ(j : Text) → +42)
+2
|]

fuseOptionalFB :: TestTree
fuseOptionalFB = testCase "build . fold" $ do
  test <- code [NeatInterpolation.text|
Optional/build
Natural
(   λ(optional : Type)
→   λ(just : Natural → optional)
→   λ(nothing : optional)
→   just
    (   Optional/fold
        Text
        (["foo"] : Optional Text)
        Natural
        (λ(just : Text) → +42)
        +2
    )
)
|]
  test `assertNormalizesTo` "[+42] : Optional Natural"
