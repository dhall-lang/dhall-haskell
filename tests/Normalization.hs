{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Normalization (normalizationTests) where

import qualified Control.Exception
import qualified Data.Text
import qualified Data.Text.Lazy
import           Dhall.Core
import           Dhall.Parser (Src)
import qualified Dhall.Parser
import qualified Dhall.Import
import           Dhall.TypeCheck
import qualified NeatInterpolation
import           Test.Tasty
import           Test.Tasty.HUnit

code :: Data.Text.Text -> IO (Expr Src X)
code strictText = do
    let lazyText = Data.Text.Lazy.fromStrict strictText
    expr0 <- case Dhall.Parser.exprFromText mempty lazyText of
        Left parseError -> Control.Exception.throwIO parseError
        Right expr0     -> return expr0
    expr1 <- Dhall.Import.load expr0
    case Dhall.TypeCheck.typeOf expr1 of
        Left typeError -> Control.Exception.throwIO typeError
        Right _        -> return ()
    return expr1

normalizationTests :: TestTree
normalizationTests = testGroup "normalization" [ constantFolding
                                               , fusion
                                               ]

constantFolding :: TestTree
constantFolding = testGroup "folding of constants" [ naturalPlus, optionalFold, optionalBuild ]

naturalPlus :: TestTree
naturalPlus = testCase "natural plus" $ do
  e <- code "+1 + +2"
  isNormalized e @?= False
  normalize' e @?= "+3"

optionalFold :: TestTree
optionalFold = testGroup "Optional/fold" [ just, nothing ]
  where test label inp out = testCase label $ do
             e <- code [NeatInterpolation.text|
Optional/fold Text ([$inp] : Optional Text) Natural (λ(j : Text) → +1) +2
|]
             isNormalized e @?= False
             normalize' e @?= out
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
  isNormalized e @?= False
  normalize' e @?= "[+1] : Optional Natural"

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
  isNormalized e @?= False
  normalize' e @?= "[1] : Optional Integer"

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
  isNormalized e @?= True
-- normalize e @?= e

normalize' :: Expr Src X -> Data.Text.Lazy.Text
normalize' = Dhall.Core.pretty . normalize

fusion :: TestTree
fusion = testGroup "Optional build/fold fusion" [ fuseOptionalBF
                                                , fuseOptionalFB
                                                ]

fuseOptionalBF :: TestTree
fuseOptionalBF = testCase "fold . build" $ do
  j <- test "just \"foo\""
  isNormalized j @?= False
  normalize' j @?= "+42"
  n <- test "nothing"
  isNormalized n @?= False
  normalize' n @?= "+2"
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
  isNormalized test @?= False
  normalize' test @?= "[+42] : Optional Natural"
