{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Tutorial where

import qualified Data.Text.Lazy
import qualified Data.Vector
import qualified Dhall
import qualified NeatInterpolation
import qualified Test.Tasty
import qualified Test.Tasty.HUnit
import qualified Util

import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@?=))

tutorialTests :: TestTree
tutorialTests =
    Test.Tasty.testGroup "tutorial"
        [ Test.Tasty.testGroup "Interpolation"
            [ _Interpolation_0
            , _Interpolation_1
            ]
        , Test.Tasty.testGroup "Functions"
            [ _Functions_0
            , _Functions_1
            ]
        ]

_Interpolation_0 :: TestTree
_Interpolation_0 = Test.Tasty.HUnit.testCase "Example #0" (do
    e <- Util.code [NeatInterpolation.text|
    let name = "John Doe"
in  let age  = 21
in  "My name is $${name} and my age is $${Integer/show age}"
|]
    Util.assertNormalizesTo e "\"My name is John Doe and my age is 21\"" )

_Interpolation_1 :: TestTree
_Interpolation_1 = Test.Tasty.HUnit.testCase "Example #1" (do
    e <- Util.code [NeatInterpolation.text|
''
    for file in *; do
      echo "Found ''$${file}"
    done
''
|]
    Util.assertNormalized e )

_Functions_0 :: TestTree
_Functions_0 = Test.Tasty.HUnit.testCase "Example #0" (do
    let text = Data.Text.Lazy.fromStrict [NeatInterpolation.text|
\(n : Bool) ->
    [ n && True, n && False, n || True, n || False ]
|]
    makeBools <- Dhall.input Dhall.auto text
    makeBools True @?= Data.Vector.fromList [True,False,True,True] )

_Functions_1 :: TestTree
_Functions_1 = Test.Tasty.HUnit.testCase "Example #1" (do
    let text = Data.Text.Lazy.fromStrict [NeatInterpolation.text|
λ(x : Bool) → λ(y : Bool) → x && y
|]
    makeBools <- Dhall.input Dhall.auto text
    makeBools True False @?= False )
