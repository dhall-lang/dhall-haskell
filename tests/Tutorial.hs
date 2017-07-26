{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tutorial where

import qualified Data.Vector
import qualified Dhall
import qualified Test.Tasty
import qualified Test.Tasty.HUnit
import qualified Util

import Dhall (Inject)
import GHC.Generics (Generic)
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
            , _Functions_2
            ]
        ]

_Interpolation_0 :: TestTree
_Interpolation_0 = Test.Tasty.HUnit.testCase "Example #0" (do
    e <- Util.code
        "    let name = \"John Doe\"                                 \n\
        \in  let age  = 21                                           \n\
        \in  \"My name is ${name} and my age is ${Integer/show age}\"\n"
    Util.assertNormalizesTo e "\"My name is John Doe and my age is 21\"" )

_Interpolation_1 :: TestTree
_Interpolation_1 = Test.Tasty.HUnit.testCase "Example #1" (do
    e <- Util.code
        "''                            \n\
        \    for file in *; do         \n\
        \      echo \"Found ''${file}\"\n\
        \    done                      \n\
        \''                            \n"
    Util.assertNormalized e )

_Functions_0 :: TestTree
_Functions_0 = Test.Tasty.HUnit.testCase "Example #0" (do
    let text = "\\(n : Bool) -> [ n && True, n && False, n || True, n || False ]"
    makeBools <- Dhall.input Dhall.auto text
    makeBools True @?= Data.Vector.fromList [True,False,True,True] )

_Functions_1 :: TestTree
_Functions_1 = Test.Tasty.HUnit.testCase "Example #1" (do
    let text = "λ(x : Bool) → λ(y : Bool) → x && y"
    makeBools <- Dhall.input Dhall.auto text
    makeBools True False @?= False )

data Example0 = Example0 { foo :: Bool, bar :: Bool }
    deriving (Generic, Inject)

_Functions_2 :: TestTree
_Functions_2 = Test.Tasty.HUnit.testCase "Example #2" (do
    f <- Dhall.input Dhall.auto "λ(r : { foo : Bool, bar : Bool }) → r.foo && r.bar"
    f (Example0 { foo = True, bar = False }) @?= False
    f (Example0 { foo = True, bar = True  }) @?= True )
