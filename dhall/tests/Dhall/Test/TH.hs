{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Dhall.Test.TH where

import Dhall.TH   (HaskellType (..))
import Test.Tasty (TestTree)

import qualified Dhall
import qualified Dhall.TH
import qualified Test.Tasty       as Tasty
import qualified Test.Tasty.HUnit as Tasty.HUnit

Dhall.TH.makeHaskellTypeFromUnion "T" "./tests/th/example.dhall"

deriving instance Eq   T
deriving instance Show T

Dhall.TH.makeHaskellTypes
    [ MultipleConstructors "Department" "./tests/th/Department.dhall"
    , SingleConstructor "Employee" "MakeEmployee" "./tests/th/Employee.dhall"
    ]

deriving instance Eq   Department
deriving instance Show Department

deriving instance Eq   Employee
deriving instance Show Employee

tests :: TestTree
tests = Tasty.testGroup "Template Haskell" [ makeHaskellTypeFromUnion ]

makeHaskellTypeFromUnion :: TestTree
makeHaskellTypeFromUnion = Tasty.HUnit.testCase "makeHaskellTypeFromUnion" $ do
    t0 <- Dhall.input Dhall.auto "let T = ./tests/th/example.dhall in T.A { x = True, y = [ \"ABC\" ] }"

    Tasty.HUnit.assertEqual "" t0 A{ x = True, y = [ "ABC" ] }

    t1 <- Dhall.input Dhall.auto "let T = ./tests/th/example.dhall in T.B (Some [ 1 ])"

    Tasty.HUnit.assertEqual "" t1 (B (Just [ 1 ]))

    t2 <- Dhall.input Dhall.auto "let T = ./tests/th/example.dhall in T.C"

    Tasty.HUnit.assertEqual "" t2 C

    employee <- Dhall.input Dhall.auto "let Department = ./tests/th/Department.dhall in { name = \"John\", department = Department.Marketing }"

    Tasty.HUnit.assertEqual "" employee MakeEmployee{ name = "John", department = Marketing }
