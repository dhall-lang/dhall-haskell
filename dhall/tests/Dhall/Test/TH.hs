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

Dhall.TH.makeHaskellTypes
  [ SingleConstructor "Bar" "MakeBar" "(./tests/th/issue2066.dhall).Bar"
  , SingleConstructor "Foo" "MakeFoo" "(./tests/th/issue2066.dhall).Foo"
  , MultipleConstructors "Qux" "(./tests/th/issue2066.dhall).Qux"
  ]

deriving instance Eq   Bar
deriving instance Show Bar

deriving instance Eq   Foo
deriving instance Show Foo

deriving instance Eq   Qux
deriving instance Show Qux

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

    qux <- Dhall.input Dhall.auto "let T = ./tests/th/issue2066.dhall in T.Qux.Foo { foo = +2, bar = { baz = +3 } }"

    Tasty.HUnit.assertEqual "" qux (Foo MakeFoo{ foo = 2, bar = MakeBar{ baz = 3 } })
