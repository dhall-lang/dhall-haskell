{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Dhall.Test.TH where

import Control.Exception      (throwIO)
import Data.Either.Validation (Validation (..))
import Dhall.TH               (HaskellType (..))
import Test.Tasty             (TestTree)

import qualified Data.Text
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

Dhall.TH.makeHaskellTypesWith (Dhall.TH.defaultGenerateOptions
    { Dhall.TH.constructorModifier = ("My" <>)
    , Dhall.TH.fieldModifier = ("my" <>) . Data.Text.toTitle
    })
    [ MultipleConstructors "MyT" "./tests/th/example.dhall"
    , MultipleConstructors "MyDepartment" "./tests/th/Department.dhall"
    , SingleConstructor "MyEmployee" "Employee" "./tests/th/Employee.dhall"
    ]

deriving instance Eq   MyT
deriving instance Eq   MyDepartment
deriving instance Eq   MyEmployee
deriving instance Show MyT
deriving instance Show MyDepartment
deriving instance Show MyEmployee

testMakeHaskellTypesWith :: TestTree
testMakeHaskellTypesWith = Tasty.HUnit.testCase "makeHaskellTypesWith" $ do
    let text0 = "let T = ./tests/th/example.dhall in T.A { x = True, y = [] : List Text }"
        ref0 = MyA{ myX = True, myY = [] }
    myTest text0 ref0

    let text1 = "let T = ./tests/th/example.dhall in T.B (None (List Natural))"
        ref1 = MyB Nothing
    myTest text1 ref1

    let text2 = "let T = ./tests/th/example.dhall in T.C"
        ref2 = MyC
    myTest text2 ref2

    let textDepartment = "let T = ./tests/th/Department.dhall in T.Sales"
        refDepartment = MySales
    myTest textDepartment refDepartment

    let textEmployee = "let T = ./tests/th/Department.dhall in T.Sales"
        refEmployee = MyEmployee{ myName = "", myDepartment = MySales }
    myTest textEmployee refEmployee
    where
        myTest text ref = do
            expr <- Dhall.inputExpr text
            t <- case Dhall.extract Dhall.auto expr of
                Failure e -> throwIO e
                Success t -> return t

            Tasty.HUnit.assertEqual "" t ref
            Tasty.HUnit.assertEqual "" expr $ Dhall.embed Dhall.inject ref

Dhall.TH.makeHaskellTypesWith (Dhall.TH.defaultGenerateOptions
    { Dhall.TH.constructorModifier = ("NoFromDhall" <>)
    , Dhall.TH.fieldModifier = ("noFromDhall" <>) . Data.Text.toTitle
    , Dhall.TH.generateFromDhallInstance = False
    })
    [ MultipleConstructors "NoFromDhallT" "./tests/th/example.dhall"
    ]

instance Dhall.FromDhall NoFromDhallT

Dhall.TH.makeHaskellTypesWith (Dhall.TH.defaultGenerateOptions
    { Dhall.TH.constructorModifier = ("NoToDhall" <>)
    , Dhall.TH.fieldModifier = ("noToDhall" <>) . Data.Text.toTitle
    , Dhall.TH.generateToDhallInstance = False
    })
    [ MultipleConstructors "NoToDhallT" "./tests/th/example.dhall"
    ]

instance Dhall.ToDhall NoToDhallT

Dhall.TH.makeHaskellTypesWith (Dhall.TH.defaultGenerateOptions
    { Dhall.TH.constructorModifier = ("NoInstances" <>)
    , Dhall.TH.fieldModifier = ("noInstances" <>) . Data.Text.toTitle
    , Dhall.TH.generateFromDhallInstance = False
    , Dhall.TH.generateToDhallInstance = False
    })
    [ MultipleConstructors "NoInstancesT" "./tests/th/example.dhall"
    ]

deriving instance Dhall.Generic NoInstancesT
instance Dhall.FromDhall NoInstancesT
instance Dhall.ToDhall NoInstancesT

Dhall.TH.makeHaskellTypesWith (Dhall.TH.defaultGenerateOptions
    { Dhall.TH.constructorModifier = ("Strict" <>)
    , Dhall.TH.fieldModifier = ("strict" <>) . Data.Text.toTitle
    , Dhall.TH.makeStrict = True
    })
    [ MultipleConstructors "StrictFields" "./tests/th/example.dhall"
    ]
