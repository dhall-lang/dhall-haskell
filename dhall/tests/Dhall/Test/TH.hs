{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Dhall.Test.TH where

import Control.Exception      (throwIO)
import Data.Either.Validation (Validation (..))
import Data.Time              (TimeOfDay (..), TimeZone (..), fromGregorian)
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
    , SingleConstructor "TimeExample" "TimeExample" "./tests/th/Time.dhall"
    ]

deriving instance Eq   Department
deriving instance Show Department

deriving instance Eq   Employee
deriving instance Show Employee

deriving instance Eq   TimeExample
deriving instance Show TimeExample

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

    timex <- Dhall.input Dhall.auto "let T = ./tests/th/Time.dhall in { txTime = 21:12:00, txDate = 1976-04-01, txTimeZone = +05:00 } : T"

    Tasty.HUnit.assertEqual "" timex TimeExample { txTime = tod, txDate = day, txTimeZone = tz}

    where
      tod = TimeOfDay { todHour = 21, todMin = 12, todSec = 0 }
      day = fromGregorian 1976 4 1
      tz = TimeZone { timeZoneMinutes = 300, timeZoneSummerOnly = False, timeZoneName = "" }
  

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

  
Dhall.TH.makeHaskellTypesWith (Dhall.TH.defaultGenerateOptions
    { Dhall.TH.constructorModifier = ("My" <>)
    , Dhall.TH.fieldModifier = ("my" <>) . Data.Text.toTitle
    , Dhall.TH.generateFromDhallInstance = False
    , Dhall.TH.generateToDhallInstance = False
    })
    [ SingleConstructor "MyHKSingle" "HKSingle" "./tests/th/HigherKindSingle.dhall"
    , MultipleConstructors "MyHKUnion" "./tests/th/HigherKindUnion.dhall"
    , SingleConstructor "MyHKInnerS" "HKInnerS" "(./tests/th/HigherKindNested.dhall).InnerS"
    , MultipleConstructors "MyHKInnerU" "(./tests/th/HigherKindNested.dhall).InnerU"
    , SingleConstructor "MyHKOuter" "HKOuter" "(./tests/th/HigherKindNested.dhall).Outer"
    ]

type MyHKSingle_ = MyHKSingle Maybe Int
type MyHKUnion_ = MyHKUnion Bool Int
type MyHKInnerS_ = MyHKInnerS Bool Data.Text.Text
type MyHKInnerU_ = MyHKInnerU Int Data.Text.Text
type MyHKOuter_ = MyHKOuter Bool Int

deriving instance Eq MyHKSingle_
deriving instance Show MyHKSingle_
deriving instance Dhall.Generic MyHKSingle_
instance Dhall.FromDhall MyHKSingle_
instance Dhall.ToDhall MyHKSingle_

deriving instance Eq MyHKUnion_
deriving instance Show MyHKUnion_
deriving instance Dhall.Generic MyHKUnion_
instance Dhall.FromDhall MyHKUnion_
instance Dhall.ToDhall MyHKUnion_

deriving instance Eq MyHKInnerS_
deriving instance Show MyHKInnerS_
deriving instance Dhall.Generic MyHKInnerS_
instance Dhall.FromDhall MyHKInnerS_
instance Dhall.ToDhall MyHKInnerS_

deriving instance Eq MyHKInnerU_
deriving instance Show MyHKInnerU_
deriving instance Dhall.Generic MyHKInnerU_
instance Dhall.FromDhall MyHKInnerU_
instance Dhall.ToDhall MyHKInnerU_

deriving instance Eq MyHKOuter_
deriving instance Show MyHKOuter_
deriving instance Dhall.Generic MyHKOuter_
instance Dhall.FromDhall MyHKOuter_
instance Dhall.ToDhall MyHKOuter_

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

    let textHKSingle = "let T = (./tests/th/HigherKindSingle.dhall) Optional Int in T { foo = +1, bar = Some +2, bam = \"\" }"
        refHKSingle = MyHKSingle { myFoo = 1, myBar = Just 2, myBam = "" } :: MyHKSingle_
    myTest textHKSingle refHKSingle

    let textHKUnion0 = "let T = (./tests/th/HigherKindUnion.dhall) Bool Int in T.Foo True"
        refHKUnion0 = MyFoo True :: MyHKUnion_
    myTest textHKUnion0 refHKUnion0

    let textHKUnion1 = "let T = (./tests/th/HigherKindUnion.dhall) Bool Int in T.Bar +1"
        refHKUnion1 = MyBar 1 :: MyHKUnion_
    myTest textHKUnion1 refHKUnion1

    let textHKOuter = "let T = (./tests/th/HigherKindNested.dhall) in T.Outer Bool Int { oFoo = { iFoo = True, iBar = \"\" }, oBar = T.InnerU.Ifoo +1 }"
        refHKOuter = MyHKOuter { myObar = MyIfoo 1, myOfoo = (MyHKInnerS { myIbar = "", myIfoo = True }) } :: MyHKOuter_
    myTest textHKOuter refHKOuter

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
