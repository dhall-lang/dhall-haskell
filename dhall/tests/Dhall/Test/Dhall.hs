{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Test.Dhall where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Dhall (Inject, Interpret)
import Dhall.Core (Expr(..))
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Map
import qualified Dhall.Parser

tests :: TestTree
tests =
    testGroup "Input"
     [ shouldShowDetailedTypeError
     , shouldHandleBothUnionLiterals
     , shouldHaveWorkingGenericAuto
     , shouldHandleUnionsCorrectly
     , shouldTreatAConstructorStoringUnitAsEmptyAlternative
     ]

data MyType = MyType { foo :: String , bar :: Natural }

wrongDhallType :: Dhall.Type MyType 
wrongDhallType = Dhall.Type { .. }
  where expected =
          Dhall.Core.Record
            ( Dhall.Map.fromList
              [ ( "bar", Dhall.Core.Natural)
              , ( "foo", Dhall.Core.Text )
              ]
            )
        extract _ = Nothing

shouldShowDetailedTypeError :: TestTree
shouldShowDetailedTypeError = testCase "detailed TypeError" $ do
  inputEx :: Either SomeException MyType <-
        try ( Dhall.input wrongDhallType "{  bar = 0, foo = \"foo\" }") 

  let expectedMsg =
        "\ESC[1;31mError\ESC[0m: Invalid Dhall.Type                                                  \n\
        \                                                                                \n\
        \Every Type must provide an extract function that succeeds if an expression      \n\
        \matches the expected type.  You provided a Type that disobeys this contract     \n\
        \                                                                                \n\
        \The Type provided has the expected dhall type:                                  \n\
        \                                                                                \n\
        \↳ { bar : Natural, foo : Text }\n\
        \                                                                                \n\
        \and it couldn't extract a value from the well-typed expression:                 \n\
        \                                                                                \n\
        \↳ { bar = 0, foo = \"foo\" }\n\
        \                                                                                \n"

  let assertMsg = "The exception message did not match the expected output"
  
  case inputEx of
    Left ex -> assertEqual assertMsg expectedMsg (show ex)
    Right _ -> fail "The extraction using a wrong type succeded"

-- https://github.com/dhall-lang/dhall-haskell/issues/915
shouldHandleBothUnionLiterals :: TestTree
shouldHandleBothUnionLiterals = testCase "Marshal union literals" $ do
    let example :: Dhall.Type Bool
        example = Dhall.union (Dhall.constructor "Test" Dhall.bool)

    _ <- Dhall.input example "< Test : Bool >.Test True"
    _ <- Dhall.input example "< Test = True >"

    return ()

shouldTreatAConstructorStoringUnitAsEmptyAlternative :: TestTree
shouldTreatAConstructorStoringUnitAsEmptyAlternative = testCase "Handle unit constructors" $ do
    let exampleType :: Dhall.Type ()
        exampleType = Dhall.union (Dhall.constructor "A" Dhall.unit)

    () <- Dhall.input exampleType "< A >.A"

    let exampleInputType :: Dhall.InputType ()
        exampleInputType = Dhall.inputUnion (Dhall.inputConstructor "A")

    Dhall.embed exampleInputType () @=? Field (Union (Dhall.Map.singleton "A" Nothing)) "A"

data CompilerFlavor3 =
  GHC3 | GHCJS3 | Helium3
  deriving (Generic, Show, Eq)

data CompilerFlavor2 =
  GHC2 | GHCJS2
  deriving (Generic, Show, Eq)

-- https://github.com/dhall-lang/dhall-haskell/issues/926
shouldHaveWorkingGenericAuto :: TestTree
shouldHaveWorkingGenericAuto = testGroup "genericAuto"
  [ testCase "works for a three-constructor enum" $ do
      compiler <- Dhall.input Dhall.genericAuto "< GHC3 | GHCJS3 | Helium3 >.GHC3"
      assertEqual "genericAuto didn't give us what we wanted" GHC3 compiler

  , testCase "works for a two-constructor enum" $ do
      compiler <- Dhall.input Dhall.genericAuto "< GHC2 | GHCJS2 >.GHC2"
      assertEqual "genericAuto didn't give us what we wanted" GHC2 compiler
  ]

data NonEmptyUnion = N0 Bool | N1 Natural | N2 Text
    deriving (Eq, Generic, Inject, Interpret, Show)

data Enum = E0 | E1 | E2
    deriving (Eq, Generic, Inject, Interpret, Show)

data Mixed = M0 Bool | M1 | M2 ()
    deriving (Eq, Generic, Inject, Interpret, Show)

deriving instance Interpret ()

shouldHandleUnionsCorrectly :: TestTree
shouldHandleUnionsCorrectly =
  testGroup "Handle union literals"
    [ "λ(x : < N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >) → x"
        `shouldPassThrough` [ N0 True, N1 5, N2 "ABC" ]
    , "λ(x : < E0 | E1 | E2 >) → x"
        `shouldPassThrough` [ E0, E1, E2 ]
    , "λ(x : < M0 : { _1 : Bool } | M1 | M2 : { _1 : {} } >) → x"
        `shouldPassThrough` [ M0 True, M1, M2 () ]

    , "(< N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >).N0 { _1 = True }"
        `shouldMarshalInto` N0 True
    , "(< N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >).N1 { _1 = 5 }"
        `shouldMarshalInto` N1 5
    , "(< N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >).N2 { _1 = \"ABC\" }"
        `shouldMarshalInto` N2 "ABC"

    , "< N0 = { _1 = True } | N1 : { _1 : Natural } | N2 : { _1 : Text } >"
        `shouldMarshalInto` N0 True
    , "< N0 : { _1 : Bool } | N1 = { _1 = 5 } | N2 : { _1 : Text } >"
        `shouldMarshalInto` N1 5
    , "< N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 = { _1 = \"ABC\" } >"
        `shouldMarshalInto` N2 "ABC"

    , "(< E0 | E1 | E2>).E0" `shouldMarshalInto` E0
    , "(< E0 | E1 | E2>).E1" `shouldMarshalInto` E1
    , "(< E0 | E1 | E2>).E2" `shouldMarshalInto` E2

    , "(< M0 : { _1 : Bool } | M1 | M2 : { _1 : {} } >).M0 { _1 = True }"
        `shouldMarshalInto` M0 True
    , "(< M0 : { _1 : Bool } | M1 | M2 : { _1 : {} } >).M1"
        `shouldMarshalInto` M1
    , "(< M0 : { _1 : Bool } | M1 | M2 : { _1 : {} } >).M2 { _1 = {=} }"
        `shouldMarshalInto` M2 ()

    , "< M0 = { _1 = True } | M1 | M2 : { _1 : {} } >"
        `shouldMarshalInto` M0 True

    , N0 True
        `shouldInjectInto`
        "(< N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >).N0 { _1 = True }"
    , N1 5
        `shouldInjectInto`
        "(< N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >).N1 { _1 = 5 }"
    , N2 "ABC"
        `shouldInjectInto`
        "(< N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >).N2 { _1 = \"ABC\" }"

    , E0 `shouldInjectInto` "< E0 | E1 | E2 >.E0"
    , E1 `shouldInjectInto` "< E0 | E1 | E2 >.E1"
    , E2 `shouldInjectInto` "< E0 | E1 | E2 >.E2"

    , M0 True `shouldInjectInto` "(< M0 : { _1 : Bool } | M1 | M2 : { _1 : {} } >).M0 { _1 = True }"
    , M1 `shouldInjectInto` "(< M0 : { _1 : Bool } | M1 | M2 : { _1 : {} } >).M1"
    , M2 () `shouldInjectInto` "(< M0 : { _1 : Bool } | M1 | M2 : { _1 : {} } >).M2 { _1 = {=} }"
    ]
  where
    code `shouldPassThrough` values = testCase "Pass through" $ do
        f <- Dhall.input Dhall.auto code

        values @=? map f values

    code `shouldMarshalInto` expectedValue = testCase "Marshal" $ do
        actualValue <- Dhall.input Dhall.auto code
        expectedValue @=? actualValue

    value `shouldInjectInto` expectedCode = testCase "Inject" $ do
        parsedExpression <- Dhall.Core.throws (Dhall.Parser.exprFromText "(test)" expectedCode)

        resolvedExpression <- Dhall.Import.assertNoImports parsedExpression

        Dhall.Core.denote resolvedExpression @=? Dhall.embed Dhall.inject value
