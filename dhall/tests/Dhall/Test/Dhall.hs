{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Test.Dhall where

import Control.Exception (SomeException, try)
import Data.Fix (Fix(..))
import Data.Sequence (Seq)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)
import Dhall (Inject, Interpret)
import Dhall.Core (Expr(..))
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text.Lazy
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Map
import qualified Dhall.Parser

data ExprF expr
   = LitF Natural
   | AddF expr expr
   | MulF expr expr
   deriving (Eq, Functor, Generic, Interpret, Show)

tests :: TestTree
tests =
    testGroup "Input"
     [ shouldShowDetailedTypeError
     , shouldHandleUnionLiteral
     , shouldHaveWorkingRecursiveInterpret
     , shouldHaveWorkingGenericAuto
     , shouldHandleUnionsCorrectly
     , shouldTreatAConstructorStoringUnitAsEmptyAlternative
     , shouldConvertDhallToHaskellCorrectly 
     , shouldConvertHaskellToDhallCorrectly
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
        extract expr = Dhall.typeError expected expr

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
shouldHandleUnionLiteral :: TestTree
shouldHandleUnionLiteral = testCase "Marshal union literals" $ do
    let example :: Dhall.Type Bool
        example = Dhall.union (Dhall.constructor "Test" Dhall.bool)

    _ <- Dhall.input example "< Test : Bool >.Test True"

    return ()

shouldTreatAConstructorStoringUnitAsEmptyAlternative :: TestTree
shouldTreatAConstructorStoringUnitAsEmptyAlternative = testCase "Handle unit constructors" $ do
    let exampleType :: Dhall.Type ()
        exampleType = Dhall.union (Dhall.constructor "A" Dhall.unit)

    () <- Dhall.input exampleType "< A >.A"

    let exampleInputType :: Dhall.InputType ()
        exampleInputType = Dhall.inputUnion (Dhall.inputConstructor "A")

    Dhall.embed exampleInputType () @=? Field (Union (Dhall.Map.singleton "A" Nothing)) "A"

shouldHaveWorkingRecursiveInterpret :: TestTree
shouldHaveWorkingRecursiveInterpret = testGroup "recursive Interpret instance"
    [ testCase "works for a recursive expression" $ do
        actual <- Dhall.input Dhall.auto "./tests/recursive/expr0.dhall"

        expected @=? actual
    , testCase "passes a shadowing sanity check" $ do
        actual <- Dhall.input Dhall.auto "./tests/recursive/expr1.dhall"

        expected @=? actual
    ]
  where
    expected =
        Fix
            (AddF
                (Fix (MulF (Fix (LitF 3)) (Fix (LitF 7))))
                (Fix (AddF (Fix (LitF 1)) (Fix (LitF 2))))
            )

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
    [ "λ(x : < N0 : Bool | N1 : Natural | N2 : Text >) → x"
        `shouldPassThrough` [ N0 True, N1 5, N2 "ABC" ]
    , "λ(x : < E0 | E1 | E2 >) → x"
        `shouldPassThrough` [ E0, E1, E2 ]
    , "λ(x : < M0 : Bool | M1 | M2 >) → x"
        `shouldPassThrough` [ M0 True, M1, M2 () ]

    , "(< N0 : Bool | N1 : Natural | N2 : Text >).N0 True"
        `shouldMarshalInto` N0 True
    , "(< N0 : Bool | N1 : Natural | N2 : Text >).N1 5"
        `shouldMarshalInto` N1 5
    , "(< N0 : Bool | N1 : Natural | N2 : Text >).N2 \"ABC\""
        `shouldMarshalInto` N2 "ABC"

    , "(< E0 | E1 | E2>).E0" `shouldMarshalInto` E0
    , "(< E0 | E1 | E2>).E1" `shouldMarshalInto` E1
    , "(< E0 | E1 | E2>).E2" `shouldMarshalInto` E2

    , "(< M0 : Bool | M1 | M2 >).M0 True"
        `shouldMarshalInto` M0 True
    , "(< M0 : Bool | M1 | M2 >).M1"
        `shouldMarshalInto` M1
    , "(< M0 : Bool | M1 | M2 >).M2"
        `shouldMarshalInto` M2 ()

    , N0 True
        `shouldInjectInto`
        "(< N0 : Bool | N1 : Natural | N2 : Text >).N0 True"
    , N1 5
        `shouldInjectInto`
        "(< N0 : Bool | N1 : Natural | N2 : Text >).N1 5"
    , N2 "ABC"
        `shouldInjectInto`
        "(< N0 : Bool | N1 : Natural | N2 : Text >).N2 \"ABC\""

    , E0 `shouldInjectInto` "< E0 | E1 | E2 >.E0"
    , E1 `shouldInjectInto` "< E0 | E1 | E2 >.E1"
    , E2 `shouldInjectInto` "< E0 | E1 | E2 >.E2"

    , M0 True `shouldInjectInto` "(< M0 : Bool | M1 | M2 >).M0 True"
    , M1 `shouldInjectInto` "(< M0 : Bool | M1 | M2 >).M1"
    , M2 () `shouldInjectInto` "(< M0 : Bool | M1 | M2 >).M2"
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

shouldConvertDhallToHaskellCorrectly :: TestTree
shouldConvertDhallToHaskellCorrectly =
    testGroup
        "Marshall Dhall code to Haskell"
        [ "True" `correspondsTo` True
        , "False" `correspondsTo` False
        , "2" `correspondsTo` (2 :: Natural)
        , "+2" `correspondsTo` (2 :: Integer)
        , "2.0" `correspondsTo` (2.0 :: Double)
        , "2.0" `correspondsTo` (2.0 :: Scientific)
        , "\"ABC\"" `correspondsTo` ("ABC" :: Data.Text.Text)
        , "\"ABC\"" `correspondsTo` ("ABC" :: Data.Text.Lazy.Text)
        , "\"ABC\"" `correspondsTo` ("ABC" :: String)
        , "Some 2" `correspondsTo` (Just 2 :: Maybe Natural)
        , "None Natural" `correspondsTo` (Nothing :: Maybe Natural)
        , "[ 2, 3, 5 ]" `correspondsTo` ([ 2, 3, 5 ] :: Seq Natural)
        , "[ 2, 3, 5 ]" `correspondsTo` ([ 2, 3, 5 ] :: [Natural])
        , "[ 2, 3, 5 ]" `correspondsTo` ([ 2, 3, 5 ] :: Vector Natural)
        , "[] : List Natural" `correspondsTo` ([] :: [Natural])
        , "{=}" `correspondsTo` ()
        , "{ _1 = True, _2 = {=} }" `correspondsTo` (True, ())
        ]
  where
    correspondsTo :: (Eq a, Interpret a, Show a) => Text -> a -> TestTree
    dhallCode `correspondsTo` expectedHaskellValue =
      testCase "Marshall Dhall code to Haskell" $ do
          actualHaskellValue <- Dhall.input Dhall.auto dhallCode

          expectedHaskellValue @=? actualHaskellValue

shouldConvertHaskellToDhallCorrectly :: TestTree
shouldConvertHaskellToDhallCorrectly =
    testGroup
        "Marshall Haskell to Dhall code"
        [ "True" `correspondsTo` True
        , "False" `correspondsTo` False
        , "2" `correspondsTo` (2 :: Natural)
        , "+2" `correspondsTo` (2 :: Integer)
        , "2.0" `correspondsTo` (2.0 :: Double)
        , "2.0" `correspondsTo` (2.0 :: Scientific)
        , "\"ABC\"" `correspondsTo` ("ABC" :: Data.Text.Text)
        , "\"ABC\"" `correspondsTo` ("ABC" :: Data.Text.Lazy.Text)
        , "\"ABC\"" `correspondsTo` ("ABC" :: String)
        , "Some 2" `correspondsTo` (Just 2 :: Maybe Natural)
        , "None Natural" `correspondsTo` (Nothing :: Maybe Natural)
        , "[ 2, 3, 5 ]" `correspondsTo` ([ 2, 3, 5 ] :: Seq Natural)
        , "[ 2, 3, 5 ]" `correspondsTo` ([ 2, 3, 5 ] :: [Natural])
        , "[ 2, 3, 5 ]" `correspondsTo` ([ 2, 3, 5 ] :: Vector Natural)
        , "[] : List Natural" `correspondsTo` ([] :: [Natural])
        , "{=}" `correspondsTo` ()
        , "{ _1 = True, _2 = {=} }" `correspondsTo` (True, ())
        ]
  where
    correspondsTo :: Inject a => Text -> a -> TestTree
    expectedDhallCode `correspondsTo` haskellValue =
        testCase "Marshall Haskell to Dhall code" $ do
            let actualDhallCode =
                    Dhall.Core.pretty (Dhall.embed Dhall.inject haskellValue)

            expectedDhallCode @=? actualDhallCode
