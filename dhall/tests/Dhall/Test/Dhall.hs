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
import Dhall (ToDhall, FromDhall)
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
   deriving (Eq, Functor, Generic, FromDhall, Show)

tests :: TestTree
tests =
    testGroup "Input"
     [ shouldShowDetailedTypeError
     , shouldHandleUnionLiteral
     , shouldHaveWorkingRecursiveFromDhall
     , shouldHaveWorkingGenericAuto
     , shouldHandleUnionsCorrectly
     , shouldTreatAConstructorStoringUnitAsEmptyAlternative
     , shouldConvertDhallToHaskellCorrectly 
     , shouldConvertHaskellToDhallCorrectly
     ]

data MyType = MyType { foo :: String , bar :: Natural }

wrongDhallType :: Dhall.Decoder MyType
wrongDhallType = Dhall.Decoder { .. }
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
        "\ESC[1;31mError\ESC[0m: Invalid Dhall.Decoder                                               \n\
        \                                                                                \n\
        \Every Decoder must provide an extract function that succeeds if an expression   \n\
        \matches the expected type.  You provided a Decoder that disobeys this contract  \n\
        \                                                                                \n\
        \The Decoder provided has the expected dhall type:                               \n\
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
    let example :: Dhall.Decoder Bool
        example = Dhall.union (Dhall.constructor "Test" Dhall.bool)

    _ <- Dhall.input example "< Test : Bool >.Test True"

    return ()

shouldTreatAConstructorStoringUnitAsEmptyAlternative :: TestTree
shouldTreatAConstructorStoringUnitAsEmptyAlternative = testCase "Handle unit constructors" $ do
    let exampleType :: Dhall.Decoder ()
        exampleType = Dhall.union (Dhall.constructor "A" Dhall.unit)

    () <- Dhall.input exampleType "< A >.A"

    let exampleEncoder :: Dhall.Encoder ()
        exampleEncoder = Dhall.unionEncoder (Dhall.encodeConstructor "A")

    Dhall.embed exampleEncoder () @=? Field (Union (Dhall.Map.singleton "A" Nothing)) "A"

shouldHaveWorkingRecursiveFromDhall :: TestTree
shouldHaveWorkingRecursiveFromDhall = testGroup "recursive FromDhall instance"
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
    deriving (Eq, Generic, ToDhall, FromDhall, Show)

data Enum = E0 | E1 | E2
    deriving (Eq, Generic, ToDhall, FromDhall, Show)

data Records
    = R0 {}
    | R1 { a :: () }
    | R2 { x :: Double }
    | R3 { a :: (), b :: () }
    | R4 { x :: Double, y :: Double }
    deriving (Eq, Generic, ToDhall, FromDhall, Show)

data Products = P0 | P1 () | P2 Double | P3 () () | P4 Double Double
    deriving (Eq, Generic, ToDhall, FromDhall, Show)

shouldHandleUnionsCorrectly :: TestTree
shouldHandleUnionsCorrectly =
  testGroup "Handle union literals"
    [ "λ(x : < N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >) → x"
        `shouldPassThrough` [ N0 True, N1 5, N2 "ABC" ]
    , "λ(x : < E0 | E1 | E2 >) → x"
        `shouldPassThrough` [ E0, E1, E2 ]
    , "λ(x : < R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >) → x"
        `shouldPassThrough` [ R0 {}, R1 { a = () }, R2 { x = 1.0 }, R3 { a = (), b = () }, R4 { x = 1.0, y = 2.0 } ]
    , "λ(x : < P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >) → x"
        `shouldPassThrough` [ P0 , P1 (), P2 1.0, P3 () (), P4 1.0 2.0 ]

    , "λ(x : < N0 : Bool | N1 : Natural | N2 : Text >) → x"
        `shouldPassThroughSmart` [ N0 True, N1 5, N2 "ABC" ]
    , "λ(x : < R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >) → x"
        `shouldPassThroughSmart` [ R0 {}, R1 { a = () }, R2 { x = 1.0 }, R3 { a = (), b = () }, R4 { x = 1.0, y = 2.0 } ]
    , "λ(x : < P0 | P1 | P2 : Double | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >) → x"
        `shouldPassThroughSmart` [ P0 , P1 (), P2 1.0, P3 () (), P4 1.0 2.0 ]

    , "(< N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >).N0 { _1 = True }"
        `shouldMarshalInto` N0 True
    , "(< N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >).N1 { _1 = 5 }"
        `shouldMarshalInto` N1 5
    , "(< N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >).N2 { _1 = \"ABC\" }"

        `shouldMarshalInto` N2 "ABC"

    , "(< N0 : Bool | N1 : Natural | N2 : Text >).N0 True"
        `shouldMarshalIntoSmart` N0 True
    , "(< N0 : Bool | N1 : Natural | N2 : Text >).N1 5"
        `shouldMarshalIntoSmart` N1 5
    , "(< N0 : Bool | N1 : Natural | N2 : Text >).N2 \"ABC\""
        `shouldMarshalIntoSmart` N2 "ABC"

    , "(< E0 | E1 | E2>).E0" `shouldMarshalInto` E0
    , "(< E0 | E1 | E2>).E1" `shouldMarshalInto` E1
    , "(< E0 | E1 | E2>).E2" `shouldMarshalInto` E2

    , "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R0"
        `shouldMarshalInto` R0
    , "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R1 { a = {=} }"
        `shouldMarshalInto` R1 { a = () }
    , "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R2 { x = 1.0 }"
        `shouldMarshalInto` R2 { x = 1.0 }
    , "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R3 { a = {=}, b = {=} }"
        `shouldMarshalInto` R3 { a = (), b = () }
    , "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R4 { x = 1.0, y = 2.0 }"
        `shouldMarshalInto` R4 { x = 1.0, y = 2.0 }

    , "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R0"
        `shouldMarshalIntoSmart` R0
    , "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R1 { a = {=} }"
        `shouldMarshalIntoSmart` R1 { a = () }
    , "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R2 { x = 1.0 }"
        `shouldMarshalIntoSmart` R2 { x = 1.0 }
    , "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R3 { a = {=}, b = {=} }"
        `shouldMarshalIntoSmart` R3 { a = (), b = () }
    , "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R4 { x = 1.0, y = 2.0 }"
        `shouldMarshalIntoSmart` R4 { x = 1.0, y = 2.0 }

    , "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P0"
        `shouldMarshalInto` P0
    , "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P1 { _1 = {=} }"
        `shouldMarshalInto` P1 ()
    , "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P2 { _1 = 1.0 }"
        `shouldMarshalInto` P2 1.0
    , "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P3 { _1 = {=}, _2 = {=} }"
        `shouldMarshalInto` P3 () ()
    , "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P4 { _1 = 1.0, _2 = 2.0 }"
        `shouldMarshalInto` P4 1.0 2.0

    , "< P0 | P1 | P2 : Double | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P0"
        `shouldMarshalIntoSmart` P0
    , "< P0 | P1 | P2 : Double | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P1"
        `shouldMarshalIntoSmart` P1 ()
    , "< P0 | P1 | P2 : Double | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P2 1.0"
        `shouldMarshalIntoSmart` P2 1.0
    , "< P0 | P1 | P2 : Double | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P3 { _1 = {=}, _2 = {=} }"
        `shouldMarshalIntoSmart` P3 () ()
    , "< P0 | P1 | P2 : Double | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P4 { _1 = 1.0, _2 = 2.0 }"
        `shouldMarshalIntoSmart` P4 1.0 2.0

    , N0 True
        `shouldEmbedAs`
        "(< N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >).N0 { _1 = True }"
    , N1 5
        `shouldEmbedAs`
        "(< N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >).N1 { _1 = 5 }"
    , N2 "ABC"
        `shouldEmbedAs`
        "(< N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >).N2 { _1 = \"ABC\" }"

    , N0 True
        `shouldEmbedAsSmart`
        "(< N0 : Bool | N1 : Natural | N2 : Text >).N0 True"
    , N1 5
        `shouldEmbedAsSmart`
        "(< N0 : Bool | N1 : Natural | N2 : Text >).N1 5"
    , N2 "ABC"
        `shouldEmbedAsSmart`
        "(< N0 : Bool | N1 : Natural | N2 : Text >).N2 \"ABC\""

    , E0 `shouldEmbedAs` "< E0 | E1 | E2 >.E0"
    , E1 `shouldEmbedAs` "< E0 | E1 | E2 >.E1"
    , E2 `shouldEmbedAs` "< E0 | E1 | E2 >.E2"

    , R0 `shouldEmbedAs` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R0"
    , R1 { a = () } `shouldEmbedAs` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R1 { a = {=} }"
    , R2 { x = 1.0 } `shouldEmbedAs` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R2 { x = 1.0}"
    , R3 { a = (), b = () } `shouldEmbedAs` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R3 { a = {=}, b = {=} }"
    , R4 { x = 1.0, y = 2.0 } `shouldEmbedAs` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R4 { x = 1.0, y = 2.0 }"

    , R0 `shouldEmbedAsSmart` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R0"
    , R1 { a = () } `shouldEmbedAsSmart` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R1 { a = {=} }"
    , R2 { x = 1.0 } `shouldEmbedAsSmart` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R2 { x = 1.0}"
    , R3 { a = (), b = () } `shouldEmbedAsSmart` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R3 { a = {=}, b = {=} }"
    , R4 { x = 1.0, y = 2.0 } `shouldEmbedAsSmart` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R4 { x = 1.0, y = 2.0 }"

    , P0 `shouldEmbedAs` "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P0"
    , P1 () `shouldEmbedAs` "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P1 { _1 = {=} }"
    , P2 1.0 `shouldEmbedAs` "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P2 { _1 = 1.0 }"
    , P3 () () `shouldEmbedAs` "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P3 { _1 = {=}, _2 = {=} }"
    , P4 1.0 2.0 `shouldEmbedAs` "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P4 { _1 = 1.0, _2 = 2.0 }"

    , P0 `shouldEmbedAsSmart` "< P0 | P1 | P2 : Double | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P0"
    , P1 () `shouldEmbedAsSmart` "< P0 | P1 | P2 : Double | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P1"
    , P2 1.0 `shouldEmbedAsSmart` "< P0 | P1 | P2 : Double | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P2 1.0"
    , P3 () () `shouldEmbedAsSmart` "< P0 | P1 | P2 : Double | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P3 { _1 = {=}, _2 = {=} }"
    , P4 1.0 2.0 `shouldEmbedAsSmart` "< P0 | P1 | P2 : Double | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P4 { _1 = 1.0, _2 = 2.0 }"
    ]
  where
    smartOptions =
        Dhall.defaultInterpretOptions
            { Dhall.singletonConstructors = Dhall.Smart }

    code `shouldPassThrough` values = testCase "Pass through" $ do
        f <- Dhall.input Dhall.auto code

        values @=? map f values

    code `shouldPassThroughSmart` values = testCase "Pass through" $ do
        f <- Dhall.input (Dhall.autoWith smartOptions) code

        values @=? map f values

    code `shouldMarshalInto` expectedValue = testCase "Marshal" $ do
        actualValue <- Dhall.input Dhall.auto code

        expectedValue @=? actualValue

    code `shouldMarshalIntoSmart` expectedValue = testCase "Marshal" $ do
        actualValue <- Dhall.input (Dhall.autoWith smartOptions) code

        expectedValue @=? actualValue

    value `shouldEmbedAs` expectedCode = testCase "ToDhall" $ do
        parsedExpression <- Dhall.Core.throws (Dhall.Parser.exprFromText "(test)" expectedCode)

        resolvedExpression <- Dhall.Import.assertNoImports parsedExpression

        Dhall.Core.denote resolvedExpression @=? Dhall.embed Dhall.inject value

    value `shouldEmbedAsSmart` expectedCode = testCase "ToDhall" $ do
        parsedExpression <- Dhall.Core.throws (Dhall.Parser.exprFromText "(test)" expectedCode)

        resolvedExpression <- Dhall.Import.assertNoImports parsedExpression

        Dhall.Core.denote resolvedExpression @=? Dhall.embed (Dhall.injectWith smartOptions) value

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
    correspondsTo :: (Eq a, FromDhall a, Show a) => Text -> a -> TestTree
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
    correspondsTo :: ToDhall a => Text -> a -> TestTree
    expectedDhallCode `correspondsTo` haskellValue =
        testCase "Marshall Haskell to Dhall code" $ do
            let actualDhallCode =
                    Dhall.Core.pretty (Dhall.embed Dhall.inject haskellValue)

            expectedDhallCode @=? actualDhallCode
