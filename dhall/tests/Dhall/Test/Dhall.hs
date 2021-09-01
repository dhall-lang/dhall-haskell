{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dhall.Test.Dhall where

import Control.Exception      (SomeException, throwIO, try)
import Data.Either.Validation (validationToEither)
import Data.Fix               (Fix (..))
import Data.Functor.Classes   (Eq1 (..), Show1 (..))
import Data.List.NonEmpty     (NonEmpty (..))
import Data.Maybe             (isJust)
import Data.Scientific        (Scientific)
import Data.Sequence          (Seq)
import Data.Text              (Text)
import Data.Vector            (Vector)
import Data.Void              (Void)
import Dhall                  (FromDhall, ToDhall)
import Dhall.Core             (Expr (..))
import GHC.Generics           (Generic, Rep)
import Numeric.Natural        (Natural)
import System.Timeout         (timeout)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Functor.Classes as Classes
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
   deriving (Eq, Functor, Generic, FromDhall, ToDhall, Show)

instance Eq1 ExprF where
    liftEq _  (LitF aL) (LitF aR) = aL == aR
    liftEq eq (AddF aL bL) (AddF aR bR) = (aL `eq` aR) && (bL `eq` bR)
    liftEq eq (MulF aL bL) (MulF aR bR) = (aL `eq` aR) && (bL `eq` bR)
    liftEq _ _ _ = False

instance Show1 ExprF where
    liftShowsPrec _  _ d (LitF a) = showsPrec d a
    liftShowsPrec sp _ d (AddF a b) = Classes.showsBinaryWith sp sp "AddF" d a b
    liftShowsPrec sp _ d (MulF a b) = Classes.showsBinaryWith sp sp "MulF" d a b

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
     , shouldShowCorrectErrorForInvalidDecoderInUnion
     ]

data MyType = MyType { foo :: String , bar :: Natural }

wrongDhallType :: Dhall.Decoder MyType
wrongDhallType = Dhall.Decoder {..}
  where expected = pure $
          Dhall.Core.Record
            ( Dhall.Map.fromList
              [ ( "bar", Dhall.Core.makeRecordField Dhall.Core.Natural)
              , ( "foo", Dhall.Core.makeRecordField Dhall.Core.Text )
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
        \Every Decoder must provide an extract function that does not fail with a type   \n\
        \error if an expression matches the expected type.  You provided a Decoder that  \n\
        \disobeys this contract                                                          \n\
        \                                                                                \n\
        \The Decoder provided has the expected dhall type:                               \n\
        \                                                                                \n\
        \↳ { bar : Natural, foo : Text }\n\
        \                                                                                \n\
        \and it threw a type error during extraction from the well-typed expression:     \n\
        \                                                                                \n\
        \↳ { bar = 0, foo = \"foo\" }\n\
        \                                                                                \n"

  let assertMsg = "The exception message did not match the expected output"

  case inputEx of
    Left ex -> assertEqual assertMsg expectedMsg (show ex)
    Right _ -> fail "The extraction using a wrong type succeeded"

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

    let a = Dhall.Core.makeFieldSelection "A"
    Dhall.embed exampleEncoder () @=? Field (Union (Dhall.Map.singleton "A" Nothing)) a

newtype RecursiveType a = RecursiveType (RecursiveType a)
    deriving Generic

instance FromDhall (RecursiveType a)

shouldHaveWorkingRecursiveFromDhall :: TestTree
shouldHaveWorkingRecursiveFromDhall = testGroup "recursive FromDhall instance"
    [ testCase "works for a recursive expression" $ do
        actual <- Dhall.input Dhall.auto "./tests/recursive/expr0.dhall"

        expected @=? actual
    , testCase "roundtrips (one-way)" $ do
        let expr = Dhall.embed Dhall.inject expected
        actual <- either throwIO pure . validationToEither . Dhall.extract Dhall.auto $ expr

        expected @=? actual
    , testCase "passes a shadowing sanity check" $ do
        actual <- Dhall.input Dhall.auto "./tests/recursive/expr1.dhall"

        expected @=? actual
    , testCase "terminate if type is recursive (monomorphic)" $ do
        actual <- timeout (1 * 1000000) $ do
            !typ <- return $ Dhall.expected (Dhall.auto :: Dhall.Decoder (RecursiveType Void))
            return typ
        assertBool "Does not terminate!" $ isJust actual
    , testCase "terminate if type is recursive (polymorphic)" $ do
        actual <- timeout (1 * 1000000) $ do
            !typ <- return $ Dhall.expected (Dhall.auto :: Dhall.Decoder (RecursiveType a))
            return typ
        assertBool "Does not terminate!" $ isJust actual
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
        `shouldPassThroughWrapped` [ N0 True, N1 5, N2 "ABC" ]
    , "λ(x : < E0 | E1 | E2 >) → x"
        `shouldPassThroughWrapped` [ E0, E1, E2 ]
    , "λ(x : < R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >) → x"
        `shouldPassThroughWrapped` [ R0 {}, R1 { a = () }, R2 { x = 1.0 }, R3 { a = (), b = () }, R4 { x = 1.0, y = 2.0 } ]
    , "λ(x : < P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >) → x"
        `shouldPassThroughWrapped` [ P0 , P1 (), P2 1.0, P3 () (), P4 1.0 2.0 ]

    , "λ(x : < N0 : Bool | N1 : Natural | N2 : Text >) → x"
        `shouldPassThroughSmart` [ N0 True, N1 5, N2 "ABC" ]
    , "λ(x : < R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >) → x"
        `shouldPassThroughSmart` [ R0 {}, R1 { a = () }, R2 { x = 1.0 }, R3 { a = (), b = () }, R4 { x = 1.0, y = 2.0 } ]
    , "λ(x : < P0 | P1 | P2 : Double | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >) → x"
        `shouldPassThroughSmart` [ P0 , P1 (), P2 1.0, P3 () (), P4 1.0 2.0 ]

    , "(< N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >).N0 { _1 = True }"
        `shouldMarshalIntoWrapped` N0 True
    , "(< N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >).N1 { _1 = 5 }"
        `shouldMarshalIntoWrapped` N1 5
    , "(< N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >).N2 { _1 = \"ABC\" }"

        `shouldMarshalIntoWrapped` N2 "ABC"

    , "(< N0 : Bool | N1 : Natural | N2 : Text >).N0 True"
        `shouldMarshalIntoSmart` N0 True
    , "(< N0 : Bool | N1 : Natural | N2 : Text >).N1 5"
        `shouldMarshalIntoSmart` N1 5
    , "(< N0 : Bool | N1 : Natural | N2 : Text >).N2 \"ABC\""
        `shouldMarshalIntoSmart` N2 "ABC"

    , "(< E0 | E1 | E2>).E0" `shouldMarshalIntoWrapped` E0
    , "(< E0 | E1 | E2>).E1" `shouldMarshalIntoWrapped` E1
    , "(< E0 | E1 | E2>).E2" `shouldMarshalIntoWrapped` E2

    , "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R0"
        `shouldMarshalIntoWrapped` R0
    , "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R1 { a = {=} }"
        `shouldMarshalIntoWrapped` R1 { a = () }
    , "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R2 { x = 1.0 }"
        `shouldMarshalIntoWrapped` R2 { x = 1.0 }
    , "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R3 { a = {=}, b = {=} }"
        `shouldMarshalIntoWrapped` R3 { a = (), b = () }
    , "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R4 { x = 1.0, y = 2.0 }"
        `shouldMarshalIntoWrapped` R4 { x = 1.0, y = 2.0 }

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
        `shouldMarshalIntoWrapped` P0
    , "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P1 { _1 = {=} }"
        `shouldMarshalIntoWrapped` P1 ()
    , "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P2 { _1 = 1.0 }"
        `shouldMarshalIntoWrapped` P2 1.0
    , "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P3 { _1 = {=}, _2 = {=} }"
        `shouldMarshalIntoWrapped` P3 () ()
    , "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P4 { _1 = 1.0, _2 = 2.0 }"
        `shouldMarshalIntoWrapped` P4 1.0 2.0

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
        `shouldEmbedAsWrapped`
        "(< N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >).N0 { _1 = True }"
    , N1 5
        `shouldEmbedAsWrapped`
        "(< N0 : { _1 : Bool } | N1 : { _1 : Natural } | N2 : { _1 : Text } >).N1 { _1 = 5 }"
    , N2 "ABC"
        `shouldEmbedAsWrapped`
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

    , E0 `shouldEmbedAsWrapped` "< E0 | E1 | E2 >.E0"
    , E1 `shouldEmbedAsWrapped` "< E0 | E1 | E2 >.E1"
    , E2 `shouldEmbedAsWrapped` "< E0 | E1 | E2 >.E2"

    , R0 `shouldEmbedAsWrapped` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R0"
    , R1 { a = () } `shouldEmbedAsWrapped` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R1 { a = {=} }"
    , R2 { x = 1.0 } `shouldEmbedAsWrapped` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R2 { x = 1.0}"
    , R3 { a = (), b = () } `shouldEmbedAsWrapped` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R3 { a = {=}, b = {=} }"
    , R4 { x = 1.0, y = 2.0 } `shouldEmbedAsWrapped` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R4 { x = 1.0, y = 2.0 }"

    , R0 `shouldEmbedAsSmart` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R0"
    , R1 { a = () } `shouldEmbedAsSmart` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R1 { a = {=} }"
    , R2 { x = 1.0 } `shouldEmbedAsSmart` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R2 { x = 1.0}"
    , R3 { a = (), b = () } `shouldEmbedAsSmart` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R3 { a = {=}, b = {=} }"
    , R4 { x = 1.0, y = 2.0 } `shouldEmbedAsSmart` "< R0 | R1 : { a : {} } | R2 : { x : Double } | R3 : { a : {}, b : {} } | R4 : { x : Double, y : Double } >.R4 { x = 1.0, y = 2.0 }"

    , P0 `shouldEmbedAsWrapped` "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P0"
    , P1 () `shouldEmbedAsWrapped` "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P1 { _1 = {=} }"
    , P2 1.0 `shouldEmbedAsWrapped` "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P2 { _1 = 1.0 }"
    , P3 () () `shouldEmbedAsWrapped` "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P3 { _1 = {=}, _2 = {=} }"
    , P4 1.0 2.0 `shouldEmbedAsWrapped` "< P0 | P1 : { _1 : {} } | P2 : { _1 : Double } | P3 : { _1 : {}, _2 : {} } | P4 : { _1 : Double, _2 : Double } >.P4 { _1 = 1.0, _2 = 2.0 }"

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

    wrappedOptions =
        Dhall.defaultInterpretOptions
            { Dhall.singletonConstructors = Dhall.Wrapped }

    functionWithOptions
      :: ( Generic a
         , Dhall.GenericToDhall (Rep a)
         , Generic b
         , Dhall.GenericFromDhall b (Rep b)
         )
      => Dhall.InterpretOptions -> Dhall.Decoder (a -> b)
    functionWithOptions options =
      Dhall.function (Dhall.genericToDhallWith options) (Dhall.genericAutoWith options)

    code `shouldPassThroughWrapped` values = testCase "Pass through" $ do
        f <- Dhall.input (functionWithOptions wrappedOptions) code

        values @=? map f values

    code `shouldPassThroughSmart` values = testCase "Pass through" $ do
        f <- Dhall.input (functionWithOptions smartOptions) code

        values @=? map f values

    code `shouldMarshalIntoWrapped` expectedValue = testCase "Marshal" $ do
        actualValue <- Dhall.input (Dhall.genericAutoWith wrappedOptions) code

        expectedValue @=? actualValue

    code `shouldMarshalIntoSmart` expectedValue = testCase "Marshal" $ do
        actualValue <- Dhall.input (Dhall.genericAutoWith smartOptions) code

        expectedValue @=? actualValue

    value `shouldEmbedAsWrapped` expectedCode = testCase "ToDhall" $ do
        parsedExpression <- Dhall.Core.throws (Dhall.Parser.exprFromText "(test)" expectedCode)

        resolvedExpression <- Dhall.Import.assertNoImports parsedExpression

        Dhall.Core.denote resolvedExpression @=? Dhall.embed (Dhall.genericToDhallWith wrappedOptions) value

    value `shouldEmbedAsSmart` expectedCode = testCase "ToDhall" $ do
        parsedExpression <- Dhall.Core.throws (Dhall.Parser.exprFromText "(test)" expectedCode)

        resolvedExpression <- Dhall.Import.assertNoImports parsedExpression

        Dhall.Core.denote resolvedExpression @=? Dhall.embed (Dhall.genericToDhallWith smartOptions) value

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

-- https://github.com/dhall-lang/dhall-haskell/issues/1711
data Issue1711
  = A Bad
  | B
  | C
  | D
  deriving (Generic, Show, FromDhall)

newtype Bad = Bad Text
  deriving (Show)

issue1711Msg :: Text
issue1711Msg = "Issue 1711"

instance FromDhall Bad where
  autoWith _ =
    Dhall.Decoder
      (const (Dhall.extractError issue1711Msg))
      (Dhall.expected Dhall.strictText)

type DhallExtractErrors = Dhall.ExtractErrors Dhall.Parser.Src Void

-- https://github.com/dhall-lang/dhall-haskell/issues/1711
shouldShowCorrectErrorForInvalidDecoderInUnion :: TestTree
shouldShowCorrectErrorForInvalidDecoderInUnion =
  testCase "Correct error is thrown for invalid decoder in union" $ do
    let value = "< B | D | C | A : Text >.A \"\""

    inputEx :: Either DhallExtractErrors Issue1711 <- try (Dhall.input Dhall.auto value)

    let expectedMsg = issue1711Msg

    let assertMsg = "The exception message did not match the expected output"

    case inputEx of
      Left (Dhall.DhallErrors errs) -> case errs of
        (err :| []) -> case err of
          Dhall.TypeMismatch {} -> fail "The extraction using an invalid decoder failed with a type mismatch"
          Dhall.ExpectedTypeError _ -> fail "An error occurred while determining the expected Dhall type"
          Dhall.ExtractError extractError -> assertEqual assertMsg expectedMsg extractError
        _ -> fail "The extraction using an invalid decoder failed with multiple errors"
      Right _ -> fail "The extraction using an invalid decoder succeeded"
