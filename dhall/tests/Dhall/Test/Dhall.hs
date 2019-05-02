{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dhall.Test.Dhall where

import Control.Exception (SomeException, try)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Map

tests :: TestTree
tests =
    testGroup "Input"
     [ shouldShowDetailedTypeError
     , shouldHandleBothUnionLiterals
     , shouldHaveWorkingGenericAuto
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

license :: Dhall.Type ()
license = Dhall.union (Dhall.constructor "AllRightsReserved" Dhall.unit)

-- https://github.com/dhall-lang/dhall-haskell/issues/915
shouldHandleBothUnionLiterals :: TestTree
shouldHandleBothUnionLiterals = testCase "Marshal union literals" $ do
    _ <- Dhall.input license "< AllRightsReserved : {} >.AllRightsReserved {=}"
    _ <- Dhall.input license "< AllRightsReserved = {=} >"
    return ()

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
      compiler <- Dhall.input Dhall.genericAuto "< GHC3 : {} | GHCJS3 : {} | Helium3 : {} >.GHC3 {=}"
      assertEqual "genericAuto didn't give us what we wanted" GHC3 compiler

    , testCase "works for a two-constructor enum" $ do
      compiler <- Dhall.input Dhall.genericAuto "< GHC2 : {} | GHCJS2 : {} >.GHC2 {=}"
      assertEqual "genericAuto didn't give us what we wanted" GHC2 compiler
  ]
