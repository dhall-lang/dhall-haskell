{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dhall.Test.Regression.Primitives (tests) where

import Data.Void        (Void)
import Numeric.Natural  (Natural)
import Test.Tasty       (TestTree)
import Test.Tasty.HUnit ((@?=))

import qualified Dhall
import qualified Dhall.Binary
import qualified Dhall.Core
import qualified Dhall.Test.Util      as Util
import qualified Test.Tasty
import qualified Test.Tasty.HUnit

-- These tests cover built-ins implemented as ordinary variable names.  A name
-- behaves like a built-in only when lookup reaches the empty context/environment.
tests :: TestTree
tests =
    Test.Tasty.testGroup "Primitives"
        [ Test.Tasty.HUnit.testCase "extracts fields from date literals" $ do
            year :: Natural <- Dhall.input Dhall.auto "Date/year 2024-08-13"
            month :: Natural <- Dhall.input Dhall.auto "Date/month 2024-08-13"
            day :: Natural <- Dhall.input Dhall.auto "Date/day 2024-08-13"

            year @?= 2024
            month @?= 8
            day @?= 13

        , Test.Tasty.HUnit.testCase "extracts fields from time literals" $ do
            hour :: Natural <- Dhall.input Dhall.auto "Time/hour 09:08:07.123"
            minute :: Natural <- Dhall.input Dhall.auto "Time/minute 09:08:07.123"
            second :: Natural <- Dhall.input Dhall.auto "Time/second 09:08:07.123"

            hour @?= 9
            minute @?= 8
            second @?= 7

        , Test.Tasty.HUnit.testCase "normalizes field extraction" $ do
            dateExpression <- Util.code "Date/day 2024-08-13"
            timeExpression <- Util.code "Time/second 09:08:07.987"

            Util.normalize' dateExpression @?= "13"
            Util.normalize' timeExpression @?= "7"

        , Test.Tasty.HUnit.testCase "allows let-bound names to override primitives" $ do
            dateResult :: Natural <-
                Dhall.input Dhall.auto "let Date/year : Natural = 42 in Date/year"
            timeResult :: Natural <-
                Dhall.input Dhall.auto "let Time/hour : Natural = 42 in Time/hour"

            dateResult @?= 42
            timeResult @?= 42

        , Test.Tasty.HUnit.testCase "allows lambda-bound names to override primitives" $ do
            dateResult :: Natural <-
                Dhall.input Dhall.auto "(\\(Date/day : Natural) -> Date/day) 99"
            timeResult :: Natural <-
                Dhall.input Dhall.auto "(\\(Time/second : Natural) -> Time/second) 99"

            dateResult @?= 99
            timeResult @?= 99

        , Test.Tasty.HUnit.testCase "allows explicit outer index to reach shadowed primitive" $ do
            dateResult :: Natural <-
                Dhall.input Dhall.auto "let Date/year : Natural = 42 in Date/year@1 2024-08-13"
            timeResult :: Natural <-
                Dhall.input Dhall.auto "let Time/hour : Natural = 42 in Time/hour@1 09:08:07"

            dateResult @?= 2024
            timeResult @?= 9

        , Test.Tasty.HUnit.testCase "rejects out-of-range builtin indices" $ do
            Util.assertDoesntTypeCheck "Date/year@1 2024-08-13"
            Util.assertDoesntTypeCheck "Time/hour@1 09:08:07"

        , Test.Tasty.HUnit.testCase "encodes as ordinary variables" $ do
            let dateExpression =
                    Dhall.Core.Var (Dhall.Core.V "Date/year" 0) :: Dhall.Core.Expr Void Void
            let timeExpression =
                    Dhall.Core.Var (Dhall.Core.V "Time/hour" 0) :: Dhall.Core.Expr Void Void

            Dhall.Binary.decodeExpression (Dhall.Binary.encodeExpression dateExpression)
                @?= Right dateExpression
            Dhall.Binary.decodeExpression (Dhall.Binary.encodeExpression timeExpression)
                @?= Right timeExpression
        ]
