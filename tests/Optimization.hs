{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Optimization (optimizationTests) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Numeric.Natural
import Dhall.Core (Expr)
import Dhall.TypeCheck (X)

import qualified Control.Exception
import qualified Data.Text
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Dhall.Optimizer

--import Dhall.Core
import Test.Tasty
import Test.Tasty.HUnit

optimizationTests :: TestTree
optimizationTests =
    testGroup "optimization"
        [ shouldNormalize "NaturalMonus positive value" "NaturalMonus" (9999, 8888, 1111),
          shouldNormalize "NaturalMonus equal value" "NaturalMonus" (55, 55, 0),
          shouldNormalize "NaturalMonus zero value" "NaturalMonus" (0, 0, 0),
          shouldNormalize "NaturalMonus negative value" "NaturalMonus" (6666, 7777, 0)
        ]

should :: Text -> Text -> (Natural, Natural, Natural) -> TestTree
should name basename (x, y, r) =
    Test.Tasty.HUnit.testCase (Data.Text.unpack name) $ do
        let functionCode   = "./tests/optimization/" <> basename <> ".dhall"

        functionExpr <- case Dhall.Parser.exprFromText mempty functionCode of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr
        functionResolved <- Dhall.Import.load functionExpr
        case Dhall.TypeCheck.typeOf functionResolved of
            Left  err -> Control.Exception.throwIO err
            Right _   -> return ()
        let fullExpression = Dhall.Core.App (Dhall.Core.App functionResolved (Dhall.Core.NaturalLit x)) (Dhall.Core.NaturalLit y)
        let actualNormalized = Dhall.Core.normalizeOpt (Just Dhall.Optimizer.optimizer) fullExpression :: Expr X X
        let expectedNormalized = Dhall.Core.NaturalLit r

        let message =
                "The normalized expression did not match the expected output"
        Test.Tasty.HUnit.assertEqual message expectedNormalized actualNormalized

shouldNormalize :: Text -> Text -> (Natural, Natural, Natural) -> TestTree
shouldNormalize name = should ("optimize " <> name <> " correctly")
