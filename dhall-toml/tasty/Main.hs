module Main where

import Control.Monad       (unless)
import Data.Text           (unpack)
import Test.Tasty          (TestTree)
import Test.Tasty.HUnit    (HasCallStack, Assertion, assertFailure)
import Toml.Type.TOML      (TOML, tomlDiff)
import Toml.Type.Printer   (pretty)

import qualified Toml.Parser
import qualified Data.Text.IO
import qualified Dhall.Core      as Core
import qualified Dhall.Parser
import qualified Dhall.Import
import qualified Dhall.Toml
import qualified Dhall.TypeCheck
import qualified GHC.IO.Encoding
import qualified Test.Tasty
import qualified Test.Tasty.HUnit

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    Test.Tasty.defaultMain testTree


testTree :: TestTree
testTree =
    Test.Tasty.testGroup "dhall-toml" dhallToTomlTests
    where
        dhallToTomlTests = map testDhallToToml
            [ "./tasty/data/empty"
            , "./tasty/data/natural"
            , "./tasty/data/float"
            , "./tasty/data/multiple-fields"
            , "./tasty/data/nested-tables"
            , "./tasty/data/adjacent-tables"
            , "./tasty/data/inline-list"
            , "./tasty/data/record-list"
            ]

testDhallToToml :: String -> TestTree
testDhallToToml prefix = Test.Tasty.HUnit.testCase prefix $ do
    let inputFile = prefix ++ ".dhall"
    let outputFile = prefix ++ ".toml"
    text <- Data.Text.IO.readFile inputFile
    parsedExpression <-
        Core.throws (Dhall.Parser.exprFromText inputFile text)
    resolvedExpression <- Dhall.Import.load parsedExpression
    _ <- Core.throws (Dhall.TypeCheck.typeOf resolvedExpression)
    actualValue <-
        Core.throws (Dhall.Toml.dhallToToml resolvedExpression)
    inputText <- Data.Text.IO.readFile outputFile
    expectedValue <- case Toml.Parser.parse inputText of
        Left tomlErr -> fail $ show tomlErr
        Right expectedValue -> return expectedValue
    let message = "Conversion to TOML did not generate the expected output"
    assertTomlEq message expectedValue actualValue

assertTomlEq :: HasCallStack => String -> TOML -> TOML -> Assertion
assertTomlEq prefix expected actual  = unless (expected == actual) (assertFailure msg)
    where
        pretty' = unpack . pretty
        msg = prefix ++ "\nExpected:\n" ++ pretty' expected ++ "\nActual:\n" ++ pretty' actual ++
            "Diff:\nMissing:\n" ++ pretty' (tomlDiff expected actual) ++
            "\nExtra:\n" ++ pretty' (tomlDiff actual expected) ++
            "AST:\nExpected:\n" ++ show expected ++ "\nActual:\n" ++ show actual


