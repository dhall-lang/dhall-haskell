module Main where

import Test.Tasty (TestTree)

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
    Test.Tasty.HUnit.assertEqual message expectedValue actualValue

