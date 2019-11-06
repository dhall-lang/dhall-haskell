{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Data.Monoid ((<>))
import Test.Tasty (TestTree)

import qualified Data.ByteString
import qualified Data.Text.IO
import qualified Dhall.Yaml -- dhall-json
import qualified Dhall.YAML -- dhall-yaml
import qualified GHC.IO.Encoding
import qualified Test.Tasty
import qualified Test.Tasty.HUnit

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    Test.Tasty.defaultMain testTree

testTree :: TestTree
testTree =
    Test.Tasty.testGroup "dhall-json"
        [ testDhallToYaml
            Dhall.Yaml.defaultOptions
            "./tasty/data/normal"
        , testDhallToYaml
            Dhall.Yaml.defaultOptions
            "./tasty/data/special"
        , testDhallToYaml
            (Dhall.Yaml.defaultOptions { Dhall.Yaml.quoted = True })
            "./tasty/data/quoted"
        ]

testDhallToYaml :: Dhall.Yaml.Options -> String -> TestTree
testDhallToYaml options prefix =
    Test.Tasty.testGroup prefix
        [ testCase Dhall.YAML.dhallToYaml "HsYAML"
        , testCase Dhall.Yaml.dhallToYaml "aeson-yaml"
        ]
  where
    testCase dhallToYaml s = Test.Tasty.HUnit.testCase s $ do
        let inputFile = prefix <> ".dhall"
        let outputFile = prefix <> ".yaml"

        text <- Data.Text.IO.readFile inputFile

        actualValue <- dhallToYaml options (Just inputFile) text

        expectedValue <- Data.ByteString.readFile outputFile

        let message = "Conversion to YAML did not generate the expected output"

        Test.Tasty.HUnit.assertEqual message expectedValue actualValue
