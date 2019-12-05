{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Data.Monoid ((<>))
import Dhall.JSON.Yaml (Options(..), UnionTagOptions(..), UnionTagMode(..))
import Test.Tasty (TestTree)

import qualified Data.ByteString
import qualified Data.Text.IO
import qualified Dhall.JSON.Yaml
import qualified Dhall.Yaml
import qualified GHC.IO.Encoding
import qualified Test.Tasty
import qualified Test.Tasty.ExpectedFailure
import qualified Test.Tasty.HUnit

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    Test.Tasty.defaultMain testTree

testTree :: TestTree
testTree =
    Test.Tasty.testGroup "dhall-yaml"
        [ testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions
            "./tasty/data/normal"
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions
            "./tasty/data/special"
        , Test.Tasty.ExpectedFailure.ignoreTestBecause "#1516" $
          testDhallToYaml
            (Dhall.JSON.Yaml.defaultOptions { quoted = True })
            "./tasty/data/quoted"
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions
            "./tasty/data/special"
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions { unionTagOptions = Just inlineTagOptions }
            "./tasty/data/autoNestingInlineA"
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions { unionTagOptions = Just inlineTagOptions }
            "./tasty/data/autoNestingInlineB"
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions { unionTagOptions = Just nestedTagOptions }
            "./tasty/data/autoNestingNestedA"
        , testDhallToYaml
            Dhall.JSON.Yaml.defaultOptions { unionTagOptions = Just nestedTagOptions }
            "./tasty/data/autoNestingNestedB"
        ]

inlineTagOptions :: UnionTagOptions
inlineTagOptions = UnionTagOptions
    { tagMode = TagInline
    , tagField = "tag"
    }

nestedTagOptions :: UnionTagOptions
nestedTagOptions = UnionTagOptions
    { tagMode = TagNested "contents"
    , tagField = "tag"
    }

testDhallToYaml :: Options -> String -> TestTree
testDhallToYaml options prefix =
    Test.Tasty.testGroup prefix
        [ testCase Dhall.Yaml.dhallToYaml "HsYAML"
        , testCase Dhall.JSON.Yaml.dhallToYaml "aeson-yaml"
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
