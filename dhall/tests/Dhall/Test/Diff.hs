{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Diff where

import Data.Text  (Text)
import Test.Tasty (TestTree)

import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text.IO
import qualified Dhall.Core                as Core
import qualified Dhall.Diff                as Diff
import qualified Dhall.Parser              as Parser
import qualified Dhall.Pretty
import qualified Dhall.Test.Util           as Test.Util
import qualified Prettyprinter.Render.Text as Pretty.Text
import qualified Test.Tasty                as Tasty
import qualified Test.Tasty.HUnit          as Tasty.HUnit
import qualified Turtle

diffDirectory :: FilePath
diffDirectory = "./tests/diff"

getTests :: IO TestTree
getTests = do
    diffTests <- Test.Util.discover (Turtle.chars <* "A.dhall") diffTest (Turtle.lstree diffDirectory)

    let testTree = Tasty.testGroup "diff tests" [ diffTests ]

    return testTree

diffTest :: Text -> TestTree
diffTest prefix =
    Tasty.HUnit.testCase (Text.unpack prefix) $ do
        let leftFile  = Text.unpack (prefix <> "A.dhall")
        let rightFile = Text.unpack (prefix <> "B.dhall")
        let diffFile  = Text.unpack (prefix <> ".txt")

        let toInput file = do
                text <- Text.IO.readFile file
                Core.throws (Parser.exprFromText mempty text)

        leftInput  <- toInput leftFile
        rightInput <- toInput rightFile

        expectedDiffText <- Text.IO.readFile diffFile

        let actualDiffDocument =
                Diff.doc (Diff.diffNormalized leftInput rightInput) <> "\n"

        let actualDiffText = Pretty.Text.renderStrict
                    (Dhall.Pretty.layout actualDiffDocument)

        let message = "The diffed expressions did not match the expected output"

        Tasty.HUnit.assertEqual message expectedDiffText actualDiffText
