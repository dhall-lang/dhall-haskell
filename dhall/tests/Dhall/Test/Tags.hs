{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Tags where

import Data.Text       (Text)
import Dhall.Util      (Input (..))
import System.FilePath ((</>))
import Test.Tasty      (TestTree)

import qualified Data.Text        as Text
import qualified Data.Text.IO     as Text.IO
import qualified Dhall.Tags       as Tags
import qualified Dhall.Test.Util  as Test.Util
import qualified Test.Tasty       as Tasty
import qualified Test.Tasty.HUnit as Tasty.HUnit
import qualified Turtle

tagsDirectory :: FilePath
tagsDirectory = "./tests/tags"

getTests :: IO TestTree
getTests = do
    tagsTests <- Test.Util.discover (Turtle.chars <* ".dhall") tagsTest (Turtle.lstree tagsDirectory)

    let testTree = Tasty.testGroup "tags tests" [ tagsTests, tagsDirTest ]

    return testTree

tagsTest :: Text -> TestTree
tagsTest prefix =
    Tasty.HUnit.testCase (Text.unpack prefix) $ do
        -- The use of toDhallPah is a hack to ensure we always get the same file
        -- paths, i.e. ones with a '.' prefixed and UNIX path separators.
        let inputFile  = Text.unpack (Test.Util.toDhallPath prefix <> ".dhall")
        let outputFile = Text.unpack (prefix <> ".tags")

        actualTags <- fixPathSeparators <$> Tags.generate (InputFile inputFile) Nothing False

        expectedTags <- Text.IO.readFile outputFile

        let message = "The actual tags did not match the expected tags"

        Tasty.HUnit.assertEqual message expectedTags actualTags

tagsDirTest :: TestTree
tagsDirTest =
    Tasty.HUnit.testCase "all" $ do
        let outputFile = Text.unpack . Turtle.format Turtle.fp $ tagsDirectory </> "all.tags"

        actualTags <- fmap fixPathSeparators
                      (Tags.generate
                          (InputFile (Text.unpack . Turtle.format Turtle.fp $ tagsDirectory))
                          (Just [".dhall"])
                          False)

        expectedTags <- Text.IO.readFile outputFile

        let message = "The actual tags did not match the expected tags for directory test"

        Tasty.HUnit.assertEqual message expectedTags actualTags

fixPathSeparators :: Text -> Text
fixPathSeparators = Text.replace "\\" "/"
