{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.ETags where

import Data.Monoid ((<>))
import Data.Text (Text)
import Dhall.Util (Input(..))
import Prelude hiding (FilePath)
import Test.Tasty (TestTree)
import Turtle (FilePath, format, fp)

import qualified Data.Text        as Text
import qualified Data.Text.IO     as Text.IO
import qualified Dhall.ETags      as ETags
import qualified Dhall.Test.Util  as Test.Util
import qualified Test.Tasty       as Tasty
import qualified Test.Tasty.HUnit as Tasty.HUnit
import qualified Turtle

etagsDirectory :: FilePath
etagsDirectory = "./tests/etags"

getTests :: IO TestTree
getTests = do
    etagsTests <- Test.Util.discover (Turtle.chars <* ".dhall") etagsTest (Turtle.lstree etagsDirectory)

    let testTree = Tasty.testGroup "etags tests" [ etagsTests, etagsDirTest ]

    return testTree

etagsTest :: Text -> TestTree
etagsTest prefix =
    Tasty.HUnit.testCase (Text.unpack prefix) $ do
        let inputFile  = Text.unpack (prefix <> ".dhall")
        let outputFile = Text.unpack (prefix <> ".tags")

        actualTags <- fixPathSeparators <$> ETags.generate (InputFile inputFile) Nothing False

        expectedTags <- readExpected outputFile

        let message = "The actual tags did not match the expected tags"

        Tasty.HUnit.assertEqual message expectedTags actualTags

etagsDirTest :: TestTree
etagsDirTest =
    Tasty.HUnit.testCase "all" $ do
        let outputFile = Text.unpack . format fp $ etagsDirectory Turtle.</> "all.tags"

        actualTags <- fmap fixPathSeparators
                      (ETags.generate
                          (InputFile (Text.unpack . format fp $ etagsDirectory))
                          (Just [".dhall"])
                          False)

        expectedTags <- readExpected outputFile

        let message = "The actual tags did not match the expected tags for directory test"

        Tasty.HUnit.assertEqual message expectedTags actualTags

readExpected :: String -> IO Text
readExpected file = fixPathSeparators <$> Text.IO.readFile file

fixPathSeparators :: Text -> Text
fixPathSeparators = Text.replace "\\" "/"
