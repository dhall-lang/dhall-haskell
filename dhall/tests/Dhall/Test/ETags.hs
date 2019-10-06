{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.ETags where

import Data.Monoid ((<>))
import Data.Text (Text)
import Dhall.Util (Input(..))
import Prelude hiding (FilePath)
import Test.Tasty (TestTree)
import Turtle (FilePath)

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
    etagsTests <- Test.Util.discover (Turtle.chars <* "A.dhall") etagsTest (Turtle.lstree etagsDirectory)

    let testTree = Tasty.testGroup "etags tests" [ etagsTests ]

    return testTree

fixSlash :: Text -> Text
fixSlash = Text.map (\c -> if c == '\\' then '/' else c)

etagsTest :: Text -> TestTree
etagsTest prefix =
    Tasty.HUnit.testCase (Text.unpack prefix) $ do
        let inputFile  = Text.unpack (prefix <> "A.dhall")
        let outputFile = Text.unpack (prefix <> "B.tags")

        actualTags <- fixSlash <$> ETags.generate (File inputFile) [""] False

        expectedTags <- fixSlash <$> Text.IO.readFile outputFile

        let message = "The actual tags did not match the expected tags"

        Tasty.HUnit.assertEqual message expectedTags actualTags

