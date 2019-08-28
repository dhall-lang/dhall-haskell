{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Diff where

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc)
import Dhall.Core (Expr, Import)
import Dhall.Pretty (Ann)
import Dhall.Src (Src)
import Prelude hiding (FilePath)
import Test.Tasty (TestTree)
import Turtle (FilePath, (</>))

import qualified Data.Text                             as Text
import qualified Data.Text.IO                          as Text.IO
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty.Text
import qualified Dhall.Core                            as Core
import qualified Dhall.Diff                            as Diff
import qualified Dhall.Parser                          as Parser
import qualified Dhall.Test.Util                       as Test.Util
import qualified Test.Tasty                            as Tasty
import qualified Test.Tasty.HUnit                      as Tasty.HUnit
import qualified Turtle

diffDirectory :: FilePath
diffDirectory = "./tests/diff"

getTests :: IO TestTree
getTests = do
    normalizedDiffTests <- Test.Util.discover (Turtle.chars <* "A.dhall") (diffTest Diff.diffNormalized) (Turtle.lstree (diffDirectory </> "normalized"))

    nonNormalizedDiffTests <- Test.Util.discover (Turtle.chars <* "A.dhall") (diffTest Diff.diff) (Turtle.lstree (diffDirectory </> "non-normalized"))

    let testTree = Tasty.testGroup "diff tests" [ normalizedDiffTests, nonNormalizedDiffTests ]

    return testTree

diffTest :: (Expr Src Import -> Expr Src Import -> Doc Ann) -> Text -> TestTree
diffTest diffFunction prefix =
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
                diffFunction leftInput rightInput <> "\n"

        let options =
                Pretty.LayoutOptions
                    { Pretty.layoutPageWidth = Pretty.Unbounded }

        let actualDiffText =
                Pretty.Text.renderStrict
                    (Pretty.layoutPretty options actualDiffDocument)

        let message = "The diffed expressions did not match the expected output"

        Tasty.HUnit.assertEqual message expectedDiffText actualDiffText
