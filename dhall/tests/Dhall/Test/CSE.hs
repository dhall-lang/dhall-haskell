{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.CSE where

import Data.Text    (Text)
import Data.Void    (Void)
import Test.Tasty   (TestTree)

import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text.IO
import qualified Dhall.Core                as Core
import qualified Dhall.Parser              as Parser
import qualified Dhall.Pretty              as Pretty
import qualified Dhall.Test.Util           as Test.Util
import qualified Prettyprinter.Render.Text as Doc.Render.Text
import qualified Test.Tasty                as Tasty
import qualified Test.Tasty.HUnit          as Tasty.HUnit
import qualified Turtle

cseDirectory :: FilePath
cseDirectory = "./tests/cse"

getTests :: IO TestTree
getTests = do
    cseTests <- Test.Util.discover (Turtle.chars <* "A.dhall") cseTest (Turtle.lstree cseDirectory)

    let testTree = Tasty.testGroup "cse tests" [ cseTests ]

    return testTree

format :: Core.Expr Void Core.Import -> Text
format expr =
    let renotedExpr = Core.renote expr :: Core.Expr Parser.Src Core.Import
        doc         = Pretty.prettyCharacterSet Pretty.Unicode renotedExpr <> "\n"
        docStream   = Pretty.layout doc
    in
        Doc.Render.Text.renderStrict docStream

cseTest :: Text -> TestTree
cseTest prefix =
    Tasty.HUnit.testCase (Text.unpack prefix) $ do
        let inputFile  = Text.unpack (prefix <> "A.dhall")
        let outputFile = Text.unpack (prefix <> "B.dhall")

        inputText <- Text.IO.readFile inputFile

        (_, parsedInput) <- Core.throws (Parser.exprAndHeaderFromText mempty inputText)

        let denotedInput = Core.denote parsedInput

        let actualExpression = Core.cse denotedInput

        let actualText = format actualExpression

        expectedText <- Text.IO.readFile outputFile

        let message = "The CSE'd expression did not match the expected output"

        Tasty.HUnit.assertEqual message expectedText actualText
