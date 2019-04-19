{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Format where

import Data.Monoid (mempty, (<>))
import Data.Text (Text)
import Dhall.Pretty (CharacterSet(..))
import Test.Tasty (TestTree)

import qualified Control.Monad                         as Monad
import qualified Data.Text                             as Text
import qualified Data.Text.IO                          as Text.IO
import qualified Data.Text.Prettyprint.Doc             as Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text as Doc.Render.Text
import qualified Dhall.Core                            as Core
import qualified Dhall.Parser                          as Parser
import qualified Dhall.Pretty                          as Pretty
import qualified Dhall.Test.Util                       as Test.Util
import qualified Test.Tasty                            as Tasty
import qualified Test.Tasty.HUnit                      as Tasty.HUnit
import qualified Turtle

getTests :: IO TestTree
getTests = do
    let unicodeFiles = do
            path <- Turtle.lstree "./tests/format"

            let skip = [ "./tests/format/asciiA.dhall" ]

            Monad.guard (path `notElem` skip)

            return path

    unicodeTests <- Test.Util.discover (Turtle.chars <* "A.dhall") (formatTest Unicode) unicodeFiles

    asciiTests <- Test.Util.discover (Turtle.chars <* "A.dhall") (formatTest ASCII) (pure "./tests/format/asciiA.dhall")

    let testTree =
            Tasty.testGroup "format tests"
                [ unicodeTests
                , asciiTests
                ]

    return testTree

formatTest :: CharacterSet -> Text -> TestTree
formatTest characterSet prefix =
    Tasty.HUnit.testCase (Text.unpack prefix) $ do
        let inputFile  = Text.unpack (prefix <> "A.dhall")
        let outputFile = Text.unpack (prefix <> "B.dhall")

        inputText <- Text.IO.readFile inputFile

        expr <- Core.throws (Parser.exprFromText mempty inputText)

        let doc        = Pretty.prettyCharacterSet characterSet  expr
        let docStream  = Doc.layoutSmart Pretty.layoutOpts doc
        let actualText = Doc.Render.Text.renderStrict docStream

        expectedText <- Text.IO.readFile outputFile

        let message =
                "The formatted expression did not match the expected output"

        Tasty.HUnit.assertEqual message expectedText actualText
