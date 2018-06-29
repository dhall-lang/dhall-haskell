{-# LANGUAGE OverloadedStrings #-}

module Format where

import Data.Monoid (mempty, (<>))
import Data.Text (Text)
import Test.Tasty (TestTree)

import qualified Control.Exception
import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Test.Tasty
import qualified Test.Tasty.HUnit

formatTests :: TestTree
formatTests =
    Test.Tasty.testGroup "format tests"
        [ should
            "prefer multi-line strings when newlines present"
            "multiline"
        , should
            "escape ${ for single-quoted strings"
            "escapeSingleQuotedOpenInterpolation"
        , should
            "preserve the original order of fields"
            "fieldOrder"
        , should
            "escape numeric labels correctly"
            "escapeNumericLabel"
        , should
            "correctly handle scientific notation with a large exponent"
            "largeExponent"
        , should
            "correctly format the empty record literal"
            "emptyRecord"
        , should
            "indent then/else to the same column"
            "ifThenElse"
        , should
            "handle indenting long imports correctly without trailing space per line"
            "importLines"
        , should
            "handle indenting small imports correctly without trailing space inline"
            "importLines2"
        , should
            "not remove parentheses when accessing a field of a record"
            "importAccess"
        , should
            "handle formatting sha256 imports correctly"
            "sha256Printing"
        , should
            "handle formatting of Import suffix correctly"
            "importSuffix"
        ]

should :: Text -> Text -> TestTree
should name basename =
    Test.Tasty.HUnit.testCase (Data.Text.unpack name) $ do
        let inputFile =
                Data.Text.unpack ("./tests/format/" <> basename <> "A.dhall")
        let outputFile =
                Data.Text.unpack ("./tests/format/" <> basename <> "B.dhall")
        inputText <- Data.Text.IO.readFile inputFile

        expr <- case Dhall.Parser.exprFromText mempty inputText of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr

        let doc        = Data.Text.Prettyprint.Doc.pretty expr
        let docStream  = Data.Text.Prettyprint.Doc.layoutSmart Dhall.Pretty.layoutOpts doc
        let actualText = Data.Text.Prettyprint.Doc.Render.Text.renderStrict docStream

        expectedText <- Data.Text.IO.readFile outputFile

        let message =
                "The formatted expression did not match the expected output"
        Test.Tasty.HUnit.assertEqual message expectedText actualText
