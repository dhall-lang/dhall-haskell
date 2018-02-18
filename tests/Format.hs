{-# LANGUAGE OverloadedStrings #-}

module Format where

import Data.Monoid (mempty, (<>))
import Data.Text (Text)
import Test.Tasty (TestTree)

import qualified Control.Exception
import qualified Data.Text
import qualified Data.Text.Lazy.IO
import qualified Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text
import qualified Dhall.Parser
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
        ]

opts :: Data.Text.Prettyprint.Doc.LayoutOptions
opts =
    Data.Text.Prettyprint.Doc.defaultLayoutOptions
        { Data.Text.Prettyprint.Doc.layoutPageWidth =
            Data.Text.Prettyprint.Doc.AvailablePerLine 80 1.0
        }

should :: Text -> Text -> TestTree
should name basename =
    Test.Tasty.HUnit.testCase (Data.Text.unpack name) $ do
        let inputFile =
                Data.Text.unpack ("./tests/format/" <> basename <> "A.dhall")
        let outputFile =
                Data.Text.unpack ("./tests/format/" <> basename <> "B.dhall")
        inputText <- Data.Text.Lazy.IO.readFile inputFile

        expr <- case Dhall.Parser.exprFromText mempty inputText of
            Left  err  -> Control.Exception.throwIO err
            Right expr -> return expr

        let doc        = Data.Text.Prettyprint.Doc.pretty expr
        let docStream  = Data.Text.Prettyprint.Doc.layoutSmart opts doc
        let actualText = Data.Text.Prettyprint.Doc.Render.Text.renderLazy docStream

        expectedText <- Data.Text.Lazy.IO.readFile outputFile

        let message =
                "The formatted expression did not match the expected output"
        Test.Tasty.HUnit.assertEqual message expectedText actualText
