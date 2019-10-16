{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Dhall.Test.Format where

import Data.Monoid (mempty, (<>))
import Data.Text (Text)
import Dhall.Pretty (CharacterSet(..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck ((===))
import Dhall.Test.QuickCheck () -- For Arbitrary (Expr s a)

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
import qualified Test.Tasty.QuickCheck                 as Tasty.QuickCheck
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
                , idempotentTests Unicode
                , idempotentTests ASCII
                ]

    return testTree

format :: CharacterSet -> Text -> Core.Expr Parser.Src Core.Import -> Text
format characterSet header expr =
    let doc =  Doc.pretty header
            <> Pretty.prettyCharacterSet characterSet expr
            <> "\n"

        docStream = Doc.layoutSmart Pretty.layoutOpts doc
    in
        Doc.Render.Text.renderStrict docStream

formatTest :: CharacterSet -> Text -> TestTree
formatTest characterSet prefix =
    Tasty.HUnit.testCase (Text.unpack prefix) $ do
        let inputFile  = Text.unpack (prefix <> "A.dhall")
        let outputFile = Text.unpack (prefix <> "B.dhall")

        inputText <- Text.IO.readFile inputFile

        (header, expr) <- Core.throws (Parser.exprAndHeaderFromText mempty inputText)

        let actualText = format characterSet header expr
        expectedText <- Text.IO.readFile outputFile

        let message =
                   "The formatted expression did not match the expected output\n"
                <> "Expected:\n\n" <> Text.unpack expectedText <> "\n\n"
                <> "Actual:\n\n" <> Text.unpack actualText <> "\n\n"
                <> "Expected (show): " <> show expectedText <> "\n"
                <> "Actual   (show): " <> show actualText <> "\n"

        Tasty.HUnit.assertBool message (actualText == expectedText)

idempotentTests :: CharacterSet -> TestTree
idempotentTests characterSet =
    Tasty.QuickCheck.testProperty
        ("Formatting should be idempotent with " <> showCharacterSet characterSet)
        $ \(formatted -> once) ->
            case Parser.exprAndHeaderFromText mempty once of
                Right (formatted -> twice) -> once === twice
                Left _ -> Tasty.QuickCheck.property Tasty.QuickCheck.Discard
    where
    showCharacterSet Unicode = "\"Unicode\""
    showCharacterSet ASCII   = "\"ASCII\""

    formatted = uncurry (format characterSet)
