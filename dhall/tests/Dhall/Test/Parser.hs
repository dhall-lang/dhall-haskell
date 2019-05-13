{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Parser where

import Data.Text (Text)
import Dhall.Core (Expr, Import)
import Dhall.TypeCheck (X)
import Prelude hiding (FilePath)
import Test.Tasty (TestTree)
import Turtle (FilePath, (</>))

import qualified Codec.Serialise      as Serialise
import qualified Control.Monad        as Monad
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text.IO
import qualified Dhall.Binary         as Binary
import qualified Dhall.Core           as Core
import qualified Dhall.Parser         as Parser
import qualified Dhall.Test.Util      as Test.Util
import qualified Test.Tasty           as Tasty
import qualified Test.Tasty.HUnit     as Tasty.HUnit
import qualified Turtle

parseDirectory :: FilePath
parseDirectory = "./dhall-lang/tests/parser"

binaryDecodeDirectory :: FilePath
binaryDecodeDirectory = "./dhall-lang/tests/binary-decode"

getTests :: IO TestTree
getTests = do
    let successFiles = do
            path <- Turtle.lstree (parseDirectory </> "success")

            let skip =
                    -- This is a bug created by a parsing performance
                    -- improvement
                    [ parseDirectory </> "success/unit/MergeParenAnnotationA.dhall"
                    ]

            Monad.guard (path `notElem` skip)

            return path

    successTests <- do
        Test.Util.discover (Turtle.chars <* "A.dhall") shouldParse successFiles

    let failureFiles = do
            path <- Turtle.lstree (parseDirectory </> "failure")

            let skip =
                    [ -- These two unexpected successes are due to not correctly
                      -- requiring non-empty whitespace after the `:` in a type
                      -- annotatoin
                      parseDirectory </> "failure/annotation.dhall"
                    , parseDirectory </> "failure/unit/ImportEnvWrongEscape.dhall"

                      -- Similarly, the implementation does not correctly
                      -- require a space between a function and its argument
                    , parseDirectory </> "failure/missingSpace.dhall"

                      -- For parsing performance reasons the implementation
                      -- treats a missing type annotation on an empty list as
                      -- as a type-checking failure instead of a parse failure,
                      -- but this might be fixable.
                    , parseDirectory </> "failure/unit/ListLitEmptyAnnotation.dhall"
                      -- The same performance improvements also broke the
                      -- precedence of parsing empty list literals
                    , parseDirectory </> "failure/unit/ListLitEmptyPrecedence.dhall"
                    ]

            Monad.guard (path `notElem` skip)

            return path

    failureTests <- do
        Test.Util.discover (Turtle.chars <> ".dhall") shouldNotParse failureFiles

    let binaryDecodeFiles =
            Turtle.lstree (binaryDecodeDirectory </> "success")

    binaryDecodeTests <- do
        Test.Util.discover (Turtle.chars <* "A.dhallb") shouldDecode binaryDecodeFiles

    let testTree =
            Tasty.testGroup "parser tests"
                [ successTests
                , failureTests
                , binaryDecodeTests
                ]

    return testTree

shouldParse :: Text -> TestTree
shouldParse path = do
    let pathString = Text.unpack path

    Tasty.HUnit.testCase pathString $ do
        text <- Text.IO.readFile (pathString <> "A.dhall")

        encoded <- ByteString.Lazy.readFile (pathString <> "B.dhallb")

        expression <- Core.throws (Parser.exprFromText mempty text)

        let term = Binary.encode expression

        let bytes = Serialise.serialise term

        let message = "The expected CBOR representation doesn't match the actual one"
        Tasty.HUnit.assertEqual message encoded bytes

shouldNotParse :: Text -> TestTree
shouldNotParse path = do
    let pathString = Text.unpack path

    Tasty.HUnit.testCase pathString (do
        text <- Text.IO.readFile pathString

        case Parser.exprFromText mempty text of
            Left  _ -> return ()
            Right _ -> fail "Unexpected successful parser" )

shouldDecode :: Text -> TestTree
shouldDecode pathText = do
    let pathString = Text.unpack pathText

    Tasty.HUnit.testCase pathString (do
        bytes <- ByteString.Lazy.readFile (pathString <> "A.dhallb")

        term <- Core.throws (Serialise.deserialiseOrFail bytes)

        decodedExpression <- Core.throws (Binary.decodeExpression term)

        text <- Text.IO.readFile (pathString <> "B.dhall")

        parsedExpression <- Core.throws (Parser.exprFromText mempty text)

        let strippedExpression :: Expr X Import
            strippedExpression = Core.denote parsedExpression

        let message =
                "The decoded expression didn't match the parsed expression"

        Tasty.HUnit.assertEqual message decodedExpression strippedExpression )
