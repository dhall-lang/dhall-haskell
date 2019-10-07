{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Parser where

import Data.Monoid ((<>))
import Data.Text (Text)
import Dhall.Core (Binding(..), Expr(..), Import, Var(..))
import Dhall.TypeCheck (X)
import Prelude hiding (FilePath)
import Test.Tasty (TestTree)
import Turtle (FilePath, (</>))

import qualified Codec.CBOR.Read      as CBOR
import qualified Codec.CBOR.Term      as CBOR
import qualified Codec.Serialise      as Serialise
import qualified Control.Monad        as Monad
import qualified Data.Bifunctor       as Bifunctor
import qualified Data.ByteString      as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text.IO
import qualified Data.Text.Encoding   as Text.Encoding
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
    let successFiles = Turtle.lstree (parseDirectory </> "success")

    successTests <- do
        Test.Util.discover (Turtle.chars <* "A.dhall") shouldParse successFiles

    let failureFiles = Turtle.lstree (parseDirectory </> "failure")

    failureTests <- do
        Test.Util.discover (Turtle.chars <> ".dhall") shouldNotParse failureFiles

    let binaryDecodeSuccessFiles =
            Turtle.lstree (binaryDecodeDirectory </> "success")

    binaryDecodeSuccessTests <- do
        Test.Util.discover (Turtle.chars <* "A.dhallb") shouldDecode binaryDecodeSuccessFiles

    let binaryDecodeFailureFiles = Turtle.lstree (binaryDecodeDirectory </> "failure")

    binaryDecodeFailureTests <- do
        Test.Util.discover (Turtle.chars <* ".dhallb") shouldNotDecode binaryDecodeFailureFiles

    let testTree =
            Tasty.testGroup "parser tests"
                [ successTests
                , failureTests
                , internalTests
                , binaryDecodeSuccessTests
                , binaryDecodeFailureTests
                ]

    return testTree

internalTests :: TestTree
internalTests =
    Tasty.testGroup "internal"
        [ notesInLetInLet ]

notesInLetInLet :: TestTree
notesInLetInLet = do
    Tasty.HUnit.testCase "Notes in let-in-let" $ do
        let code = "let x = 0 let y = 1 in let z = 2 in x"

        expression <- Core.throws (Parser.exprFromText mempty code)

        let simplifyNotes = Bifunctor.first Parser.srcText

        let expected =
                (Note code
                  (Let
                    (Binding
                      (Just " ")
                      "x"
                      (Just " ")
                      Nothing
                      (Just " ")
                      (Note "0 " (NaturalLit 0)))
                    -- This 'Let' isn't wrapped in a 'Note'!
                    (Let
                      (Binding
                        (Just " ")
                        "y"
                        (Just " ")
                        Nothing
                        (Just " ")
                        (Note "1 " (NaturalLit 1))
                      )
                      (Note "let z = 2 in x"
                        (Let
                          (Binding
                            (Just " ")
                            "z"
                            (Just " ")
                            Nothing
                            (Just " ")
                            (Note "2 " (NaturalLit 2))
                          )
                          (Note "x"
                            (Var (V "x" 0))))))))

        let msg = "Unexpected parse result"

        Tasty.HUnit.assertEqual msg expected (simplifyNotes expression)

shouldParse :: Text -> TestTree
shouldParse path = do
    let skip =
            -- This is a bug created by a parsing performance
            -- improvement
            [ parseDirectory </> "success/unit/MergeParenAnnotation" ]

    let pathString = Text.unpack path

    Test.Util.testCase path skip $ do
        text <- Text.IO.readFile (pathString <> "A.dhall")

        encoded <- ByteString.Lazy.readFile (pathString <> "B.dhallb")

        expression <- Core.throws (Parser.exprFromText mempty text)

        let term = Binary.encodeExpression expression

        let bytes = Serialise.serialise term

        Monad.unless (encoded == bytes) $ do
            ("", expected) <- Core.throws (CBOR.deserialiseFromBytes CBOR.decodeTerm encoded)

            let message = "The expected CBOR representation doesn't match the actual one\n"
                          ++ "expected: " ++ show expected ++ "\n but got: " ++ show term
                          ++ "\n expr: " ++ show expression

            Tasty.HUnit.assertFailure message


shouldNotParse :: Text -> TestTree
shouldNotParse path = do
    let skip =
            [ -- These two unexpected successes are due to not correctly
              -- requiring non-empty whitespace after the `:` in a type
              -- annotation
              parseDirectory </> "failure/unit/ImportEnvWrongEscape.dhall"

              -- Other spacing related unexpected successes:
            , parseDirectory </> "failure/spacing/AnnotationNoSpace.dhall"
            , parseDirectory </> "failure/spacing/ApplicationNoSpace1.dhall"
            , parseDirectory </> "failure/spacing/ApplicationNoSpace2.dhall"
            , parseDirectory </> "failure/spacing/AssertNoSpace.dhall"
            , parseDirectory </> "failure/spacing/ForallNoSpace.dhall"
            , parseDirectory </> "failure/spacing/ImportAltNoSpace.dhall"
            , parseDirectory </> "failure/spacing/ImportHashedNoSpace.dhall"
            , parseDirectory </> "failure/spacing/LambdaNoSpace.dhall"
            , parseDirectory </> "failure/spacing/ListLitEmptyNoSpace.dhall"
            , parseDirectory </> "failure/spacing/MergeAnnotationNoSpace3.dhall"
            , parseDirectory </> "failure/spacing/MergeNoSpace2.dhall"
            , parseDirectory </> "failure/spacing/NaturalPlusNoSpace.dhall"
            , parseDirectory </> "failure/spacing/RecordTypeNoSpace.dhall"
            , parseDirectory </> "failure/spacing/ToMapAnnotNoSpace.dhall"
            , parseDirectory </> "failure/spacing/UnionTypeNoSpace.dhall"

            , parseDirectory </> "failure/ImportHeadersExteriorHash.dhall"

              -- For parsing performance reasons the implementation
              -- treats a missing type annotation on an empty list as
              -- as a type-checking failure instead of a parse failure,
              -- but this might be fixable.
            , parseDirectory </> "failure/unit/ListLitEmptyMissingAnnotation.dhall"
            , parseDirectory </> "failure/unit/ListLitEmptyAnnotation.dhall"

              -- The same performance improvements also broke the
              -- precedence of parsing empty list literals
            , parseDirectory </> "failure/unit/ListLitEmptyPrecedence.dhall"
            ]

    let pathString = Text.unpack path

    Test.Util.testCase path skip (do
        bytes <- ByteString.readFile pathString

        case Text.Encoding.decodeUtf8' bytes of
            Left _ -> return ()
            Right text -> do
                case Parser.exprFromText mempty text of
                    Left  _ -> return ()
                    Right _ -> Tasty.HUnit.assertFailure "Unexpected successful parse" )

shouldDecode :: Text -> TestTree
shouldDecode pathText = do
    let skip = []

    let pathString = Text.unpack pathText

    Test.Util.testCase pathText skip (do
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

shouldNotDecode :: Text -> TestTree
shouldNotDecode pathText = do
    let skip = []

    let pathString = Text.unpack pathText

    Test.Util.testCase pathText skip (do
        bytes <- ByteString.Lazy.readFile (pathString <> ".dhallb")

        term <- Core.throws (Serialise.deserialiseOrFail bytes)

        case Binary.decodeExpression term :: Either Binary.DecodingFailure (Expr X Import) of
            Left _ -> return ()
            Right _ -> Tasty.HUnit.assertFailure "Unexpected successful decode" )
