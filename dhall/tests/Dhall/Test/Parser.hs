{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Parser where

import Data.Text       (Text)
import Data.Void       (Void)
import Dhall.Core      (Binding (..), Expr (..), Import, Var (..))
import System.FilePath ((</>))
import Test.Tasty      (TestTree)

import qualified Control.Monad        as Monad
import qualified Data.Bifunctor       as Bifunctor
import qualified Data.ByteString      as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.List            as List
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text.Encoding
import qualified Data.Text.IO         as Text.IO
import qualified Dhall.Binary         as Binary
import qualified Dhall.Core           as Core
import qualified Dhall.Parser         as Parser
import qualified Dhall.Test.Util      as Test.Util
import qualified Test.Tasty           as Tasty
import qualified Test.Tasty.HUnit     as Tasty.HUnit
import qualified Text.Printf          as Printf
import qualified Turtle

parseDirectory :: FilePath
parseDirectory = "./dhall-lang/tests/parser"

binaryDecodeDirectory :: FilePath
binaryDecodeDirectory = "./dhall-lang/tests/binary-decode"

getTests :: IO TestTree
getTests = do
    let successFiles = Turtle.lstree (parseDirectory </> "success")

    successTests <-
        Test.Util.discover (Turtle.chars <* "A.dhall") shouldParse successFiles

    let failureFiles = Turtle.lstree (parseDirectory </> "failure")

    failureTests <-
        Test.Util.discover (Turtle.chars <> ".dhall") shouldNotParse failureFiles

    let binaryDecodeSuccessFiles =
            Turtle.lstree (binaryDecodeDirectory </> "success")

    binaryDecodeSuccessTests <-
        Test.Util.discover (Turtle.chars <* "A.dhallb") shouldDecode binaryDecodeSuccessFiles

    let binaryDecodeFailureFiles = Turtle.lstree (binaryDecodeDirectory </> "failure")

    binaryDecodeFailureTests <-
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
        [ notesInLetInLet
        , errorLocationTests
        ]

errorLocationTests :: TestTree
errorLocationTests =
    Tasty.testGroup "error locations and hints"
        [ failsAtInnerMistake
            "#1592 application of malformed lambda"
            "a (\\(x:Natural) -> x)"
            ["unexpected '('"]
        , failsAtInnerMistake
            "#1592 list of malformed lambda"
            "[ \\(x:Natural) -> x ]"
            ["unexpected '['"]
        , failsAtInnerMistake
            "#2009 toMap without space"
            "foo (toMap{bar=\"baz\"})"
            ["unexpected '('"]
        , failsAtInnerMistake
            "#2605 leading zero in nested record"
            "[ { x = { y = 13 } }, { x = { y = 07 } }]"
            ["unexpected '{'"]
        , messageContains
            "lambda missing space after ':'"
            "\\(x:Natural) -> x"
            "Whitespace is required after :"
        , messageContains
            "invalid escape sequence"
            "\"\\latex\""
            "Invalid escape sequence"
        , messageContains
            "missing comma in record"
            "{ x = 1 y = 2 }"
            "Missing ',' in record literal"
        , messageContains
            "#2265 extra comma in record"
            "{ a = \"a\", b = { a = \"foo\",, } }"
            "Unexpected extra ',' in record literal"
        , failsAtInnerMistake
            "#2265 extra comma does not blame the outer field"
            "{ a = \"a\", b = { a = \"foo\",, } }"
            ["unexpected 'b'"]
        , messageContains
            "record type missing space after ':'"
            "{ x:Natural }"
            "Whitespace is required after :"
        , messageContains
            "annotation missing space after ':'"
            "x:Natural"
            "Whitespace is required after :"
        , failsAtInnerMistake
            "leading zero in list"
            "[07]"
            ["unexpected '['"]
        ]

failsAtInnerMistake :: String -> Text -> [String] -> TestTree
failsAtInnerMistake name input bannedSubstrings =
    Tasty.HUnit.testCase name $ do
        case Parser.exprFromText name input of
            Right _ ->
                Tasty.HUnit.assertFailure "Unexpected successful parse"
            Left err -> do
                let msg = show err
                Monad.forM_ bannedSubstrings $ \banned ->
                    Tasty.HUnit.assertBool
                        ("Parse error should not blame the outer construct with "
                            <> show banned
                            <> ":\n"
                            <> msg)
                        (not (banned `List.isInfixOf` msg))

messageContains :: String -> Text -> String -> TestTree
messageContains name input expected =
    Tasty.HUnit.testCase name $ do
        case Parser.exprFromText name input of
            Right _ ->
                Tasty.HUnit.assertFailure "Unexpected successful parse"
            Left err -> do
                let msg = show err
                Tasty.HUnit.assertBool
                    ("Expected parse error to contain "
                        <> show expected
                        <> ":\n"
                        <> msg)
                    (expected `List.isInfixOf` msg)

notesInLetInLet :: TestTree
notesInLetInLet =
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
                      (Note "0 " (Note "0" (NaturalLit 0)))
                    )
                    -- This 'Let' isn't wrapped in a 'Note'!
                    (Let
                      (Binding
                        (Just " ")
                        "y"
                        (Just " ")
                        Nothing
                        (Just " ")
                        (Note "1 " (Note "1" (NaturalLit 1)))
                      )
                      (Note "let z = 2 in x"
                        (Let
                          (Binding
                            (Just " ")
                            "z"
                            (Just " ")
                            Nothing
                            (Just " ")
                            (Note "2 " (Note "2" (NaturalLit 2)))
                          )
                          (Note "x" (Var (V "x" 0)))
                        )
                      )
                    )
                  )
                )

        let msg = "Unexpected parse result"

        Tasty.HUnit.assertEqual msg expected (simplifyNotes expression)

shouldParse :: Text -> TestTree
shouldParse path = do
    let expectedFailures = []

    let pathString = Text.unpack path

    Test.Util.testCase path expectedFailures $ do
        text <- Text.IO.readFile (pathString <> "A.dhall")

        encoded <- ByteString.Lazy.readFile (pathString <> "B.dhallb")

        expression <- case Parser.exprFromText mempty text of
            Left  exception  -> Tasty.HUnit.assertFailure (show exception)
            Right expression -> return expression

        let bytes = Binary.encodeExpression (Core.denote expression)

        let render =
                  concatMap (Printf.printf "%02x ")
                . ByteString.Lazy.unpack

        Monad.unless (encoded == bytes) $ do
            let message = "The expected CBOR representation doesn't match the actual one\n"
                          ++ "expected: " ++ render encoded ++ "\n but got: " ++ render bytes
                          ++ "\n expr: " ++ show (Core.denote expression :: Expr Void Import)

            Tasty.HUnit.assertFailure message


shouldNotParse :: Text -> TestTree
shouldNotParse path = do
    let expectedFailures =
            [ parseDirectory </> "failure/spacing/LetNoSpace4.dhall"
            ]

    let pathString = Text.unpack path

    Test.Util.testCase path expectedFailures (do
        bytes <- ByteString.readFile pathString

        case Text.Encoding.decodeUtf8' bytes of
            Left _ -> return ()
            Right text ->
                case Parser.exprFromText mempty text of
                    Left  _ -> return ()
                    Right _ -> Tasty.HUnit.assertFailure "Unexpected successful parse" )

shouldDecode :: Text -> TestTree
shouldDecode pathText = do
    let expectedFailures =
          [ {- Note that this test actually successfully decodes the value, but
               mistakenly decodes the value to `_` instead of `x`.  This is
               because the 55799 tag causes normal decoding to fail, so it falls
               back to treating the `"x"` as a version tag instead of a label.

               Either way, fixing 55799 decoding would cause this test to pass
               again.
            -}
            binaryDecodeDirectory </> "success/unit/SelfDescribeCBORX2"
          , binaryDecodeDirectory </> "success/unit/SelfDescribeCBORX3"
          ]

    let pathString = Text.unpack pathText

    Test.Util.testCase pathText expectedFailures (do
        bytes <- ByteString.Lazy.readFile (pathString <> "A.dhallb")

        decodedExpression <- case Binary.decodeExpression bytes of
            Left exception ->
                Tasty.HUnit.assertFailure (show exception)
            Right decodedExpression ->
                return decodedExpression

        text <- Text.IO.readFile (pathString <> "B.dhall")

        parsedExpression <- Core.throws (Parser.exprFromText mempty text)

        let strippedExpression :: Expr Void Import
            strippedExpression = Core.denote parsedExpression

        let message =
                "The decoded expression didn't match the parsed expression"

        Tasty.HUnit.assertEqual message strippedExpression decodedExpression )

shouldNotDecode :: Text -> TestTree
shouldNotDecode pathText = do
    let expectedFailures = []

    let pathString = Text.unpack pathText

    Test.Util.testCase pathText expectedFailures (do
        bytes <- ByteString.Lazy.readFile (pathString <> ".dhallb")

        case Binary.decodeExpression bytes :: Either Binary.DecodingFailure (Expr Void Import) of
            Left _ -> return ()
            Right _ -> Tasty.HUnit.assertFailure "Unexpected successful decode" )
