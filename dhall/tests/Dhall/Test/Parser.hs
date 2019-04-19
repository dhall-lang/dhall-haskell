{-# LANGUAGE OverloadedStrings #-}

module Dhall.Test.Parser where

import Data.Text (Text)
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

getTests :: IO TestTree
getTests = do
    successTests <- do
        Test.Util.discover (Turtle.chars <* "A.dhall") shouldParse (Turtle.ls (parseDirectory </> "success"))

    let failureFiles = do
            path <- Turtle.ls (parseDirectory </> "failure")

            let skip =
                    [ parseDirectory </> "failure/annotation.dhall"
                    , parseDirectory </> "failure/missingSpace.dhall"
                    ]

            Monad.guard (path `notElem` skip)

            return path

    failureTests <- do
        Test.Util.discover (Turtle.chars <> ".dhall") shouldNotParse failureFiles

    let testTree =
            Tasty.testGroup "parser tests"
                [ successTests
                , failureTests
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
