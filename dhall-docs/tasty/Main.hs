{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Text       (Text)
import Dhall.Docs.Core
import Dhall.Pretty    (CharacterSet (..))
import Path            (Dir, File, Path, Rel, (</>))
import Prelude         hiding (FilePath)
import Test.Tasty      (TestTree)
import Turtle          (FilePath, Pattern, Shell)

import qualified Control.Foldl                 as Foldl
import qualified Control.Monad
import qualified Data.ByteString
import qualified Data.Map.Strict               as Map
import qualified Data.Text
import qualified Data.Text.IO                  as Text.IO
import qualified GHC.IO.Encoding
import qualified Path
import qualified Path.IO
import qualified Test.Tasty
import qualified Test.Tasty.HUnit              as Tasty.HUnit
import qualified Test.Tasty.Silver             as Silver
import qualified Test.Tasty.Silver.Interactive as Silver
import qualified Text.PrettyPrint              as Pretty
import qualified Text.XML.HaXml.Html.Parse     as HaXml
import qualified Text.XML.HaXml.Pretty         as HaXml
import qualified Turtle

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    input <- getPackageContents
    let GeneratedDocs _ docs = generateDocsPure "test-package" Unicode input
    let docsMap = Map.fromList docs
    commentTests <- getCommentTests
    let testTree = Test.Tasty.testGroup "dhall-docs"
                    [ docGenerationTests docsMap
                    , commentTests
                    ]
    Silver.defaultMain testTree

getDirContents :: Path Rel Dir -> IO [(Path Rel File, ByteString)]
getDirContents dataDir = do
    files <- snd <$> Path.IO.listDirRecurRel dataDir
    Control.Monad.forM files $ \file -> do
        contents <- Data.ByteString.readFile $ Path.fromRelFile $ dataDir </> file
        return (file, contents)

goldenDir :: Path Rel Dir
goldenDir = $(Path.mkRelDir "./tasty/data/golden")

getPackageContents :: IO [(Path Rel File, ByteString)]
getPackageContents = getDirContents $(Path.mkRelDir "./tasty/data/package")

docGenerationTests :: Map (Path Rel File) Text -> TestTree
docGenerationTests docsMap =
    Test.Tasty.testGroup "doc-generation"
        $ map makeTest $ Map.assocs docsMap
  where
    makeTest :: (Path Rel File, Text) -> TestTree
    makeTest (testFile, text) =
        Silver.goldenVsAction testName goldenFilePath action converter
      where
        goldenFilePath = Path.fromRelFile $ goldenDir </> testFile
        testName = Path.fromRelFile testFile
        action = return prettyHtmlDoc
        converter = Data.Text.pack

        prettyHtmlDoc = Pretty.render
            $ HaXml.document
            $ HaXml.htmlParse testName (Data.Text.unpack text)

getCommentTests :: IO TestTree
getCommentTests = do
    ignoreTests <- getIgnoreTests
    invalidCommentTests <- getInvalidCommentsTests
    validComentTests <- getValidCommentTests
    return $ Test.Tasty.testGroup "comments"
                [ ignoreTests
                , invalidCommentTests
                , validComentTests
                ]

getIgnoreTests :: IO TestTree
getIgnoreTests = do
    tests <- discover (Turtle.chars <* ".txt") makeText (Turtle.lstree testsPath)
    return $ Test.Tasty.testGroup "ignored comments" [ tests ]
  where
    testsPath = "./tasty/data/comments/empty/"
    makeText prefix = Tasty.HUnit.testCase (Data.Text.unpack prefix) $ do
        let inputFile  = Data.Text.unpack (prefix <> ".txt")

        inputText <- Text.IO.readFile inputFile

        case parseSingleDhallDocsComment inputFile inputText of
            Nothing -> return ()
            e -> fail ("Should not return a dhall-docs comment neither an error: " <> show e)


getInvalidCommentsTests :: IO TestTree
getInvalidCommentsTests  = do
    tests <- discover (Turtle.chars <* ".txt") makeTest (Turtle.lstree testsPath)
    return $ Test.Tasty.testGroup "invalid comments" [ tests ]
  where
    testsPath = "./tasty/data/comments/invalid/"
    makeTest prefix = Tasty.HUnit.testCase (Data.Text.unpack prefix) $ do
        let inputFile  = Data.Text.unpack (prefix <> ".txt")

        inputText <- Text.IO.readFile inputFile

        case parseSingleDhallDocsComment inputFile inputText of
            Just (Left _) -> return ()
            e -> fail ("Should have returned an error: " <> show e)


getValidCommentTests :: IO TestTree
getValidCommentTests = do
    tests <- discover (Turtle.chars <* "A.txt") makeTest (Turtle.lstree testPath)
    return $ Test.Tasty.testGroup "valid comments" [ tests ]
  where
    testPath = "./tasty/data/comments/valid/"
    makeTest prefix =
        Silver.goldenVsAction (drop 2 testName) goldenFilePath action converter
      where
        testName = Data.Text.unpack prefix
        goldenFilePath = testName <> "B.txt"
        action = do
            let inputFile = testName <> "A.txt"
            contents <- Text.IO.readFile inputFile
            case parseSingleDhallDocsComment inputFile contents of
                Just (Right t) -> return t
                e -> fail ("It shouldn't have returned an error or no-text: " <> show e)
        converter = unDhallDocsText


discover :: Pattern Text -> (Text -> TestTree) -> Shell FilePath -> IO TestTree
discover pattern buildTest paths = do
    let shell = do
            path_ <- paths

            let pathText = Turtle.format Turtle.fp path_

            prefix : _ <- return (Turtle.match pattern pathText)

            return (buildTest prefix)

    tests <- Turtle.fold shell Foldl.list

    return (Test.Tasty.testGroup "discover" tests)
