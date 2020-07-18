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
import Test.Tasty      (TestTree)

import qualified Control.Monad
import qualified Data.ByteString
import qualified Data.Map.Strict               as Map
import qualified Data.Text
import qualified Data.Text.IO                  as Text.IO
import qualified GHC.IO.Encoding
import qualified Path
import qualified Path.IO
import qualified Test.Tasty
import qualified Test.Tasty.Silver             as Silver
import qualified Test.Tasty.Silver.Interactive as Silver
import qualified Text.PrettyPrint              as Pretty
import qualified Text.XML.HaXml.Html.Parse     as HaXml
import qualified Text.XML.HaXml.Pretty         as HaXml

main :: IO ()
main = do
    GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8

    input <- getPackageContents
    let GeneratedDocs _ docs = generateDocsPure "test-package" Unicode input
    let docsMap = Map.fromList docs
    Silver.defaultMain $ testTree docsMap

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

testTree :: Map (Path Rel File) Text -> TestTree
testTree docsMap =
    Test.Tasty.testGroup "dhall-docs"
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
