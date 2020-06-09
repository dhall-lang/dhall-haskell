{-| Compile-time generated files, used to put all data-files in the executable
    to make it easy to distribute.
-}

{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Dhall.Docs.Embedded (getDataDir) where

import Data.ByteString (ByteString)

#if defined(EMBED)
import Data.FileEmbed (embedDir)
import Path           (File, Path, Rel)

import qualified Path
#else
import Paths_dhall_docs hiding (getDataDir)

import qualified Control.Foldl   as Foldl
import qualified Data.ByteString as ByteString
import qualified Turtle
#endif

getDataDir :: IO [(Path Rel File, ByteString)]
#if defined(EMBED)
getDataDir = return $(embedDir "src/Dhall/data") >>= mapM f
  where
    f :: (FilePath, ByteString) -> IO (Path Rel File, ByteString)
    f (filePath, contents) = (,contents) <$> Path.parseRelFile filePath
#else
getDataDir = do
    dir <- Turtle.directory . Turtle.decodeString
            <$> getDataFileName "src/Dhall/data/index.css"

    flip Turtle.fold Foldl.list $ do
        file <- Turtle.lstree dir
        contents <- Turtle.liftIO $ ByteString.readFile $ Turtle.encodeString file
        return (Turtle.encodeString $ Turtle.filename file, contents)

#endif
