{-| Compile-time generated files, used to put all data-files in the executable
    to make it easy to distribute.
-}

{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Dhall.Docs.Embedded (getDataDir) where

import Data.ByteString (ByteString)
import Path            (File, Path, Rel)

import qualified Path

#if defined(EMBED)

import Data.FileEmbed (embedDir)

#else

import Paths_dhall_docs hiding (getDataDir)

import qualified Control.Monad
import qualified Data.ByteString as ByteString
import qualified Path.IO

#endif

getDataDir :: IO [(Path Rel File, ByteString)]
#if defined(EMBED)
getDataDir = mapM f $(embedDir "src/Dhall/data")
  where
    f :: (FilePath, ByteString) -> IO (Path Rel File, ByteString)
    f (filePath, contents) = (,contents) <$> Path.parseRelFile filePath
#else
getDataDir = do
    dir <- Path.parent
        <$> (getDataFileName "src/Dhall/data/index.css" >>= Path.parseAbsFile)
    files <- snd <$> Path.IO.listDir dir
    Control.Monad.forM files $ \file -> do
        contents <- ByteString.readFile $ Path.fromAbsFile file
        return (Path.filename file, contents)

#endif
