{-| Compile-time generated files, used to put all data-files in the executable
    to make it easy to distribute.
-}

{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Dhall.Docs.Embedded (getDataDir) where

import Data.ByteString

#if defined(EMBED)
import Data.FileEmbed (embedDir)
#else
import Paths_dhall_docs hiding (getDataDir)

import qualified Control.Foldl   as Foldl
import qualified Data.ByteString as ByteString
import qualified Turtle
#endif

getDataDir :: IO [(FilePath, ByteString)]
#if defined(EMBED)
getDataDir = return $(embedDir "src/Dhall/data")
#else
getDataDir = do
    dir <- Turtle.directory . Turtle.decodeString
            <$> getDataFileName "src/Dhall/data/index.css"

    flip Turtle.fold Foldl.list $ do
        file <- Turtle.lstree dir
        contents <- Turtle.liftIO $ ByteString.readFile $ Turtle.encodeString file
        return (Turtle.encodeString $ Turtle.filename file, contents)

#endif
