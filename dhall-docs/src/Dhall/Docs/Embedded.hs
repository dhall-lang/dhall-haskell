{-| Compile-time generated files, used to put all data-files in the executable
    to make it easy to distribute.
-}

{-# LANGUAGE TemplateHaskell #-}

module Dhall.Docs.Embedded (dataDir) where

import Data.ByteString
import Data.FileEmbed  (embedDir)

dataDir :: [(FilePath, ByteString)]
dataDir = $(embedDir "src/Dhall/data")
