{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the implementation of the @dhall tags@ command

module Dhall.ETags
    ( -- * ETags
      generate
    ) where

import Dhall.Util (Input(..), Output(..))
import Data.Text (Text)
import Control.Monad
import qualified Data.Text as T
import qualified Dhall.Core
import qualified Dhall.Optics
import Data.List (isSuffixOf)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents,
                         getModificationTime, canonicalizePath,
#if MIN_VERSION_directory(1,3,0)
                         pathIsSymbolicLink)
#else
                         isSymbolicLink)
#endif
import System.FilePath ((</>))
import System.IO (Handle, IOMode, hClose, openFile, stdout)

{-| Generate ETags for Dhall expressions
-}
generate :: Input -> [Text] -> Bool -> IO Text
generate StandardInput _ _ = return "qqq"
generate (File p) sxs followSyms = do
    isD <- doesDirectoryExist p
    files <- dirToFiles followSyms (map T.unpack (if isD then sxs else [""])) p
    return (T.intercalate "\n" (map T.pack files))

{-| from https://github.com/MarcWeber/hasktags/blob/master/src/Hasktags.hs
 -| suffixes: [".dhall"], use "" to match all files
-}
dirToFiles :: Bool -> [String] -> FilePath -> IO [ FilePath ]
dirToFiles _ _ "STDIN" = lines <$> getContents
dirToFiles followSyms suffixes p = do
  isD <- doesDirectoryExist p
#if MIN_VERSION_directory(1,3,0)
  isSymLink <- pathIsSymbolicLink p
#else
  isSymLink <- isSymbolicLink p
#endif
  if isD
    then if isSymLink && not followSyms
        then return []
        else do
          -- filter . .. and hidden files .*
          contents <- filter ((/=) '.' . head) `fmap` getDirectoryContents p
          concat `fmap` mapM (dirToFiles followSyms suffixes . (</>) p) contents
    else return [p | matchingSuffix ]
  where matchingSuffix = any (`isSuffixOf` p) suffixes
