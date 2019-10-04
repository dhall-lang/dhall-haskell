{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the implementation of the @dhall tags@ command

module Dhall.ETags
    ( -- * ETags
      generate
    ) where

import Dhall.Util (Input(..))
import Data.Text (Text)
import Control.Monad
import Control.Exception
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Data.List (isSuffixOf)
import System.Directory (doesDirectoryExist, getDirectoryContents,
#if MIN_VERSION_directory(1,3,0)
                         pathIsSymbolicLink)
#else
                         isSymbolicLink)
#endif
import System.FilePath ((</>), takeFileName)

data Pos = Pos Int Int deriving (Eq, Ord)
newtype Tags = Tags (M.Map FilePath (M.Map Pos Tag))
instance Semigroup Tags where
    (Tags ts1) <> (Tags ts2) = Tags (M.unionWith M.union ts1 ts2)
data Tag = Tag Text Text

{-| Generate ETags for Dhall expressions
-}
generate :: Input -> [Text] -> Bool -> IO Text
generate StandardInput _ _ = showTags . tags "" <$> TIO.getContents
generate (File p) sxs followSyms = do
    isD <- doesDirectoryExist p
    files <- dirToFiles followSyms (map T.unpack (if isD then sxs else [""])) p
    showTags <$> foldM (\ts f -> do
                                   t <- tags f <$> handle (\(SomeException _) -> return "")
                                                          (TIO.readFile f)
                                   return (ts <> t)) (Tags M.empty) files

tags :: FilePath -> Text -> Tags
tags f t = Tags (M.singleton f
                 (M.insert (Pos 0 1) (Tag (T.pack f) ((T.pack . takeFileName) f)) (parse t)))

parse :: Text -> M.Map Pos Tag
parse t = M.empty

showTags :: Tags -> Text
showTags (Tags ts) = T.concat . M.elems . M.mapWithKey showFileTags $ ts

showFileTags :: FilePath -> M.Map Pos Tag -> T.Text
showFileTags f ts = "\x0c\n" <> T.pack f <> "," <> (showInt . T.length) cs <> "\n" <> cs
    where cs = T.intercalate "\n" . M.elems . M.mapWithKey showPosTag $ ts

showPosTag :: Pos -> Tag -> Text
showPosTag (Pos line shift) (Tag def term) = def <>"\x7f" <> term <> "\x01" <> showInt line <>
                                             "," <> showInt shift <> "\n"

showInt :: Int -> Text
showInt = T.pack . show

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
