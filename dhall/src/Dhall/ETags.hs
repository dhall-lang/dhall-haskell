{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | This module contains the implementation of the @dhall tags@ command

module Dhall.ETags
    ( -- * ETags
      generate
    ) where

import Control.Exception (handle, SomeException(..))
#if MIN_VERSION_base(4,10,0)
import Data.Either (fromRight)
#endif
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(..))
import Dhall.Map (foldMapWithKey)
import Data.Text (Text)
import Dhall.Util (Input(..))
import Dhall.Core (Expr(..), Binding(..))
import Dhall.Src (Src(srcStart))
import Dhall.Parser (exprFromText)
import System.Directory ( doesDirectoryExist
#if MIN_VERSION_directory(1,3,0)
                        , pathIsSymbolicLink
#elif MIN_VERSION_directory(1,2,6)
                        , isSymbolicLink
#endif
                        , getDirectoryContents
                        )
import System.FilePath ((</>), takeFileName)
import Text.Megaparsec (sourceLine, sourceColumn, unPos)

import qualified Data.Map      as M
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO

#if !(MIN_VERSION_base(4,10,0))
fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b
#endif

{-| 
    Documentation for ETags format is not very informative and not very correct.
    You can find version of documentation here:
    https://en.wikipedia.org/wiki/Ctags#Etags_2
    and you can also check source code here:
    http://cvs.savannah.gnu.org/viewvc/vtags/vtags/vtags.el?view=markup
-}

data PosInFile = FP
    { fpLine :: Int
      -- ^ line number, starting from 1, whe to find tag
    , fpOffset :: Int
      -- ^ byte offset form start of file. Not sure if any editor use it
    } deriving (Eq, Ord)

newtype Tags = Tags (M.Map FilePath (M.Map PosInFile Tag))

instance Semigroup Tags where
    (Tags ts1) <> (Tags ts2) = Tags (M.unionWith (<>) ts1 ts2)

instance Monoid Tags where
    mempty = Tags M.empty

data Tag = Tag
    { tagDefinition :: Text
      -- ^ In vtags source code this field has name pattern and EMacs use it as regex pattern
      --   to locate line with tag. It's looking for ^<tag definition>.
      --   Looks like vi is not using it.
    , tagName :: Text
      -- ^ text, that editor compare with selected text. So it's really name of entity
    }

{-| Generate ETags for Dhall expressions
-}
generate
    :: Input
    -- ^ Path to directory or filename that should be indexed
    -> [Text]
    -- ^ List of suffixes for dhall files [".dhall"] by default
    -> Bool
    -- ^ Flag if generate should follow symlinks
    -> IO Text
    -- ^ Content for tags file
generate inp sxs followSyms = do
    isD <- case inp of
               StandardInput -> return False
               InputFile p -> doesDirectoryExist p
    files <- inputToFiles followSyms (map T.unpack (if isD then sxs else [""])) inp
    tags <- foldMap (\f -> handle (\(SomeException _) -> return mempty)
                                  (fileTags f <$> TIO.readFile f)) files
    return (showTags tags)

{-| Find tags in Text (second argument) and generates list of them
    To make tags for filenames working in both, emacs and vi, add two initial tags.
    First for `filename` for vi and second with `/filename` for emacs.
    Other tags are working for both.
-}
fileTags :: FilePath -> Text -> Tags
fileTags f t = Tags (M.singleton f
                    (initialMap <> getTagsFromText t))
    where initialViTag = (FP 1 0, Tag "" (T.pack . takeFileName $ f))
          initialEmacsTag = (FP 1 1, Tag "" ("/" <> (T.pack . takeFileName) f))
          initialMap = M.fromList [initialViTag, initialEmacsTag]

getTagsFromText :: Text -> M.Map PosInFile Tag
getTagsFromText t = fromRight M.empty $
                              fixPosAndDefinition t . getTagsFromExpr <$> exprFromText "" t

-- after getTagsFromExpr line and column in line are in PosInFile for each tag
-- and tag definition is empty.
-- Emacs use tag definition to check if tag is on line. It compares line from start
-- with tag definition and in case they are the same, relocate user.
-- fixPosAndDefinition change position to line and byte offset and add tag definition.
fixPosAndDefinition :: Text -> M.Map PosInFile Tag -> M.Map PosInFile Tag
fixPosAndDefinition t = M.foldMapWithKey (\(FP ln c) (Tag _ term) ->
             let (lsl, l) = fromMaybe (0, "") (ln `M.lookup` mls) in
             M.singleton (FP ln (lsl + c)) (Tag (T.take c l) term))
    where mls :: M.Map Int (Int, Text) 
          -- mls is map that for each line has length of file before this map and line content.
          mls = M.fromList . fst .
                foldl (\(ls, lsl) (n, l) ->
                           let lsl' = lsl + 1 + T.length l in
                           ((n, (lsl, l)):ls, lsl')) ([], 0) . zip [1..] $ T.lines t

getTagsFromExpr :: Expr Src a -> M.Map PosInFile Tag
getTagsFromExpr = go (FP 0 0) M.empty
    where go lpos mts = \case
              (Let b e) -> go lpos (mts <> parseBinding b) e
              (Annot e1 e2) -> go lpos (go lpos mts e1) e2
              (Record mr) -> mts <> tagsFromDhallMap lpos mr
              (RecordLit mr) -> mts <> tagsFromDhallMap lpos mr
              (Union mmr) -> mts <> tagsFromDhallMap lpos mmr
              (Note s e) -> go (srcToPosInFile s) mts e
              _ -> mts
          tagsFromDhallMap lpos = foldMapWithKey (\k _ -> M.singleton lpos (Tag "" k))

parseBinding :: Binding Src a -> M.Map PosInFile Tag
parseBinding b = M.singleton (maybe (FP 0 0) srcToPosInFile (bindingSrc0 b))
                             (Tag "" (variable b))

srcToPosInFile :: Src -> PosInFile
srcToPosInFile s = FP line column
    where ssp = srcStart s
          line = unPos . sourceLine $ ssp
          column = unPos . sourceColumn $ ssp

showTags :: Tags -> Text
showTags (Tags ts) = T.concat . map (uncurry showFileTags) . M.toList $ ts

showFileTags :: FilePath -> M.Map PosInFile Tag -> T.Text
showFileTags f ts = "\x0c\n" <> T.pack f <> "," <> (showInt . T.length) cs <> "\n" <> cs
    where cs = T.concat . map (uncurry showPosTag) . M.toList $ ts

showPosTag :: PosInFile -> Tag -> Text
showPosTag fp tag = def <>"\x7f" <> name <> "\x01" <> showInt line <>
                    "," <> showInt offset <> "\n"
    where line = fpLine fp
          offset = fpOffset fp
          def = tagDefinition tag
          name = tagName tag

showInt :: Int -> Text
showInt = T.pack . show

{-| Generate list of files for given Input
-}
inputToFiles
    :: Bool
    -- ^ In case true, function will get files from  symbolic links
    -> [String]
    -- ^ List of suffixes. In case it contains empty string (""), all files will be returned.
    --   This parameter works only in case input is path to directory.
    -> Input
    -- ^ Where to look fo files. Can be directory name (`.` for example),
    --   file name or StandardInput. In case of StandardInput will wait for file names from
    --   STDIN, This way someone can combine tools in bash to send, for example, output from
    --   find to input of dhall tags.
    -> IO [ FilePath ]
    --   List of files.
inputToFiles _ _ StandardInput = lines <$> getContents
inputToFiles followSyms suffixes (InputFile path) = go path
    where go p = do
                   isD <- doesDirectoryExist p
#if MIN_VERSION_directory(1,3,0)
                   isSymLink <- pathIsSymbolicLink p
#elif MIN_VERSION_directory(1,2,6)
                   isSymLink <- isSymbolicLink pa
#else
                   let isSymLink = False
#endif
                   if isD
                     then if isSymLink && not followSyms
                            then return []
                            else do
                                   -- filter . .. and hidden files .*
                                   contents <- filter ((/=) '.' . head) <$> getDirectoryContents p
                                   concat <$> mapM (go . (</>) p) contents
                     else return [p | matchingSuffix ]
               where matchingSuffix = any (`isSuffixOf` p) suffixes
