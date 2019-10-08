{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | This module contains the implementation of the @dhall tags@ command

module Dhall.ETags
    ( -- * ETags
      generate
    ) where

import Control.Exception (handle, SomeException(..))
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(..))
import Dhall.Map (foldMapWithKey)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Dhall.Util (Input(..))
import Dhall.Core (Expr(..), Binding(..))
import Dhall.Src (Src(srcStart))
import Dhall.Parser (exprFromText)
import System.FilePath ((</>), takeFileName)
import Text.Megaparsec (sourceLine, sourceColumn, unPos)

import qualified Data.ByteString as BS (length)
import qualified Data.Map      as M
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO
import qualified System.Directory as SD

{-| 
    Documentation for ETags format is not very informative and not very correct.
    You can find some documentation here:
    https://en.wikipedia.org/wiki/Ctags#Etags_2
    and you can also check the source code here:
    http://cvs.savannah.gnu.org/viewvc/vtags/vtags/vtags.el?view=markup
-}

data PosInFile = FP
    { fpLine :: Int
      -- ^ line number, starting from 1, where to find the tag
    , fpOffset :: Int
      -- ^ byte offset from start of file. Not sure if any editor uses it
    } deriving (Eq, Ord)

newtype Tags = Tags (M.Map FilePath [(PosInFile, Tag)])

instance Semigroup Tags where
    (Tags ts1) <> (Tags ts2) = Tags (M.unionWith (<>) ts1 ts2)

instance Monoid Tags where
    mempty = Tags M.empty
    mappend = (<>)

{-| For example, for the line: @let foo = \"foo\"@ the tag is:
    > Tag "let " "foo"
-}
data Tag = Tag
    { tagPattern :: Text
      -- ^ In vtags source code this field is named \"pattern\" and EMacs used it as
      --   a regex pattern to locate line with tag. It's looking for ^<tag pattern>.
      --   Looks like vi is not using it.
    , tagName :: Text
      -- ^ text, that editor compare with selected text. So it's really name of entity
    }

{-| Generate ETags for Dhall expressions
-}
generate
    :: Input
    -- ^ Where to look for files. This can be a directory name (@.@ for example),
    --   a file name or StandardInput. If `StandardInput`, then this will wait for
    --   file names from @STDIN@.
    --   This way someone can combine tools in @bash@ to send, for example, output from
    --   @find@ to the input of @dhall tags@.
    -> Maybe [Text]
    -- ^ List of suffixes for dhall files ([".dhall"] by default)
    -> Bool
    -- ^ Flag if generate should follow symlinks
    -> IO Text
    -- ^ Content for tags file
generate inp sxs followSyms = do
    files <- inputToFiles followSyms (map T.unpack <$> sxs) inp
    tags <- traverse (\f -> handle (\(SomeException _) -> return mempty)
                                   (fileTags f <$> TIO.readFile f)) files
    return (showTags . mconcat $ tags)

{-| Find tags in Text (second argument) and generates a list of them
    To make tags for filenames that works in both emacs and vi, add two initial tags.
    First for @filename@ for vi and second with @/filename@ for emacs.
    Other tags are working for both.
-}
fileTags :: FilePath -> Text -> Tags
fileTags f t = Tags (M.singleton f
                    (initialMap <> getTagsFromText t))
    where initialViTag = (FP 1 0, Tag "" (T.pack . takeFileName $ f))
          initialEmacsTag = (FP 1 1, Tag "" ("/" <> (T.pack . takeFileName) f))
          initialMap = [initialViTag, initialEmacsTag]

getTagsFromText :: Text -> [(PosInFile, Tag)]
getTagsFromText t = case exprFromText "" t of
    Right expr -> fixPosAndDefinition t (getTagsFromExpr expr)
    _ -> mempty

{-| Used to update tag position and to build tag from term.
    After getTagsFromExpr line and column in line are in PosInFile for each tag.
    And tagPattern is not added.
    Emacs use tag pattern to check if tag is on line. It compares line from start
    with tag pattern and in case they are the same, relocate user.
    fixPosAndDefinition change position to line and byte offset and add tag pattern.
    For example, for Dhall string:
    >>> let dhallSource = "let foo = \"bar\"\nlet baz = \"qux\""
    Input for this function is:
    >>> foundTerms = [(FP 0 4, "foo"), (FP 1 4, "baz")]
    And:
    >>> fixPosAndDefinition dhallSource foundTerms
    [(FP 0 4, Tag "let foo " "foo"), (FP 1 20, Tag "let baz " "baz")]
    where 20 is byte offset from file start.
-}
fixPosAndDefinition :: Text -> [(PosInFile, Text)] -> [(PosInFile, Tag)]
fixPosAndDefinition t = foldMap (\(FP ln c, term) ->
             let (ln', offset, tPattern) = fromMaybe (fallbackInfoForText ln c)
                                                     (infoForText term ln)
             in [(FP ln' offset, Tag tPattern term)])
    where mls :: M.Map Int (Text, Int) 
          -- ^ mls is map that for each line has length of file before this map and line content.
          --   In example above, first line is 15 bytes long and '\n', mls contain:
          --   (1, (16, "let foo = "bar"")
          --   That allow us to get byte offset easier.
          mls = M.fromList . fst . foldl processLine ([], 0) . zip [1..] $ T.lines t
          processLine :: ([(Int, (Text, Int))], Int) -> (Int, Text) -> ([(Int, (Text, Int))], Int)
          processLine (numberedLinesWithSizes, bytesBeforeLine) (n, line) =
              ((n, (line, bytesBeforeLine)): numberedLinesWithSizes, bytesBeforeNextLine)
              where bytesBeforeNextLine = bytesBeforeLine + lengthInBytes line + 1
          lineFromMap ln = fromMaybe ("", 0) (ln `M.lookup` mls)
          lengthInBytes = BS.length . encodeUtf8
          {-| get information about term from map of lines
              In most cases, PosInFile after getTagsFromExpr point to byte before term.
              It's better to have term in term pattern, so this function finds and updates
              PosInFile and generate pattern.
          -} 
          infoForText
              :: Text
              -- ^ term to find
              -> Int
              -- ^ line where to start
              -> Maybe (Int, Int, Text)
              -- ^ (Line number, byte offset, pattern to find term in file)
          infoForText _ 0 = Nothing
          infoForText term ln
              | T.null part2 = infoForText term (ln - 1)
              | otherwise = Just (ln, lsl + 1 + lengthInBytes part1, part1 <> termAndNext)
              where (l, lsl) = lineFromMap ln
                    (part1, part2) = T.breakOn term l
                    termAndNext = T.take (T.length term + 1) part2
          fallbackInfoForText ln c = (ln, lsl + 1 + lengthInBytes pat, pat)
              where (l, lsl) = lineFromMap ln
                    pat = T.take c l

getTagsFromExpr :: Expr Src a -> [(PosInFile, Text)]
getTagsFromExpr = go (FP 0 0) []
    where go lpos mts = \case
              (Let b e) -> go lpos (mts <> parseBinding lpos b) e
              (Annot e1 e2) -> go lpos (go lpos mts e1) e2
              (Record mr) -> mts <> tagsFromDhallMap lpos mr
              (RecordLit mr) -> mts <> tagsFromDhallMap lpos mr
              (Union mmr) -> mts <> tagsFromDhallMapMaybe lpos mmr
              (Note s e) -> go (srcToPosInFile s) mts e
              _ -> mts

          tagsFromDhallMap lpos = foldMapWithKey (tagsFromDhallMapElement lpos)

          tagsFromDhallMapMaybe lpos = foldMapWithKey (\k -> \case
              Just e -> tagsFromDhallMapElement lpos k e
              _ -> [(lpos, k)])

          tagsFromDhallMapElement lpos k e = go pos [(pos, k)] e
              where pos = firstPosFromExpr lpos e

          parseBinding :: PosInFile -> Binding Src a -> [(PosInFile, Text)]
          parseBinding lpos b = go p2 [(p0, variable b)] (value b)
              where p0 = posFromBinding (bindingSrc0 b) lpos
                    p1 = posFromBinding (bindingSrc1 b) p0
                    p2 = posFromBinding (bindingSrc2 b) p1
          posFromBinding src startPos = maybe startPos srcToPosInFile src

srcToPosInFile :: Src -> PosInFile
srcToPosInFile s = FP line column
    where ssp = srcStart s
          line = unPos . sourceLine $ ssp
          column = unPos . sourceColumn $ ssp

firstPosFromExpr :: PosInFile -> Expr Src a -> PosInFile
firstPosFromExpr lpos = \case
    (Note s _) -> srcToPosInFile s
    _ -> lpos

showTags :: Tags -> Text
showTags (Tags ts) = T.concat . map (uncurry showFileTags) . M.toList $ ts

showFileTags :: FilePath -> [(PosInFile, Tag)] -> T.Text
showFileTags f ts = "\x0c\n" <> T.pack f <> "," <> (showInt . T.length) cs <> "\n" <> cs
    where cs = T.concat . map (uncurry showPosTag) $ ts

showPosTag :: PosInFile -> Tag -> Text
showPosTag fp tag = def <>"\x7f" <> name <> "\x01" <> showInt line <>
                    "," <> showInt offset <> "\n"
    where line = fpLine fp
          offset = fpOffset fp
          def = tagPattern tag
          name = tagName tag

showInt :: Int -> Text
showInt = T.pack . show

{-| Generate list of files for a given `Input`
-}
inputToFiles
    :: Bool
    -- ^ If `True`, this function will follow  symbolic links
    -> Maybe [String]
    -- ^ List of suffixes. If `Nothing`, all files will be returned.
    --   This parameter only works when the `Input` is an `InputFile` and point to a directory.
    -> Input
    -> IO [ FilePath ]
    --   List of files.
inputToFiles _ _ StandardInput = lines <$> getContents
inputToFiles followSyms suffixes (InputFile path) = go path
    where go p = do
                   isD <- SD.doesDirectoryExist p
                   isSL <- isSymLink
                   if isD
                     then if isSL && not followSyms
                            then return []
                            else do
                                   -- filter . .. and hidden files .*
                                   contents <- fmap (filter ((/=) '.' . head))
                                                    (SD.getDirectoryContents p)
                                   concat <$> mapM (go . (</>) p) contents
                     else return [p | matchingSuffix || p == path]
               where matchingSuffix = maybe True (any (`isSuffixOf` p)) suffixes
                     isSymLink = 
#if MIN_VERSION_directory(1,3,0)
                                 SD.pathIsSymbolicLink p
#elif MIN_VERSION_directory(1,2,6)
                                 SD.isSymbolicLink pa
#else
                                 return False
#endif
