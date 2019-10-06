{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the implementation of the @dhall tags@ command

module Dhall.ETags
    ( -- * ETags
      generate
    ) where

import Dhall.Util (Input(..))
import Dhall.Core (Expr(..), Binding(..))
import Dhall.Src (Src(..))
import qualified Dhall.Map as DM (keys)
import Dhall.Parser
import Data.Text (Text)
import Control.Monad
import Control.Exception
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Data.List (isSuffixOf)
import System.Directory ( doesDirectoryExist
#if MIN_VERSION_directory(1,3,0)
                        , pathIsSymbolicLink
#else
                        , isSymbolicLink
#endif
                        , getDirectoryContents
                        )
import System.FilePath ((</>), takeFileName)
import Text.Megaparsec (sourceLine, sourceColumn, unPos)

data FilePos = FP
    { fpLine :: Int
    , fpOffset :: Int
    } deriving (Eq, Ord)

newtype Tags = Tags (M.Map FilePath (M.Map FilePos Tag))

instance Semigroup Tags where
    (Tags ts1) <> (Tags ts2) = Tags (M.unionWith M.union ts1 ts2)

data Tag = Tag
    { tagDefinitioni :: Text
    , tagName :: Text
    }

{-| Generate ETags for Dhall expressions

    Arguments:

    * Path to directory or filename that should be indexed
    * List of suffixes for dhall files [".dhall"] by default
    * Flag if generate should follow symlinks
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

{-| Find tags in Text (second argument) and generates list of them
    To make tags for filenames working in both, emacs and vi, add two initial tags.
    First for `filename` for vi and second with `/filename` for emacs.
    Other tags are working for both.
-}
tags :: FilePath -> Text -> Tags
tags f t = Tags (M.singleton f
                 (M.union initialMap (parse t)))
    where initialMap = M.fromList [ (FP 1 0, Tag "" (T.pack . takeFileName $ f))
                                  , (FP 1 1, Tag "" ("/" <> (T.pack . takeFileName) f))
                                  ]

parse :: Text -> M.Map FilePos Tag
parse t = M.fromAscList . map (\(FP ln c, Tag _ term) ->
              let (lsl, l) = mls M.! ln in
              (FP ln (lsl + c), Tag (T.take c l) term)) . M.assocs $
          parseExpr e (FP 0 0) M.empty
    where e = case exprFromText "" t of
                  Left _ -> None
                  Right r -> r
          mls = M.fromList . fst . foldl (\(ls, lsl) (n, l) ->
                                             let lsl' = lsl + 1 + T.length l in
                                             ((n, (lsl, l)):ls, lsl'))
                                        ([], 0) . zip [1..] $ T.lines t

parseExpr :: Expr Src a -> FilePos -> M.Map FilePos Tag -> M.Map FilePos Tag
parseExpr (Let b e) lpos  mts = parseExpr e lpos (parseBinding b mts)
parseExpr (Annot e1 e2) lpos mts = parseExpr e1 lpos (parseExpr e2 lpos mts)
parseExpr (Record mr) lpos mts = foldr (\k mts' ->
    M.insert lpos (Tag "" k) mts') mts . DM.keys $  mr
parseExpr (RecordLit mr) lpos mts = foldr (\k mts' ->
    M.insert lpos (Tag "" k) mts') mts . DM.keys $ mr
parseExpr (Union mmr) lpos mts = foldr (\k mts' ->
    M.insert lpos (Tag "" k) mts') mts . DM.keys $ mmr
parseExpr (Note s e) _ mts = parseExpr e (srcToFilePos s) mts
parseExpr _ _ mts = mts

parseBinding :: Binding Src a -> M.Map FilePos Tag -> M.Map FilePos Tag
parseBinding b = M.insert (maybeSrcToFilePos . bindingSrc0 $ b)
                          (Tag "" var)
    where var = variable b

maybeSrcToFilePos :: Maybe Src -> FilePos
maybeSrcToFilePos Nothing = FP 0 0
maybeSrcToFilePos (Just s) = srcToFilePos s

srcToFilePos :: Src -> FilePos
srcToFilePos s = FP line column
    where ssp = srcStart s
          line = unPos . sourceLine $ ssp
          column = unPos . sourceColumn $ ssp

showTags :: Tags -> Text
showTags (Tags ts) = T.concat . M.elems . M.mapWithKey showFileTags $ ts

showFileTags :: FilePath -> M.Map FilePos Tag -> T.Text
showFileTags f ts = "\x0c\n" <> T.pack f <> "," <> (showInt . T.length) cs <> "\n" <> cs
    where cs = T.concat . M.elems . M.mapWithKey showPosTag $ ts

showPosTag :: FilePos -> Tag -> Text
showPosTag (FP line shift) (Tag def term) = def <>"\x7f" <> term <> "\x01" <> showInt line <>
                                             "," <> showInt shift <> "\n"

showInt :: Int -> Text
showInt = T.pack . show

{-| from https://github.com/MarcWeber/hasktags/blob/master/src/Hasktags.hs
    suffixes: [".dhall"], use "" to match all files
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
