{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternGuards      #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | This module provides the `Src` type used for source spans in error messages

module Dhall.Src
    ( -- * Type
      Src(..)
    , isWhitespace
    , trailingWhitespace
    ) where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc  (Pretty (..))
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift, lift)
import Text.Megaparsec (SourcePos (SourcePos), mkPos, unPos)

import {-# SOURCE #-} qualified Dhall.Util

import qualified Data.Text       as Text
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Printf     as Printf

-- | Source code extract
data Src = Src
    { srcStart :: !SourcePos
    , srcEnd   :: !SourcePos
    , srcText  :: Text -- Text field is intentionally lazy
    } deriving (Data, Eq, Generic, Ord, Show, NFData)


instance Lift Src where
    lift (Src (SourcePos a b c) (SourcePos d e f) g) =
        [| Src (SourcePos a (mkPos b') (mkPos c')) (SourcePos d (mkPos e') (mkPos f')) g |]
      where
        b' = unPos b
        c' = unPos c
        e' = unPos e
        f' = unPos f


instance Pretty Src where
    pretty (Src begin _ text) =
            pretty (Dhall.Util.snip numberedLines)
        <>  "\n"
        <>  pretty (Megaparsec.sourcePosPretty begin)
      where
        prefix = Text.replicate (n - 1) " "
          where
            n = Megaparsec.unPos (Megaparsec.sourceColumn begin)

        ls = Text.lines (prefix <> text)

        numberOfLines = length ls

        minimumNumber =
            Megaparsec.unPos (Megaparsec.sourceLine begin)

        maximumNumber = minimumNumber + numberOfLines - 1

        numberWidth :: Int
        numberWidth =
            truncate (logBase (10 :: Double) (fromIntegral maximumNumber)) + 1

        adapt n line = Text.pack outputString
          where
            inputString = Text.unpack line

            outputString =
                Printf.printf ("%" <> show numberWidth <> "d│ %s") n inputString

        numberedLines = Text.unlines (zipWith adapt [minimumNumber..] ls)

{-| Retrieve trailing whitespace for Dhall source code

    This is a hacky utility used to implement @dhall format@ support for
    preserving comments
-}
trailingWhitespace :: Text -> Text
trailingWhitespace code = Text.concat (reverse chunks0)
  where
    chunks0 = loop0 code

    loop0 :: Text -> [Text]
    loop0 text
        | Just (rest, candidateChunk) <- breakOnEndInclusive "--" text
        , not (Text.any (== '\n') candidateChunk) =
            candidateChunk : loop0 rest
        | Just (rest, candidateChunk) <- spanEnd isWhitespace text =
            candidateChunk : loop0 rest
        | Just (rest, candidateChunk) <- matchEnd "-}" text
        , Just chunks <- loop1 1 rest =
            candidateChunk : chunks
        | otherwise =
            []

    loop1 :: Int -> Text -> Maybe [Text]
    loop1 0 text =
        Just (loop0 text)
    loop1 !depth text
        | Just (rest, candidateChunk) <- breakOnEndInclusive "-}" text
        , Just chunks <- loop1 (depth + 1) rest =
            Just (candidateChunk : chunks)
        | Just (rest, candidateChunk) <- breakOnEndInclusive "{-" text
        , not (Text.isInfixOf "-}" candidateChunk)
        , Just chunks <- loop1 (depth - 1) rest =
            Just (candidateChunk : chunks)
        | otherwise =
            Nothing

-- TODO: Use `charset`
isWhitespace :: Char -> Bool
isWhitespace character =
        character == ' '
    ||  character == '\t'
    ||  character == '\n'
    ||  character == '\r'

breakOnEndInclusive :: Text -> Text -> Maybe (Text, Text)
breakOnEndInclusive needle haystack = do
    let (rest, suffix) = Text.breakOnEnd needle haystack

    (prefix, _) <- matchEnd needle rest

    return (prefix, needle <> suffix)

spanEnd :: (Char -> Bool) -> Text -> Maybe (Text, Text)
spanEnd predicate text
    | Text.null suffix = Nothing
    | otherwise        = Just (prefix, suffix)
  where
    prefix = Text.dropWhileEnd predicate text
    suffix = Text.takeWhileEnd predicate text

matchEnd :: Text -> Text -> Maybe (Text, Text)
matchEnd expectedSuffix text
    | expectedSuffix == actualSuffix = Just (prefix, actualSuffix)
    | otherwise                      = Nothing
  where
    len = Text.length expectedSuffix

    actualSuffix = Text.takeEnd len text

    prefix = Text.dropEnd len text
