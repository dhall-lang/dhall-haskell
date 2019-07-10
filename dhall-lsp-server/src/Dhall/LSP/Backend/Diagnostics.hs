{-# LANGUAGE RecordWildCards #-}

module Dhall.LSP.Backend.Diagnostics
  ( DhallError
  , diagnose
  , Diagnosis(..)
  , explain
  , offsetToPosition
  , Position
  , positionFromMegaparsec
  , positionToOffset
  , Range(..)
  , rangeFromDhall
  )
where

import Dhall.Parser (SourcedException(..), Src(..), unwrap)
import Dhall.TypeCheck (DetailedTypeError(..), TypeError(..))
import Dhall.Core (Expr(Note))

import Dhall.LSP.Util
import Dhall.LSP.Backend.Dhall

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NonEmpty
import qualified Text.Megaparsec as Megaparsec

-- | A (line, col) pair representing a position in a source file; 0-based.
type Position = (Int, Int)
-- | A source code range.
data Range = Range {left, right :: Position}
-- | A diagnosis, optionally tagged with a source code range.
data Diagnosis = Diagnosis {
    -- | Where the diagnosis came from, e.g. Dhall.TypeCheck.
    doctor :: Text,
    range :: Maybe Range,  -- ^ The range of code the diagnosis concerns
    diagnosis :: Text
    }


-- | Give a short diagnosis for a given error that can be shown to the end user.
diagnose :: DhallError -> [Diagnosis]
diagnose (ErrorInternal e) = [Diagnosis { .. }]
  where
    doctor = "Dhall"
    range = Nothing
    diagnosis =
      "An internal error has occurred while trying to process the Dhall file: "
        <> tshow e

diagnose (ErrorImportSourced (SourcedException src e)) = [Diagnosis { .. }]
  where
    doctor = "Dhall.Import"
    range = Just (rangeFromDhall src)
    diagnosis = tshow e

diagnose (ErrorTypecheck e@(TypeError _ expr _)) = [Diagnosis { .. }]
  where
    doctor = "Dhall.TypeCheck"
    range = fmap rangeFromDhall (note expr)
    diagnosis = tshow e

diagnose (ErrorParse e) =
  [ Diagnosis { .. } | (diagnosis, range) <- zip diagnoses (map Just ranges) ]
  where
    doctor = "Dhall.Parser"
    errors = (NonEmpty.toList . Megaparsec.bundleErrors . unwrap) e
    diagnoses = map (Text.pack . Megaparsec.parseErrorTextPretty) errors
    positions =
      map (positionFromMegaparsec . snd) . fst $ Megaparsec.attachSourcePos
        Megaparsec.errorOffset
        errors
        (Megaparsec.bundlePosState (unwrap e))
    texts = map parseErrorText errors
    ranges =
      [ rangeFromDhall (Src left' left' text)  -- bit of a hack, but convenient.
      | (left, text) <- zip positions texts
      , let left' = positionToMegaparsec left ]
    {- Since Dhall doesn't use custom errors (corresponding to the FancyError
       ParseError constructor) we only need to handle the case of plain
       Megaparsec errors (i.e. TrivialError), and only those who actually
       include a list of tokens that we can compute the length of. -}
    parseErrorText :: Megaparsec.ParseError Text s -> Text
    parseErrorText (Megaparsec.TrivialError _ (Just (Megaparsec.Tokens text)) _) =
      Text.pack (NonEmpty.toList text)
    parseErrorText _ = ""

-- | Give a detailed explanation for the given error; if no detailed explanation
--   is available return @Nothing@ instead.
explain :: DhallError -> Maybe Diagnosis
explain (ErrorTypecheck e@(TypeError _ expr _)) = Just
  (Diagnosis { .. })
  where
    doctor = "Dhall.TypeCheck"
    range = fmap rangeFromDhall (note expr)
    diagnosis = tshow (DetailedTypeError e)
explain _ = Nothing  -- only type errors have detailed explanations so far


-- Given an annotated AST return the note at the top-most node.
note :: Expr s a -> Maybe s
note (Note s _) = Just s
note _ = Nothing


-- Megaparsec's positions are 1-based while ours are 0-based.
positionFromMegaparsec :: Megaparsec.SourcePos -> Position
positionFromMegaparsec (Megaparsec.SourcePos _ line col) =
  (Megaparsec.unPos line - 1, Megaparsec.unPos col - 1)

-- Line and column numbers can't be negative. Clamps to 0 just in case.
positionToMegaparsec :: Position -> Megaparsec.SourcePos
positionToMegaparsec (line, col) = Megaparsec.SourcePos ""
                                     (Megaparsec.mkPos $ max 0 line + 1)
                                     (Megaparsec.mkPos $ max 0 col + 1)

-- | Convert a source range from Dhalls @Src@ format. The returned range is
--   "tight", that is, does not contain any trailing whitespace.
rangeFromDhall :: Src -> Range
rangeFromDhall (Src left _right text) = Range (x1,y1) (x2,y2)
  where
    (x1,y1) = positionFromMegaparsec left
    (dx2,dy2) = offsetToPosition text . Text.length $ Text.stripEnd text
    (x2,y2) | dx2 == 0 = (x1, y1 + dy2)
            | otherwise = (x1 + dx2, dy2)

-- Convert a (line,column) position into the corresponding character offset
-- and back, such that the two are inverses of eachother.
positionToOffset :: Text -> Position -> Int
positionToOffset txt (line, col) = if line < length ls
  then Text.length . unlines' $ take line ls ++ [Text.take col (ls !! line)]
  else Text.length txt  -- position lies outside txt
  where ls = NonEmpty.toList (lines' txt)

offsetToPosition :: Text -> Int -> Position
offsetToPosition txt off = (length ls - 1, Text.length (NonEmpty.last ls))
  where ls = lines' (Text.take off txt)
