{-# LANGUAGE RecordWildCards #-}

module Dhall.LSP.Backend.Diagnostics
  ( DhallException
  , checkDhall
  , diagnose
  , Diagnosis(..)
  , explain
  , offsetToPosition
  , Position
  , positionFromMegaparsec
  , positionToOffset
  , Range(..)
  , rangeFromDhall
  , sanitiseRange
  )
where

import Dhall.Binary (DecodingFailure)
import Dhall.Parser (ParseError, SourcedException(..), Src(..), unwrap)
import Dhall.Import (MissingImports)
import Dhall.TypeCheck (DetailedTypeError(..), TypeError(..), X)
import Dhall.Core (Expr(Note))

import Dhall.LSP.Util
import Dhall.LSP.Backend.Dhall (runDhall)

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Exception (handle, SomeException)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Text.Megaparsec as Megaparsec

-- | An exception that occurred while trying to parse, type-check and normalise
--   the input. TODO: make this list exhaustive! We currently report too many
--   exceptions as "internal errors".
data DhallException
  = ExceptionInternal SomeException
  | ExceptionCBOR DecodingFailure  -- CBOR decoding failure (not relevant?)
  | ExceptionImport (SourcedException MissingImports)  -- Failure to resolve an import statement
  | ExceptionTypecheck (TypeError Src X)  -- Input does not type-check
  | ExceptionParse ParseError  -- Input does not parse


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


-- | Parse, type-check and normalise the given Dhall code, collecting any
--   occurring errors.
checkDhall :: FilePath -> Text -> IO [DhallException]
checkDhall path txt =
  (handle' ExceptionInternal
    . handle' ExceptionCBOR
    . handle' ExceptionImport
    . handle' ExceptionTypecheck
    . handle' ExceptionParse
    )
    (const [] <$> runDhall path txt)
  where
    handle' constructor = handle (return . return . constructor)

-- | Give a short diagnosis for a given error that can be shown to the end user.
diagnose :: Text -> DhallException -> [Diagnosis]
diagnose _ (ExceptionInternal e) = [Diagnosis { .. }]
  where
    doctor = "Dhall"
    range = Nothing
    diagnosis =
      "An internal error has occurred while trying to process the Dhall file: "
        <> tshow e

diagnose _ (ExceptionCBOR t) = [Diagnosis { .. }]
  where
    doctor = "Dhall.Binary"
    range = Nothing
    diagnosis = "Failed to decode CBOR Dhall representation: " <> tshow t

diagnose txt (ExceptionImport (SourcedException src e)) = [Diagnosis { .. }]
  where
    doctor = "Dhall.Import"
    range = (Just . sanitiseRange txt . rangeFromDhall) src
    diagnosis = tshow e

diagnose txt (ExceptionTypecheck e@(TypeError _ expr _)) = [Diagnosis { .. }]
  where
    doctor = "Dhall.TypeCheck"
    range = fmap (sanitiseRange txt . rangeFromDhall) (note expr)
    diagnosis = tshow e

diagnose txt (ExceptionParse e) =
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
    lengths = map parseErrorLength errors
    ranges =
      [ Range pos r
      | (pos, len) <- zip positions lengths
      , let r = offsetToPosition txt $ positionToOffset txt pos + len
      ]
    {- Since Dhall doesn't use custom errors (corresponding to the FancyError
       ParseError constructor) we only need to handle the case of plain
       Megaparsec errors (i.e. TrivialError), and only those who actually
       include a list of tokens that we can compute the length of. -}
    parseErrorLength :: Megaparsec.ParseError s e -> Int
    parseErrorLength (Megaparsec.TrivialError _ (Just (Megaparsec.Tokens ts)) _)
      = length ts
    parseErrorLength _ = 0


-- | Give a detailed explanation for the given error; if no detailed explanation
--   is available return @Nothing@ instead.
explain :: Text -> DhallException -> Maybe Diagnosis
explain txt (ExceptionTypecheck e@(TypeError _ expr _)) = Just
  (Diagnosis { .. })
  where
    doctor = "Dhall.TypeCheck"
    range = fmap (sanitiseRange txt . rangeFromDhall) (note expr)
    diagnosis = tshow (DetailedTypeError e)
explain _ _ = Nothing  -- only type errors have detailed explanations so far


-- Adjust a given range to exclude any trailing whitespace.
sanitiseRange :: Text -> Range -> Range
sanitiseRange txt (Range l r) = Range l (max l r')
  where
    off = positionToOffset txt r
    r' =
      (offsetToPosition txt . Text.length . Text.stripEnd . Text.take off) txt


-- Given an annotated AST return the note at the top-most node.
note :: Expr s a -> Maybe s
note (Note s _) = Just s
note _ = Nothing


-- Megaparsec's positions are 1-based while ours are 0-based.
positionFromMegaparsec :: Megaparsec.SourcePos -> Position
positionFromMegaparsec (Megaparsec.SourcePos _ line col) =
  (Megaparsec.unPos line - 1, Megaparsec.unPos col - 1)


-- Convert a source range from Dhalls @Src@ format.
rangeFromDhall :: Src -> Range
rangeFromDhall (Src left right _) =
  Range (positionFromMegaparsec left) (positionFromMegaparsec right)


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
