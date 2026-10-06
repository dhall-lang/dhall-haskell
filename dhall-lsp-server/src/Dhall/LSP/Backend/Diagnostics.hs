{-# LANGUAGE RecordWildCards #-}

module Dhall.LSP.Backend.Diagnostics
  ( DhallError
  , diagnose
  , Diagnosis(..)
  , explain
  , embedsWithRanges
  , offsetToPosition
  , Position
  , positionFromMegaparsec
  , positionToOffset
  , Range(..)
  , rangeFromDhall
  , stripTrailingComments
  , subtractPosition
  , clipUserText
  )
where

import Dhall.Core      (Expr (Embed, Note), subExpressions)
import Dhall.Parser    (SourcedException (..), Src (..), unwrap)
import Dhall.TypeCheck
    ( DetailedTypeError (..)
    , ErrorMessages (..)
    , TypeError (..)
    , TypeMessage (..)
    )

import Dhall.LSP.Backend.Dhall
import Dhall.LSP.Backend.Parsing (getImportLink)
import Dhall.LSP.Util

import Control.Lens               (toListOf)
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.Text                  (Text)

import qualified Data.List.NonEmpty        as NonEmpty
import qualified Data.Text                 as Text
import Prettyprinter                   (Pretty, pretty)
import qualified Dhall.Bounded             as Bounded
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck           as TypeCheck
import qualified Prettyprinter.Render.Text as Pretty.Text
import qualified Text.Megaparsec           as Megaparsec

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
    diagnosis = clipUserText (tshow e)

diagnose (ErrorTypecheck (TypeError _ expr message)) = [Diagnosis { .. }]
  where
    doctor = "Dhall.TypeCheck"

    range = fmap rangeFromDhall (note expr)

    diagnosis =
        "Error: "
            <> Pretty.Text.renderStrict (Dhall.Pretty.layout short)
            <> assertionSides message

    ErrorMessages{..} = TypeCheck.prettyTypeMessage message

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

-- | The two sides of a failed assertion, each cut at 2KiB.
--
--   For a value mismatch the short type error is only a diff.  For a
--   type mismatch the short message names neither type.  A cut side
--   tells the user to run Explain error.
assertionSides :: Pretty a => TypeMessage Src a -> Text
assertionSides (TypeCheck.AssertionFailed left right) =
    clippedPair left right
assertionSides (TypeCheck.EquivalenceTypeMismatch _ tyL _ tyR) =
    clippedPair tyL tyR
assertionSides _ =
    ""

clippedPair :: Pretty a => Expr Src a -> Expr Src a -> Text
clippedPair left right =
    let (leftText, leftCut) = clipSide left
        (rightText, rightCut) = clipSide right
        clipped =
            if leftCut || rightCut
                then "\n\nRun Explain error to see the rest."
                else ""
    in "\n\n" <> leftText <> "\n\n" <> rightText <> clipped

clipSide :: Pretty a => Expr Src a -> (Text, Bool)
clipSide expr =
    case Bounded.prettyBounded sideBytes (Dhall.Pretty.prettyCharacterSet Dhall.Pretty.Unicode expr) of
        Bounded.Complete text ->
            (text, False)
        Bounded.Truncated text ->
            (text, True)
  where
    sideBytes = userTextLimit

-- | Give a detailed explanation for the given error; if no detailed explanation
--   is available return @Nothing@ instead.
--
--   The text is capped at the given number of characters.
explain :: Int -> DhallError -> Maybe Diagnosis
explain limit (ErrorTypecheck e@(TypeError _ expr _)) = Just
  (Diagnosis { .. })
  where
    doctor = "Dhall.TypeCheck"
    range = fmap rangeFromDhall (note expr)
    diagnosis =
        case Bounded.prettyBounded limit (pretty (DetailedTypeError e)) of
            Bounded.Complete text ->
                text
            Bounded.Truncated text ->
                text
explain limit (ErrorParse err) =
    case diagnose (ErrorParse err) of
        [] ->
            Nothing
        ds@(Diagnosis _ range_ _ : _) ->
            Just (Diagnosis "Dhall.Parser" range_ (Text.take limit body))
          where
            body = Text.intercalate "\n\n" [ text | Diagnosis _ _ text <- ds ]
explain _ _ = Nothing


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

addRelativePosition :: Position -> Position -> Position
addRelativePosition (x1, y1) (0, dy2) = (x1, y1 + dy2)
addRelativePosition (x1, _) (dx2, y2) = (x1 + dx2, y2)

-- | prop> addRelativePosition pos (subtractPosition pos pos') == pos'
subtractPosition :: Position -> Position -> Position
subtractPosition (x1, y1) (x2, y2) | x1 == x2 = (0, y2 - y1)
                                   | otherwise = (x2 - x1, y2)

-- | Convert a source range from Dhalls @Src@ format. The returned range is
--   "tight", that is, does not contain any trailing whitespace or comments.
rangeFromDhall :: Src -> Range
rangeFromDhall (Src left _right text) = Range (x1,y1) (x2,y2)
  where
    (x1,y1) = positionFromMegaparsec left
    (dx2,dy2) = offsetToPosition text . Text.length $ stripTrailingComments text
    (x2,y2) = addRelativePosition (x1,y1) (dx2,dy2)

-- | Drop trailing whitespace and comments.  The parser's source spans can
--   include the whitespace and comments that follow an expression, and
--   neither should be part of a diagnostic underline.
stripTrailingComments :: Text -> Text
stripTrailingComments = loop . Text.stripEnd
  where
    loop text =
        case blockCommentSuffix text of
            Just before ->
                loop (Text.stripEnd before)
            Nothing ->
                case lineCommentSuffix text of
                    Just before ->
                        loop (Text.stripEnd before)
                    Nothing ->
                        text

-- | If the text ends with a block comment, return the text before that
--   comment.  Block comments nest, so the matching opener is found by
--   counting closers and openers from the end.
blockCommentSuffix :: Text -> Maybe Text
blockCommentSuffix text
    | not ("-}" `Text.isSuffixOf` text) =
        Nothing
    | otherwise =
        scan (Text.length text - 3) (1 :: Int)
  where
    scan i depth
        | i < 0 =
            Nothing
        | Text.take 2 (Text.drop i text) == "-}" =
            scan (i - 1) (depth + 1)
        | Text.take 2 (Text.drop i text) == "{-" =
            if depth == 1
                then Just (Text.take i text)
                else scan (i - 1) (depth - 1)
        | otherwise =
            scan (i - 1) depth

-- | If the last line ends in a @--@ comment, return the text before it.
lineCommentSuffix :: Text -> Maybe Text
lineCommentSuffix text = do
    column <- findLineComment lastLine
    Just (Text.take (Text.length text - Text.length lastLine + column) text)
  where
    lastLine = Text.takeWhileEnd (/= '\n') text

-- | The start of the first @--@ outside a string literal, if any.  String
--   tracking is single-line only: @${}@ interpolation is not entered, so a
--   @--@ inside an interpolated string literal can still look like a
--   comment.
findLineComment :: Text -> Maybe Int
findLineComment = scan 0 Normal
  where
    scan n state rest =
        case Text.uncons rest of
            Nothing ->
                Nothing
            Just (c, cs) ->
                case state of
                    Normal
                        | c == '-', Just ('-', _) <- Text.uncons cs ->
                            Just n
                        | c == '"' ->
                            scan (n + 1) InString cs
                        | c == '\'', Just ('\'', cs') <- Text.uncons cs ->
                            scan (n + 2) InMulti cs'
                        | otherwise ->
                            scan (n + 1) Normal cs
                    InString
                        | c == '\\', Just (_, cs') <- Text.uncons cs ->
                            scan (n + 2) InString cs'
                        | c == '"' ->
                            scan (n + 1) Normal cs
                        | otherwise ->
                            scan (n + 1) InString cs
                    InMulti
                        | c == '\'', Just ('\'', cs') <- Text.uncons cs ->
                            case Text.uncons cs' of
                                -- ''' is an escaped '' inside a multi-line literal
                                Just ('\'', cs'') ->
                                    scan (n + 3) InMulti cs''
                                _ ->
                                    scan (n + 2) Normal cs'
                        | otherwise ->
                            scan (n + 1) InMulti cs

data LineScan = Normal | InString | InMulti

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

-- | Collect all `Embed` constructors (i.e. imports if the expression has type
--   `Expr Src Import`) wrapped in a Note constructor and return them together
--   with their associated range in the source code.
embedsWithRanges :: Expr Src a -> [(Range, a)]
embedsWithRanges =
  map (\(src, a) -> (rangeFromDhall . getImportLink $ src, a)) . execWriter . go
  where go :: Expr Src a -> Writer [(Src, a)] ()
        go (Note src (Embed a)) = tell [(src, a)]
        go expr = mapM_ go (toListOf subExpressions expr)

-- | Cap for diagnostic and hover text shown in the editor.
userTextLimit :: Int
userTextLimit = 2048

-- | Keep at most 'userTextLimit' characters, with an ellipsis when cut.
clipUserText :: Text -> Text
clipUserText text
    | Text.length text <= userTextLimit =
        text
    | otherwise =
        Text.take userTextLimit text <> "…"
