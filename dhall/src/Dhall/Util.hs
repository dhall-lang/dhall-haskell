{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

-- | Shared utility functions

module Dhall.Util
    ( snip
    , snipDoc
    , insert
    , _ERROR
    , Censor(..)
    , Input(..)
    , Transitivity(..)
    , OutputMode(..)
    , Output(..)
    , getExpression
    , getExpressionAndHeader
    , getExpressionAndHeaderFromStdinText
    , Header(..)
    , CheckFailed(..)
    , mapMThrowCheckFailed
    ) where

import Control.Exception         (Exception (..))
import Control.Monad.IO.Class    (MonadIO (..))
import Data.Bifunctor            (first)
import Data.Either               (lefts)
import Data.Foldable             (toList)
import Data.List.NonEmpty        (NonEmpty (..))
import Data.String               (IsString)
import Data.Text                 (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty)
import Dhall.Parser              (Header (..), ParseError)
import Dhall.Pretty              (Ann)
import Dhall.Src                 (Src)
import Dhall.Syntax              (Expr, Import)

import qualified Control.Exception
import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty.Terminal
import qualified Dhall.Parser
import qualified Dhall.Pretty

-- | Utility function to cut out the interior of a large text block
snip :: Text -> Text
snip text
    | length ls <= numberOfLinesOfContext * 2 + 1 = text
    | otherwise =
         if Data.Text.last text == '\n' then preview else Data.Text.init preview
  where
    numberOfLinesOfContext = 20

    ls = Data.Text.lines text

    header = take numberOfLinesOfContext ls

    footer = takeEnd numberOfLinesOfContext ls

    excerpt = filter (Data.Text.any (/= ' ')) (header <> footer)

    leadingSpaces =
        Data.Text.length . Data.Text.takeWhile (== ' ')

    minSpaces = minimum (map leadingSpaces excerpt)

    maxLength = maximum (map Data.Text.length excerpt)

    separator =
            Data.Text.replicate minSpaces " "
        <>  Data.Text.replicate (maxLength - minSpaces) "="

    preview =
            Data.Text.unlines header
        <>  Data.Text.take 80 separator <> "\n"
        <>  Data.Text.unlines footer

{-| Like `snip`, but for `Doc`s

    Note that this has to be opinionated and render ANSI color codes, but that
    should be fine because we don't use this in a non-interactive context
-}
snipDoc :: Doc Ann -> Doc a
snipDoc doc = Pretty.align (Pretty.pretty (snip text))
  where
    stream = Dhall.Pretty.layout doc

    ansiStream = fmap Dhall.Pretty.annToAnsiStyle stream

    text = Pretty.Terminal.renderStrict ansiStream

takeEnd :: Int -> [a] -> [a]
takeEnd n l = go (drop n l) l
  where
    go (_:xs) (_:ys) = go xs ys
    go _ r = r

-- | Function to insert an aligned pretty expression
insert :: Pretty a => a -> Doc Ann
insert expression =
    "↳ " <> Pretty.align (snipDoc (Pretty.pretty expression))

-- | Prefix used for error messages
_ERROR :: IsString string => string
_ERROR = "\ESC[1;31mError\ESC[0m"

get
    :: (String -> Text -> Either ParseError a)
    -> Censor
    -> InputOrTextFromStdin
    -> IO a
get parser censor input = do
    inText <-
        case input of
            Input_ (InputFile file) -> Data.Text.IO.readFile file
            Input_ StandardInput    -> Data.Text.IO.getContents
            StdinText text          -> pure text

    let name =
            case input of
                Input_ (InputFile file) -> file
                Input_ StandardInput    -> "(input)"
                StdinText _             -> "(input)"

    let result = parser name inText

    let censoredResult =
            case censor of
                NoCensor -> result
                Censor   -> first Dhall.Parser.censor result

    throws censoredResult

{-| Convenience utility for converting `Either`-based exceptions to `IO`-based
    exceptions
-}
throws :: (Exception e, MonadIO io) => Either e a -> io a
throws (Left  e) = liftIO (Control.Exception.throwIO e)
throws (Right r) = return r

-- | Set to `Censor` if you want to censor error text that might include secrets
data Censor = NoCensor | Censor

-- | Path to input
data Input = StandardInput | InputFile FilePath deriving (Eq)

-- | Path to input or raw input text, necessary since we can't read STDIN twice
data InputOrTextFromStdin = Input_ Input | StdinText Text

{-| Specifies whether or not an input's transitive dependencies should also be
    processed.  Transitive dependencies are restricted to relative file imports.
-}
data Transitivity
    = NonTransitive
    -- ^ Do not process transitive dependencies
    | Transitive
    -- ^ Process transitive dependencies in the same way

-- | Path to output
data Output = StandardOutput | OutputFile FilePath

{-| Some command-line subcommands can either `Write` their input or `Check`
    that the input has already been modified.  This type is shared between them
    to record that choice.
-}
data OutputMode = Write | Check

-- | Exception thrown when the @--check@ flag to a command-line subcommand fails
data CheckFailed = CheckFailed { command :: Text, modified :: Text, input :: Input }

instance Exception CheckFailed

instance Show CheckFailed where
    show CheckFailed{..} =
         _ERROR <> ": ❰dhall " <> command_ <> " --check❱ failed on " <> input_ <> "\n\
        \\n\
        \You ran ❰dhall " <> command_ <> " --check❱, but the input appears to have not\n\
        \been " <> modified_ <> " before, or was changed since the last time the input\n\
        \was " <> modified_ <> ".\n"
      where
        modified_ = Data.Text.unpack modified

        command_ = Data.Text.unpack command

        input_ = case input of
            StandardInput -> "(input)"
            InputFile file -> "\"" <> file <> "\""

-- | Exception thrown when the @--check@ flag to a command-line subcommand fails
newtype MultipleCheckFailed = MultipleCheckFailed (NonEmpty CheckFailed)

instance Exception MultipleCheckFailed

instance Show MultipleCheckFailed where
    show (MultipleCheckFailed cfs) = unlines . map show $ toList cfs

-- | Helper function similar to mapM_ to run multiple IO but only fail on CheckFailed after all IO is done
mapMThrowCheckFailed :: (Foldable t, Traversable t) => (a -> IO ()) -> t a -> IO ()
mapMThrowCheckFailed f xs = post =<< mapM (Control.Exception.tryJust match . f) xs
  where
    -- Handle CheckFailed exceptions only
    match = Just @CheckFailed

    post results =
        case lefts (toList results) of
            [] -> pure ()
            cf:cfs -> Control.Exception.throwIO (MultipleCheckFailed (cf:|cfs))

-- | Convenient utility for retrieving an expression
getExpression :: Censor -> Input -> IO (Expr Src Import)
getExpression censor = get Dhall.Parser.exprFromText censor . Input_

-- | Convenient utility for retrieving an expression along with its header
getExpressionAndHeader :: Censor -> Input -> IO (Header, Expr Src Import)
getExpressionAndHeader censor =
    get Dhall.Parser.exprAndHeaderFromText censor . Input_

-- | Convenient utility for retrieving an expression along with its header from
-- | text already read from STDIN (so it's not re-read)
getExpressionAndHeaderFromStdinText
    :: Censor -> Text -> IO (Header, Expr Src Import)
getExpressionAndHeaderFromStdinText censor =
    get Dhall.Parser.exprAndHeaderFromText censor . StdinText
