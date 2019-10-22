{-# LANGUAGE OverloadedStrings #-}

-- | Shared utility functions

module Dhall.Util
    ( snip
    , snipDoc
    , insert
    , _ERROR
    , Censor(..)
    , Input(..)
    , Output(..)
    , getExpression
    , getExpressionAndHeader
    , Header(..)
    ) where

import Data.Bifunctor (first)
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty)
import Dhall.Core (Expr, Import)
import Dhall.Parser (ParseError, Header(..))
import Dhall.Pretty (Ann)
import Dhall.Src (Src)

import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Dhall.Core
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
        <>  separator <> "\n"
        <>  Data.Text.unlines footer

{-| Like `snip`, but for `Doc`s

    Note that this has to be opinionated and render ANSI color codes, but that
    should be fine because we don't use this in a non-interactive context
-}
snipDoc :: Doc Ann -> Doc a
snipDoc doc = Pretty.align (Pretty.pretty (snip text))
  where
    stream = Pretty.layoutSmart Dhall.Pretty.layoutOpts doc

    ansiStream = fmap Dhall.Pretty.annToAnsiStyle stream

    text = Pretty.renderStrict ansiStream

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

get :: (String -> Text -> Either ParseError a) -> Censor -> Input -> IO a
get parser censor input = do
    inText <- do
        case input of
            InputFile file -> Data.Text.IO.readFile file
            StandardInput  -> Data.Text.IO.getContents

    let name =
            case input of
                InputFile file -> file
                StandardInput  -> "(stdin)"

    let result = parser name inText

    let censoredResult =
            case censor of
                NoCensor -> result
                Censor   -> first Dhall.Parser.censor result

    Dhall.Core.throws censoredResult

-- | Set to `Censor` if you want to censor error text that might include secrets
data Censor = NoCensor | Censor

-- | Path to input
data Input = StandardInput | InputFile FilePath

-- | Path to output
data Output = StandardOutput | OutputFile FilePath

-- | Convenient utility for retrieving an expression
getExpression :: Censor -> Input -> IO (Expr Src Import)
getExpression = get Dhall.Parser.exprFromText

-- | Convenient utility for retrieving an expression along with its header
getExpressionAndHeader :: Censor -> Input -> IO (Header, Expr Src Import)
getExpressionAndHeader = get Dhall.Parser.exprAndHeaderFromText
