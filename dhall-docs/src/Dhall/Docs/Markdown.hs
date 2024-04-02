{-| Contains all utilities related to markdown processing
-}
module Dhall.Docs.Markdown
    ( MarkdownParseError(..)
    , MMark
    , parseMarkdown
    , markdownToHtml
    , MMark.render
    ) where

import Data.Text       (Text)
import Lucid
import Path            (File, Path, Rel)
import Text.MMark      (MMarkErr, MMark)
import Text.Megaparsec (ParseErrorBundle (..))

import qualified Path
import qualified Text.MMark as MMark

-- | Wrapper around `MMarkErr` errors
newtype MarkdownParseError = MarkdownParseError
    { unwrap :: ParseErrorBundle Text MMarkErr
    }

{-| Takes a text that could contain markdown and returns the generated HTML.
    If an error occurs while parsing, it also returns the error information.
-}
markdownToHtml
    :: Path Rel File -- ^ Used by `Mmark.parse` for error messages
    -> Text          -- ^ Text to parse
    -> Either MarkdownParseError (Html ())
markdownToHtml relFile contents =
    MMark.render <$> parseMarkdown relFile contents

{-| Takes a text that could contain markdown and returns either the parsed
    markdown or, if parsing fails, the error information.
-}
parseMarkdown
    :: Path Rel File -- ^ Used by `Mmark.parse` for error messages
    -> Text          -- ^ Text to parse
    -> Either MarkdownParseError MMark
parseMarkdown relFile contents =
    case MMark.parse (Path.fromRelFile relFile) contents of
        Left err -> Left MarkdownParseError { unwrap = err }
        Right mmark -> Right mmark
