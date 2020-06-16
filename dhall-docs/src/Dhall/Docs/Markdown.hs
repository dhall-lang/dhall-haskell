{-| Contains all utilities related to markdown processing
-}
module Dhall.Docs.Markdown where

import Data.Text       (Text)
import Lucid           (Html)
import Path            (Abs, File, Path)
import Text.Megaparsec (ParseErrorBundle (..))
import Text.MMark      (MMarkErr)

import qualified Lucid
import qualified Path
import qualified Text.MMark as MMark

type ParseError = ParseErrorBundle Text MMarkErr

{-| Takes a text that could contain markdown and returns the generated HTML.
    If an error occurs while parsing, it also returns the error information.
-}
markdownToHtml
    :: Path Abs File -- ^ Used by `Mmark.parse` for error messages
    -> Text          -- ^ Text to parse
    -> (Maybe ParseError, Html ())
markdownToHtml absFile contents =
    case MMark.parse (Path.fromAbsFile absFile) contents of
        Left err -> (Just err, Lucid.toHtml contents)
        Right mmark -> (Nothing, MMark.render mmark)
