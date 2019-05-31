module Backend.Dhall.Linting
    ( lintDocument
    )
where

import Dhall.Pretty (CharacterSet(..), layoutOpts)
import Dhall.Parser (exprAndHeaderFromText, ParseError(..))
import Dhall.Lint (lint)

import Data.Text (Text)
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty.Text
import qualified Dhall.Pretty

-- copy-pasted from Backend.Dhall.Formatting
lintDocument :: Text -> Either ParseError Text
lintDocument text = do
    (header, expr) <- exprAndHeaderFromText "" text
    let doc =
            Pretty.pretty header
                <> Pretty.unAnnotate
                       (Dhall.Pretty.prettyCharacterSet Unicode (lint expr))
                <> "\n"
        formattedText =
            Pretty.Text.renderStrict (Pretty.layoutSmart layoutOpts doc)
    pure formattedText
