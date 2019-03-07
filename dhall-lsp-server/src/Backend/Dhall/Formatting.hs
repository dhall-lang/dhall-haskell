module Backend.Dhall.Formatting(formatDocument) where

import Dhall.Pretty (CharacterSet(..), layoutOpts)
import Dhall.Parser(exprAndHeaderFromText, ParseError(..))

import qualified Data.Text
import qualified Data.Text.Prettyprint.Doc                 as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text     as Pretty.Text
import qualified Dhall.Pretty

formatDocument :: Text -> Either ParseError Text
formatDocument text = do
    (header, expr) <- exprAndHeaderFromText "" text
    let doc =   Pretty.pretty header
                            <>  Pretty.unAnnotate (Dhall.Pretty.prettyCharacterSet Unicode expr)
                            <>  "\n"
        formattedText =
                Pretty.Text.renderStrict (Pretty.layoutSmart layoutOpts doc)
    pure formattedText

