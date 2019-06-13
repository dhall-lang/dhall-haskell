module Dhall.LSP.Handlers.Hover (hoverHandler) where

import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Utility as LSP

import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Lens as J

import Dhall.LSP.Backend.Diagnostics
import Dhall.LSP.Util (readUri)

import Control.Lens ((^.))
import qualified Network.URI.Encode as URI
import qualified Data.Text as Text
import Data.Maybe (mapMaybe)

-- | This is a prototype implementation. We should avoid recomputing the
-- diagnostics each time.
hoverHandler :: LSP.LspFuncs () -> J.HoverRequest -> IO ()
hoverHandler lsp request = do
  LSP.logs "LSP Handler: processing HoverRequest"
  let
    uri = request ^. J.params . J.textDocument . J.uri
    (J.Position line col) = request ^. (J.params . J.position)
    fileName = case J.uriToFilePath uri of
      Nothing -> fail "Failed to parse URI in ReqHover."
      Just path -> path
  txt <- readUri lsp uri
  errors <- checkDhall fileName txt
  let
    explanations = mapMaybe (explain txt) errors
    isHovered :: Diagnosis -> Bool
    isHovered (Diagnosis _ Nothing _) = False
    isHovered (Diagnosis _ (Just (Range left right)) _) =
      left <= (line, col) && (line, col) <= right
    hover = case filter isHovered explanations of
      [] -> Nothing
      (diag : _) -> hoverFromDiagnosis diag
  LSP.sendFunc lsp $ LSP.RspHover $ LSP.makeResponseMessage request hover


hoverFromDiagnosis :: Diagnosis -> Maybe J.Hover
hoverFromDiagnosis (Diagnosis _ Nothing _) = Nothing
hoverFromDiagnosis (Diagnosis _ (Just (Range left right)) diagnosis) = Just
  J.Hover { .. }
  where
    _range =
      Just $ J.Range (uncurry J.Position left) (uncurry J.Position right)
    encodedDiag = URI.encode (Text.unpack diagnosis)
    command =
      "[Explain error](dhall-explain:?" <> Text.pack encodedDiag <> " )"
    _contents = J.List [J.PlainString command]
