module Dhall.LSP.Handlers.Hover (hoverHandler) where

import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Utility as LSP

import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Lens as J

import Dhall.Core (pretty)

import Dhall.LSP.Backend.Dhall
import Dhall.LSP.Backend.Diagnostics
import Dhall.LSP.Handlers.Diagnostics (explainDiagnosis)
import Dhall.LSP.Backend.Typing
import Dhall.LSP.Util (readUri)

import Control.Lens ((^.))
import qualified Network.URI.Encode as URI
import qualified Data.Text as Text

-- | This is a prototype implementation. We should avoid recomputing the
-- diagnostics each time.
hoverHandler :: LSP.LspFuncs () -> J.HoverRequest -> IO ()
hoverHandler lsp request = do
  LSP.logs "LSP Handler: processing HoverRequest"
  let uri = request ^. J.params . J.textDocument . J.uri
      (J.Position line col) = request ^. (J.params . J.position)
      pos = (line, col)
      fileName = case J.uriToFilePath uri of
        Nothing -> fail "Failed to parse URI in ReqHover."
        Just path -> path
  txt <- readUri lsp uri
  -- Explain takes priority
  mexplain <- explainDiagnosis fileName txt (line, col)
  case mexplain of
    Just explanation -> LSP.sendFunc lsp
                         $ LSP.RspHover
                         $ LSP.makeResponseMessage
                           request (hoverFromDiagnosis explanation)
    Nothing -> do  -- infer type
      mexpr <- loadDhallExprSafe fileName txt
      case mexpr of
        Nothing -> LSP.sendFunc lsp $ LSP.RspHover
                                    $ LSP.makeResponseMessage request Nothing
        Just expr ->
          case typeAt pos expr of
            Right typ ->
              let _range = fmap (rangeToJSON . sanitiseRange txt . rangeFromDhall)
                                (srcAt pos expr)
                  _contents = J.List [J.PlainString (pretty typ)]
                  hover = J.Hover{..}
              in LSP.sendFunc lsp $ LSP.RspHover
                                  $ LSP.makeResponseMessage request (Just hover)
            _ -> LSP.sendFunc lsp $ LSP.RspHover
                                        $ LSP.makeResponseMessage request Nothing


rangeToJSON :: Range -> J.Range
rangeToJSON (Range (x1,y1) (x2,y2)) = J.Range (J.Position x1 y1) (J.Position x2 y2)

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
