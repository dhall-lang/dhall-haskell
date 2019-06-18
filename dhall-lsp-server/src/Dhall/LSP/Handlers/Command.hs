module Dhall.LSP.Handlers.Command (executeCommandHandler) where

import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Utility as LSP

import qualified Data.Aeson as J
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Lens as J

import Dhall.LSP.Backend.Dhall
import qualified Dhall.LSP.Backend.Linting as Linting
import qualified Dhall.LSP.Backend.ToJSON as ToJSON
import Dhall.LSP.Util (readUri)
import Dhall.LSP.Backend.Typing (annotateLet)
import Dhall.Parser (exprAndHeaderFromText)

import System.FilePath (replaceExtension)
import Data.HashMap.Strict (singleton)
import Control.Lens ((^.))
import Data.Text (Text)
import qualified Data.Text as Text

import Text.Megaparsec (SourcePos(..), unPos)
import Dhall.Parser (Src(..))

executeCommandHandler :: LSP.LspFuncs () -> J.ExecuteCommandRequest -> IO ()
executeCommandHandler lsp request
  | command == "dhall.server.lint" = case parseUriArgument request of
      Right uri -> executeLintAndFormat lsp uri
      Left msg -> LSP.logs msg
  | command == "dhall.server.toJSON" = case parseUriArgument request of
      Right uri -> executeDhallToJSON lsp uri
      Left msg -> LSP.logs msg
  | command == "dhall.server.annotateLet" = executeAnnotateLet lsp request
  | otherwise = LSP.logs
    ("LSP Handler: asked to execute unknown command: " ++ show command)
  where command = request ^. J.params . J.command

-- implements dhall.server.toJSON
executeDhallToJSON :: LSP.LspFuncs () -> J.Uri -> IO ()
executeDhallToJSON lsp uri = do
  txt <- readUri lsp uri
  let filepath = case J.uriToFilePath uri of
        Nothing -> fail "Failed to parse URI when converting Dhall to JSON."
        Just path -> path
  mconverted <- ToJSON.dhallToJSON filepath txt
  case mconverted of
    Just converted -> do
      let edit = J.List [ J.TextEdit (J.Range (J.Position 0 0) (J.Position 0 0))
                                     converted ]
          -- TODO: this doesn't work; we need to fix haskell-lsp-types to
          -- support file creation!
          edits = case appendSuffixToUri uri ".json" of
            Right uri' -> Just (singleton uri' edit)
            _ -> Nothing
      lid <- LSP.getNextReqId lsp
      LSP.sendFunc lsp $ LSP.ReqApplyWorkspaceEdit
                       $ LSP.fmServerApplyWorkspaceEditRequest lid
                       $ J.ApplyWorkspaceEditParams
                       $ J.WorkspaceEdit edits Nothing
    Nothing -> LSP.sendFunc lsp $ LSP.NotShowMessage
                                $ LSP.fmServerShowMessageNotification J.MtError
                                  "Failed to convert Dhall to JSON. Make sure\
                                  \ the Dhall file is free of errors first!"

-- implements dhall.server.lint
executeLintAndFormat :: LSP.LspFuncs () -> J.Uri -> IO ()
executeLintAndFormat lsp uri = do
  txt <- readUri lsp uri
  case Linting.lintAndFormatDocument txt of
    Right linted -> do
      let endline = length $ Text.lines txt
      let edit = J.List [ J.TextEdit
                           (J.Range (J.Position 0 0) (J.Position endline 0))
                           linted ]
      lid <- LSP.getNextReqId lsp
      LSP.sendFunc lsp $ LSP.ReqApplyWorkspaceEdit
                       $ LSP.fmServerApplyWorkspaceEditRequest lid
                       $ J.ApplyWorkspaceEditParams
                       $ J.WorkspaceEdit (Just (singleton uri edit)) Nothing
    _ -> LSP.logs "LSP Handler: linting failed"

-- Helper that appends a suffix to a uri. Fails if the uri does not represent a
-- file path.
appendSuffixToUri :: J.Uri -> Text -> Either String J.Uri
appendSuffixToUri uri suffix = case J.uriToFilePath uri of
  Just path -> Right . J.filePathToUri $ replaceExtension path (show suffix)
  Nothing -> Left $ "failed to append suffix to uri " ++ show uri
                    ++ " because it's not a valid file path"

parseUriArgument :: J.ExecuteCommandRequest -> Either String J.Uri
parseUriArgument request = case request ^. J.params . J.arguments of
  Just (J.List (x : _)) -> case J.fromJSON x of
    J.Success uri -> Right uri
    _ -> Left $ "unable to parse uri argument to "
                <> show (request ^. J.params . J.command)
  _ -> Left $ "unable to parse uri argument to "
              <> show (request ^. J.params . J.command)

srcToRange :: Src -> J.Range
srcToRange (Src (SourcePos _ x1 y1) (SourcePos _ x2 y2) _) =
  J.Range (J.Position (unPos x1 - 1) (unPos y1 - 1))
          (J.Position (unPos x2 - 1) (unPos y2 - 1))

executeAnnotateLet :: LSP.LspFuncs () -> J.ExecuteCommandRequest -> IO ()
executeAnnotateLet lsp request = do
  LSP.logs "LSP Handler: executing AnnotateLet"
  let Just (J.List (arg1 : _)) = request ^. J.params . J.arguments
      J.Success textDocPos = J.fromJSON arg1 :: J.Result J.TextDocumentPositionParams
      uri = textDocPos ^. J.textDocument . J.uri
      (J.Position line col) = textDocPos ^. J.position
      Just fileName = J.uriToFilePath uri
  txt <- readUri lsp uri
  Just expr <- loadDhallExprSafe fileName txt
  let Right (header, _) = exprAndHeaderFromText "" txt
      Just (src, txt') = annotateLet (line, col) expr
      edit = J.List [ J.TextEdit (srcToRange src) txt' ]
  lid <- LSP.getNextReqId lsp
  LSP.sendFunc lsp $ LSP.ReqApplyWorkspaceEdit
                   $ LSP.fmServerApplyWorkspaceEditRequest lid
                   $ J.ApplyWorkspaceEditParams
                   $ J.WorkspaceEdit (Just (singleton uri edit)) Nothing
