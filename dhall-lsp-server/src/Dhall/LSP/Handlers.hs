module Dhall.LSP.Handlers where

import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Lens as J
import qualified Language.Haskell.LSP.VFS as LSP
import qualified Data.Aeson as J
import qualified Yi.Rope as Rope

import Dhall.Core (Expr, pretty)
import Dhall.Parser (Src(..))
import Dhall.TypeCheck (X)

import Dhall.LSP.Backend.Dhall (FileIdentifier, parse, load, typecheck,
  normalize, fileIdentifierFromFilePath, fileIdentifierFromURI, invalidate,
  cacheExpr, parseWithHeader)
import Dhall.LSP.Backend.Diagnostics (Range(..), Diagnosis(..), explain,
  sanitiseRange, rangeFromDhall, diagnose)
import Dhall.LSP.Backend.Formatting (formatExprWithHeader)
import Dhall.LSP.Backend.Linting (Suggestion(..), suggest, lint)
import Dhall.LSP.Backend.Typing (typeAt, srcAt, annotateLet)
import Dhall.LSP.State

import GHC.Conc (atomically)
import Control.Applicative ((<|>))
import Control.Concurrent.STM.TVar
import Control.Lens ((^.))
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except (throwE, catchE, runExceptT)
import Control.Monad.Trans.State.Strict (execStateT)
import Control.Monad.Trans.State.Strict (gets, modify)
import qualified Data.HashMap.Strict as HashMap
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import Lens.Family (view, set, over)
import qualified Network.URI as URI
import qualified Network.URI.Encode as URI
import Text.Megaparsec (SourcePos(..), unPos)


-- Workaround to make our single-threaded LSP fit dhall-lsp's API, which
-- expects a multi-threaded implementation.
wrapHandler
  :: TVar (Maybe ServerState)
  -> (a -> HandlerM ())
  -> a
  -> IO ()
wrapHandler vstate handle message = do
  mstate <- readTVarIO vstate
  case mstate of
    Just state -> do
      state' <- (execStateT . runExceptT) (catchE (handle message) lspUserMessage) state
      atomically . writeTVar vstate $ Just state'
    Nothing ->
      fail "A handler was called before the language server was initialized\
           \ properly. This should never happen."

lspUserMessage :: (Severity, Text) -> HandlerM ()
lspUserMessage (Log, text) =
  lspSendNotification LSP.NotLogMessage J.WindowLogMessage
    $ J.LogMessageParams J.MtLog text
lspUserMessage (severity, text) =
  lspSendNotification LSP.NotShowMessage J.WindowShowMessage
    $ J.ShowMessageParams severity' text
  where severity' = case severity of
          Error -> J.MtError
          Warning -> J.MtWarning
          Info -> J.MtInfo
          Log -> J.MtLog


lspSend :: LSP.FromServerMessage -> HandlerM ()
lspSend msg = do
  send <- lift $ gets (view (lspFuncs . sendFunc))
  liftIO $ send msg

lspRespond :: (J.ResponseMessage response -> LSP.FromServerMessage)
  -> J.RequestMessage J.ClientMethod request response -> response -> HandlerM ()
lspRespond constructor request response =
  lspSend . constructor $ LSP.makeResponseMessage request response

lspSendNotification
  :: (J.NotificationMessage J.ServerMethod params -> LSP.FromServerMessage)
  -> J.ServerMethod -> params -> HandlerM ()
lspSendNotification constructor method params =
  lspSend . constructor $ J.NotificationMessage "2.0" method params

lspRequest
  :: (J.RequestMessage J.ServerMethod params response -> LSP.FromServerMessage)
  -> J.ServerMethod -> params -> HandlerM ()
lspRequest constructor method params = do
  getNextReqId <- lift $ gets (LSP.getNextReqId . view lspFuncs)
  reqId <- liftIO getNextReqId
  lspSend . constructor $ J.RequestMessage "2.0" reqId method params

-- | A helper function to query haskell-lsp's VFS.
readUri :: J.Uri -> HandlerM Text
readUri uri = do
  getVirtualFileFunc <- lift $ gets (LSP.getVirtualFileFunc . view lspFuncs)
  asd <- liftIO $ getVirtualFileFunc uri
  case asd of
    Just (LSP.VirtualFile _ rope) -> return (Rope.toText rope)
    Nothing -> fail $ "Could not find " <> show uri <> " in VFS."

loadFile :: J.Uri -> HandlerM (Expr Src X)
loadFile uri = do
  txt <- readUri uri
  fileIdentifier <- fileIdentifierFromUri uri
  cache <- lift $ gets (view importCache)

  expr <- case parse txt of
    Right e -> return e
    _ -> throwE (Error, "Failed to parse Dhall file.")

  loaded <- liftIO $ load fileIdentifier expr cache
  (cache', expr') <- case loaded of
    Right x -> return x
    _ -> throwE (Error, "Failed to resolve imports.")
  -- Update cache. Don't cache current expression because it might not have been
  -- written to disk yet (readUri reads from the VFS).
  lift $ modify (set importCache cache')
  return expr'

-- helper
fileIdentifierFromUri :: J.Uri -> HandlerM FileIdentifier
fileIdentifierFromUri uri =
  let mFileIdentifier = fmap fileIdentifierFromFilePath (J.uriToFilePath uri)
                        <|> (do uri' <- (URI.parseURI . Text.unpack . J.getUri) uri
                                fileIdentifierFromURI uri')
  in case mFileIdentifier of
    Just fileIdentifier -> return fileIdentifier
    Nothing -> throwE (Error, J.getUri uri <> " is not a valid name for a dhall file.")

-- helper
rangeToJSON :: Range -> J.Range
rangeToJSON (Range (x1,y1) (x2,y2)) = J.Range (J.Position x1 y1) (J.Position x2 y2)

hoverExplain :: J.HoverRequest -> HandlerM ()
hoverExplain request = do
  let uri = request ^. J.params . J.textDocument . J.uri
      (J.Position line col) = request ^. (J.params . J.position)
  txt <- readUri uri
  errs <- lift $ gets (Map.findWithDefault [] uri . view errors)
  let isHovered (Diagnosis _ (Just (Range left right)) _) =
        left <= (line,col) && (line,col) <= right
      isHovered _ = False

      hoverFromDiagnosis (Diagnosis _ (Just (Range left right)) diagnosis) =
        let _range = Just $ J.Range (uncurry J.Position left)
                                    (uncurry J.Position right)
            encodedDiag = URI.encode (Text.unpack diagnosis)
            command = "[Explain error](dhall-explain:?"
                        <> Text.pack encodedDiag <> " )"
            _contents = J.List [J.PlainString command]
        in Just J.Hover { .. }
      hoverFromDiagnosis _ = Nothing

      explanations = mapMaybe (explain txt) errs
      mExplanation = find isHovered explanations
      mHover = do explanation <- mExplanation
                  hoverFromDiagnosis explanation
  lspRespond LSP.RspHover request mHover

hoverType :: J.HoverRequest -> HandlerM ()
hoverType request = do
  let uri = request ^. J.params . J.textDocument . J.uri
      (J.Position line col) = request ^. (J.params . J.position)
  txt <- readUri uri
  expr <- loadFile uri
  case typeAt (line,col) expr of
    Left err -> throwE (Error, Text.pack err)
    Right typ ->
      let _range = fmap (rangeToJSON . sanitiseRange txt . rangeFromDhall)
                        (srcAt (line,col) expr)
          _contents = J.List [J.PlainString (pretty typ)]
          hover = J.Hover{..}
      in lspRespond LSP.RspHover request (Just hover)

hoverHandler :: J.HoverRequest -> HandlerM ()
hoverHandler request = do
  let uri = request ^. J.params . J.textDocument . J.uri
  errorMap <- lift $ gets (view errors)
  case Map.lookup uri errorMap of
    Nothing -> hoverType request
    _ -> hoverExplain request


diagnosticsHandler :: J.Uri -> HandlerM ()
diagnosticsHandler uri = do
  txt <- readUri uri
  fileIdentifier <- fileIdentifierFromUri uri
  -- make sure we don't keep a stale version around
  lift $ modify (over importCache (invalidate fileIdentifier))
  cache <- lift $ gets (view importCache)

  errs <- flip catchE (return . Just) $ do
      expr <- case parse txt of
        Right e -> return e
        Left err -> throwE err
      loaded <- liftIO $ load fileIdentifier expr cache
      (cache', expr') <- case loaded of
        Right x -> return x
        Left err -> throwE err
      welltyped <- case typecheck expr' of
        Right wt -> return wt
        Left err -> throwE err
      let normal = normalize welltyped
      -- cache the new expression
      lift $ modify (set importCache (cacheExpr fileIdentifier normal cache'))
      return Nothing

  let suggestions =
        case parse txt of
          Right expr -> suggest expr
          _ -> []

      suggestionToDiagnostic Suggestion {..} =
        let _range = rangeToJSON range
            _severity = Just J.DsHint
            _source = Just "Dhall.Lint"
            _code = Nothing
            _message = suggestion
            _relatedInformation = Nothing
        in J.Diagnostic {..}

      diagnosisToDiagnostic Diagnosis {..} =
        let _range = case range of
              Just range' ->
                rangeToJSON range'
              Nothing -> J.Range (J.Position 0 0) (J.Position 0 0)
            _severity = Just J.DsError
            _source = Just doctor
            _code = Nothing
            _message = diagnosis
            _relatedInformation = Nothing
        in J.Diagnostic {..}

      diagnostics = concatMap (map diagnosisToDiagnostic . diagnose txt) (maybeToList errs)
                     ++ map suggestionToDiagnostic suggestions

  lspSendNotification LSP.NotPublishDiagnostics J.TextDocumentPublishDiagnostics
                      (J.PublishDiagnosticsParams uri (J.List diagnostics))


documentFormattingHandler :: J.DocumentFormattingRequest -> HandlerM ()
documentFormattingHandler request = do
  let uri = request ^. J.params . J.textDocument . J.uri
  txt <- readUri uri

  (header, expr) <- case parseWithHeader txt of
    Right res -> return res
    _ -> throwE (Warning, "Failed to format dhall code; parse error.")

  let formatted = formatExprWithHeader expr header
      numLines = Text.length txt
      range = J.Range (J.Position 0 0) (J.Position numLines 0)
      edits = J.List [J.TextEdit range formatted]

  lspRespond LSP.RspDocumentFormatting request edits


executeCommandHandler :: J.ExecuteCommandRequest -> HandlerM ()
executeCommandHandler request
  | command == "dhall.server.lint" = executeLintAndFormat request
  | command == "dhall.server.annotateLet" = executeAnnotateLet request
  | otherwise = throwE (Warning, "Command '" <> command
                                   <> "' not known; ignored.")
  where command = request ^. J.params . J.command

getCommandArguments :: J.FromJSON a => J.ExecuteCommandRequest -> HandlerM a
getCommandArguments request = do
  json <- case request ^. J.params . J.arguments of
    Just (J.List (x : _)) -> return x
    _ -> throwE (Error, "Failed to execute command; arguments missing.")
  case J.fromJSON json of
    J.Success args -> return args
    _ -> throwE (Error, "Failed to execute command; failed to parse arguments.")


-- implements dhall.server.lint
executeLintAndFormat :: J.ExecuteCommandRequest -> HandlerM ()
executeLintAndFormat request = do
  uri <- getCommandArguments request
  txt <- readUri uri

  (header, expr) <- case parseWithHeader txt of
    Right res -> return res
    _ -> throwE (Warning, "Failed to lint dhall code; parse error.")

  let linted = formatExprWithHeader (lint expr) header
      numLines = Text.length txt
      range = J.Range (J.Position 0 0) (J.Position numLines 0)
      edit = J.WorkspaceEdit
        (Just (HashMap.singleton uri (J.List [J.TextEdit range linted]))) Nothing

  lspRespond LSP.RspExecuteCommand request J.Null
  lspRequest LSP.ReqApplyWorkspaceEdit J.WorkspaceApplyEdit
    (J.ApplyWorkspaceEditParams edit)


executeAnnotateLet :: J.ExecuteCommandRequest -> HandlerM ()
executeAnnotateLet request = do
  args :: J.TextDocumentPositionParams <- getCommandArguments request
  let uri = args ^. J.textDocument . J.uri
      line = args ^. J.position . J.line
      col = args ^. J.position . J.character

  expr <- loadFile uri
  _welltyped <- case typecheck expr of
    Left _ -> throwE (Warning, "Failed to annotate let binding; not well-typed.")
    Right e -> return e

  (Src (SourcePos _ x1 y1) (SourcePos _ x2 y2) _, txt)
    <- case annotateLet (line, col) expr of
      Right x -> return x
      Left msg -> throwE (Warning, Text.pack msg)

  let range = J.Range (J.Position (unPos x1 - 1) (unPos y1 - 1))
                      (J.Position (unPos x2 - 1) (unPos y2 - 1))
      edit = J.WorkspaceEdit
        (Just (HashMap.singleton uri (J.List [J.TextEdit range txt]))) Nothing

  lspRequest LSP.ReqApplyWorkspaceEdit J.WorkspaceApplyEdit
    (J.ApplyWorkspaceEditParams edit)




-- handler that doesn't do anything. Useful for example to make haskell-lsp shut
-- up about unhandled DidChangeTextDocument notifications (which are already
-- handled haskell-lsp itself).
nullHandler :: a -> HandlerM ()
nullHandler _ = return ()

didOpenTextDocumentNotificationHandler
  :: J.DidOpenTextDocumentNotification -> HandlerM ()
didOpenTextDocumentNotificationHandler notification = do
  let uri = notification ^. J.params . J.textDocument . J.uri
  diagnosticsHandler uri

didSaveTextDocumentNotificationHandler
  :: J.DidSaveTextDocumentNotification -> HandlerM ()
didSaveTextDocumentNotificationHandler notification = do
  let uri = notification ^. J.params . J.textDocument . J.uri
  diagnosticsHandler uri
