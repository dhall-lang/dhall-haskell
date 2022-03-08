{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP            #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf     #-}

module Dhall.LSP.Handlers where

import Data.Void    (Void)
import Dhall.Core
    ( Expr (Embed, Note)
    , Import (..)
    , ImportHashed (..)
    , ImportType (..)
    , headers
    , pretty
    )
import Dhall.Import (localToPath)
import Dhall.Parser (Src (..))

import Dhall.LSP.Backend.Completion
    ( Completion (..)
    , buildCompletionContext
    , completeEnvironmentImport
    , completeFromContext
    , completeLocalImport
    , completeProjections
    , completionQueryAt
    )
import Dhall.LSP.Backend.Dhall
    ( FileIdentifier
    , fileIdentifierFromFilePath
    , fileIdentifierFromURI
    , invalidate
    , load
    , parse
    , parseWithHeader
    , typecheck
    )
import Dhall.LSP.Backend.Diagnostics
    ( Diagnosis (..)
    , Range (..)
    , diagnose
    , embedsWithRanges
    , explain
    , rangeFromDhall
    )
import Dhall.LSP.Backend.Formatting  (formatExpr, formatExprWithHeader)
import Dhall.LSP.Backend.Freezing
    ( computeSemanticHash
    , getAllImportsWithHashPositions
    , getImportHashPosition
    , stripHash
    )
import Dhall.LSP.Backend.Linting     (Suggestion (..), lint, suggest)
import Dhall.LSP.Backend.Parsing     (binderExprFromText)
import Dhall.LSP.Backend.Typing      (annotateLet, exprAt, typeAt)
import Dhall.LSP.State

import Control.Applicative              ((<|>))
import Control.Lens                     (assign, modifying, use, (^.))
import Control.Monad                    (forM, guard)
import Control.Monad.Trans              (lift, liftIO)
import Control.Monad.Trans.Except       (catchE, throwE)
import Data.Aeson                       (FromJSON(..), Value(..))
import Data.Maybe                       (maybeToList)
import Data.Text                        (Text, isPrefixOf)
import Language.LSP.Server              (Handlers, LspT)
import Language.LSP.Types               hiding (Range(..), line)
import Language.LSP.Types.Lens
import System.FilePath
import Text.Megaparsec                  (SourcePos (..), unPos)

import qualified Data.Aeson              as Aeson
import qualified Data.HashMap.Strict     as HashMap
import qualified Data.Map.Strict         as Map
import qualified Data.Rope.UTF16         as Rope
import qualified Data.Text               as Text
import qualified Language.LSP.Server     as LSP
import qualified Language.LSP.Types      as LSP.Types
import qualified Language.LSP.VFS        as LSP
import qualified Network.URI             as URI
import qualified Network.URI.Encode      as URI

liftLSP :: LspT ServerConfig IO a -> HandlerM a
liftLSP m = lift (lift m)

-- | A helper function to query haskell-lsp's VFS.
readUri :: Uri -> HandlerM Text
readUri uri_ = do
  mVirtualFile <- liftLSP (LSP.getVirtualFile (LSP.Types.toNormalizedUri uri_))
  case mVirtualFile of
    Just (LSP.VirtualFile _ _ rope) -> return (Rope.toText rope)
    Nothing -> throwE (Error, "Could not find " <> Text.pack (show uri_) <> " in VFS.")

loadFile :: Uri -> HandlerM (Expr Src Void)
loadFile uri_ = do
  txt <- readUri uri_
  fileIdentifier <- fileIdentifierFromUri uri_
  cache <- use importCache

  expr <- case parse txt of
    Right e -> return e
    _ -> throwE (Error, "Failed to parse Dhall file.")

  loaded <- liftIO $ load fileIdentifier expr cache
  (cache', expr') <- case loaded of
    Right x -> return x
    _ -> throwE (Error, "Failed to resolve imports.")
  -- Update cache. Don't cache current expression because it might not have been
  -- written to disk yet (readUri reads from the VFS).
  assign importCache cache'
  return expr'

-- helper
fileIdentifierFromUri :: Uri -> HandlerM FileIdentifier
fileIdentifierFromUri uri_ =
  let mFileIdentifier = fmap fileIdentifierFromFilePath (uriToFilePath uri_)
                        <|> (do uri' <- (URI.parseURI . Text.unpack . getUri) uri_
                                fileIdentifierFromURI uri')
  in case mFileIdentifier of
    Just fileIdentifier -> return fileIdentifier
    Nothing -> throwE (Error, getUri uri_ <> " is not a valid name for a dhall file.")

-- helper
rangeToJSON :: Range -> LSP.Types.Range
rangeToJSON (Range (x1,y1) (x2,y2)) =
    LSP.Types.Range (Position x1 y1) (Position x2 y2)

hoverHandler :: Handlers HandlerM
hoverHandler =
    LSP.requestHandler STextDocumentHover \request respond -> do
        let uri_ = request^.params.textDocument.uri

        let Position{ _line, _character } = request^.params.position

        errorMap <- use errors

        case Map.lookup uri_ errorMap of
            Nothing -> do
                expr <- loadFile uri_
                (welltyped, _) <- case typecheck expr of
                    Left  _  -> throwE (Info, "Can't infer type; code does not type-check.")
                    Right wt -> return wt
                case typeAt (_line, _character) welltyped of
                    Left err -> throwE (Error, Text.pack err)
                    Right (mSrc, typ) -> do
                        let _range = fmap (rangeToJSON . rangeFromDhall) mSrc

                        let _contents = HoverContents (MarkupContent MkPlainText (pretty typ))
                        respond (Right (Just Hover{ _contents, _range }))
            Just err -> do
                let isHovered (Diagnosis _ (Just (Range left right)) _) =
                        left <= (_line, _character) && (_line, _character) <= right
                    isHovered _ =
                        False

                let hoverFromDiagnosis (Diagnosis _ (Just (Range left right)) diagnosis) = do
                        let _range = Just (rangeToJSON (Range left right))
                            encodedDiag = URI.encode (Text.unpack diagnosis)

                            _kind = MkMarkdown

                            _value =
                                    "[Explain error](dhall-explain:?"
                                <>  Text.pack encodedDiag
                                <>  " )"

                            _contents = HoverContents MarkupContent{..}
                        Just Hover{ _contents, _range }
                    hoverFromDiagnosis _ =
                        Nothing

                let mHover = do
                        explanation <- explain err

                        guard (isHovered explanation)

                        hoverFromDiagnosis explanation

                respond (Right mHover)

documentLinkHandler :: Handlers HandlerM
documentLinkHandler =
    LSP.requestHandler STextDocumentDocumentLink \request respond -> do
        let uri_ = request^.params.textDocument.uri

        path <- case uriToFilePath uri_ of
            Nothing ->
                throwE (Log, "Could not process document links; failed to convert URI to file path.")
            Just p ->
                return p

        txt <- readUri uri_

        expr <- case parse txt of
            Right e ->
                return e
            Left _ ->
                throwE (Log, "Could not process document links; did not parse.")

        let imports = embedsWithRanges expr :: [(Range, Import)]

        let basePath = takeDirectory path

        let go :: (Range, Import) -> IO [DocumentLink]
            go (range_, Import (ImportHashed _ (Local prefix file)) _) = do
              filePath <- localToPath prefix file
              let filePath' = basePath </> filePath  -- absolute file path
              let _range = rangeToJSON range_
              let _target = Just (filePathToUri filePath')
              let _tooltip = Nothing
              let _xdata = Nothing
              return [DocumentLink {..}]

            go (range_, Import (ImportHashed _ (Remote url)) _) = do
              let _range = rangeToJSON range_
              let url' = url { headers = Nothing }
              let _target = Just (Uri (pretty url'))
              let _tooltip = Nothing
              let _xdata = Nothing
              return [DocumentLink {..}]

            go _ = return []

        links <- liftIO $ mapM go imports
        respond (Right (List (concat links)))


diagnosticsHandler :: Uri -> HandlerM ()
diagnosticsHandler _uri = do
  txt <- readUri _uri
  fileIdentifier <- fileIdentifierFromUri _uri
  -- make sure we don't keep a stale version around
  modifying importCache (invalidate fileIdentifier)
  cache <- use importCache

  errs <- flip catchE (return . Just) $ do
      expr <- case parse txt of
        Right e -> return e
        Left err -> throwE err
      loaded <- liftIO $ load fileIdentifier expr cache
      (cache', expr') <- case loaded of
        Right x -> return x
        Left err -> throwE err
      _ <- case typecheck expr' of
        Right (wt, _typ) -> return wt
        Left err -> throwE err
      assign importCache cache'
      return Nothing

  let suggestions =
        case parse txt of
          Right expr -> suggest expr
          _ -> []

      suggestionToDiagnostic Suggestion { range = range_, .. } =
        let _range = rangeToJSON range_
            _severity = Just DsHint
            _source = Just "Dhall.Lint"
            _code = Nothing
            _message = suggestion
            _tags = Nothing
            _relatedInformation = Nothing
        in Diagnostic {..}

      diagnosisToDiagnostic Diagnosis { range = range_, .. } =
        let _range = case range_ of
              Just range' -> rangeToJSON range'
              Nothing     -> LSP.Types.Range (Position 0 0) (Position 0 0)
            _severity = Just DsError
            _source = Just doctor
            _code = Nothing
            _tags = Nothing
            _message = diagnosis
            _relatedInformation = Nothing
        in Diagnostic {..}

  modifying errors (Map.alter (const errs) _uri)  -- cache errors

  let _version = Nothing
  let _diagnostics =
          List
              (   concatMap (map diagnosisToDiagnostic . diagnose) (maybeToList errs)
              ++  map suggestionToDiagnostic suggestions
              )


  liftLSP (LSP.sendNotification STextDocumentPublishDiagnostics PublishDiagnosticsParams{ _uri, _version, _diagnostics })

documentFormattingHandler :: Handlers HandlerM
documentFormattingHandler =
    LSP.requestHandler STextDocumentFormatting \request respond -> do
        let _uri = request^.params.textDocument.uri

        txt <- readUri _uri

        (header, expr) <- case parseWithHeader txt of
          Right res -> return res
          _ -> throwE (Warning, "Failed to format dhall code; parse error.")

        ServerConfig{..} <- liftLSP LSP.getConfig

        let numLines = Text.length txt
        let _newText= formatExprWithHeader chosenCharacterSet expr header
        let _range = LSP.Types.Range (Position 0 0) (Position numLines 0)

        respond (Right (List [TextEdit{..}]))

executeCommandHandler :: Handlers HandlerM
executeCommandHandler =
    LSP.requestHandler SWorkspaceExecuteCommand \request respond -> do
        let command_ = request^.params.command
        if  | command_ == "dhall.server.lint" ->
                executeLintAndFormat request respond
            | command_ == "dhall.server.annotateLet" ->
                executeAnnotateLet request
            | command_ == "dhall.server.freezeImport" ->
                executeFreezeImport request
            | command_ == "dhall.server.freezeAllImports" ->
                executeFreezeAllImports request
            | otherwise -> do
                throwE
                    ( Warning
                    , "Command '" <> command_ <> "' not known; ignored."
                    )

getCommandArguments
    :: FromJSON a => RequestMessage 'WorkspaceExecuteCommand -> HandlerM a
-- (HasParams s a, FromJSON a) => s -> HandlerM a
getCommandArguments request = do
  json <- case request ^. params . arguments of
    Just (List (x : _)) -> return x
    _ -> throwE (Error, "Failed to execute command; arguments missing.")
  case Aeson.fromJSON json of
    Aeson.Success args ->
        return args
    _ ->
        throwE (Error, "Failed to execute command; failed to parse arguments.")

-- implements dhall.server.lint
executeLintAndFormat
    :: RequestMessage 'WorkspaceExecuteCommand
    -> (Either a Value -> HandlerM b)
    -> HandlerM ()
executeLintAndFormat request respond = do
  uri_ <- getCommandArguments request
  txt <- readUri uri_

  (header, expr) <- case parseWithHeader txt of
    Right res -> return res
    _ -> throwE (Warning, "Failed to lint dhall code; parse error.")

  ServerConfig{..} <- liftLSP LSP.getConfig

  let numLines = Text.length txt

  let _newText = formatExprWithHeader chosenCharacterSet (lint expr) header

  let _range = LSP.Types.Range (Position 0 0) (Position numLines 0)

  let _edit =
          WorkspaceEdit
              { _changes = Just (HashMap.singleton uri_ (List [TextEdit{..}]))
              , _documentChanges = Nothing
              , _changeAnnotations = Nothing
              }

  let _label = Nothing

  _ <- respond (Right Aeson.Null)

  _ <- liftLSP (LSP.sendRequest SWorkspaceApplyEdit ApplyWorkspaceEditParams{ _label, _edit } nullHandler)

  return ()

executeAnnotateLet
    :: RequestMessage 'WorkspaceExecuteCommand
    -> HandlerM ()
executeAnnotateLet request = do
  args <- getCommandArguments request :: HandlerM TextDocumentPositionParams
  let uri_ = args ^. textDocument . uri
      line_ = args ^. position . line
      col_ = args ^. position . character

  expr <- loadFile uri_
  (welltyped, _) <- case typecheck expr of
    Left _ -> throwE (Warning, "Failed to annotate let binding; not well-typed.")
    Right e -> return e

  ServerConfig{..} <- liftLSP LSP.getConfig

  (Src (SourcePos _ x1 y1) (SourcePos _ x2 y2) _, annotExpr)
    <- case annotateLet (line_, col_) welltyped of
      Right x -> return x
      Left msg -> throwE (Warning, Text.pack msg)

  let _range = LSP.Types.Range (Position (unPos x1 - 1) (unPos y1 - 1))
                      (Position (unPos x2 - 1) (unPos y2 - 1))

  let _newText= formatExpr chosenCharacterSet annotExpr

  let _edit = WorkspaceEdit
          { _changes = Just (HashMap.singleton uri_ (List [TextEdit{..}]))
          , _documentChanges = Nothing
          , _changeAnnotations = Nothing
          }

  let _label = Nothing

  _ <- liftLSP (LSP.sendRequest SWorkspaceApplyEdit ApplyWorkspaceEditParams{ _label, _edit } nullHandler)

  return ()

executeFreezeAllImports
    :: RequestMessage 'WorkspaceExecuteCommand
    -> HandlerM ()
executeFreezeAllImports request = do
  uri_ <- getCommandArguments request

  fileIdentifier <- fileIdentifierFromUri uri_
  txt <- readUri uri_
  expr <- case parse txt of
    Right e -> return e
    Left _ -> throwE (Warning, "Could not freeze imports; did not parse.")

  let importRanges = getAllImportsWithHashPositions expr
  edits_ <- forM importRanges $ \(import_, Range (x1, y1) (x2, y2)) -> do
    cache <- use importCache
    let importExpr = Embed (stripHash import_)

    hashResult <- liftIO $ computeSemanticHash fileIdentifier importExpr cache
    (cache', hash) <- case hashResult of
      Right (c, t) -> return (c, t)
      Left _ -> throwE (Error, "Could not freeze import; failed to evaluate import.")
    assign importCache cache'

    let _range = LSP.Types.Range (Position x1 y1) (Position x2 y2)
    let _newText = " " <> hash
    return TextEdit{..}

  let _edit = WorkspaceEdit
          { _changes = Just (HashMap.singleton uri_ (List edits_))
          , _documentChanges = Nothing
          , _changeAnnotations = Nothing
          }

  let _label = Nothing

  _ <- liftLSP (LSP.sendRequest SWorkspaceApplyEdit ApplyWorkspaceEditParams{ _edit, _label } nullHandler)

  return ()

executeFreezeImport
    :: RequestMessage 'WorkspaceExecuteCommand
    -> HandlerM ()
executeFreezeImport request = do
  args <- getCommandArguments request :: HandlerM TextDocumentPositionParams
  let uri_  = args ^. textDocument . uri
  let line_ = args ^. position . line
  let col_  = args ^. position . character

  txt <- readUri uri_
  expr <- case parse txt of
    Right e -> return e
    Left _ -> throwE (Warning, "Could not freeze import; did not parse.")

  (src, import_)
    <- case exprAt (line_, col_) expr of
      Just (Note src (Embed i)) -> return (src, i)
      _ -> throwE (Warning, "You weren't pointing at an import!")

  Range (x1, y1) (x2, y2) <- case getImportHashPosition src of
      Just range_ -> return range_
      Nothing -> throwE (Error, "Failed to re-parse import!")

  fileIdentifier <- fileIdentifierFromUri uri_
  cache <- use importCache
  let importExpr = Embed (stripHash import_)

  hashResult <- liftIO $ computeSemanticHash fileIdentifier importExpr cache
  (cache', hash) <- case hashResult of
    Right (c, t) -> return (c, t)
    Left _ -> throwE (Error, "Could not freeze import; failed to evaluate import.")
  assign importCache cache'

  let _range = LSP.Types.Range (Position x1 y1) (Position x2 y2)
  let _newText = " " <> hash

  let _edit = WorkspaceEdit
          { _changes = Just (HashMap.singleton uri_ (List [TextEdit{..}]))
          , _documentChanges = Nothing
          , _changeAnnotations = Nothing
          }

  let _label = Nothing

  _ <- liftLSP (LSP.sendRequest SWorkspaceApplyEdit ApplyWorkspaceEditParams{ _edit, _label } nullHandler)

  return ()

completionHandler :: Handlers HandlerM
completionHandler =
  LSP.requestHandler STextDocumentCompletion \request respond -> do
    let uri_ = request ^. params . textDocument . uri
        line_ = request ^. params . position . line
        col_ = request ^. params . position . character

    txt <- readUri uri_
    let (completionLeadup, completionPrefix) = completionQueryAt txt (line_, col_)

    let computeCompletions
          -- environment variable
          | "env:" `isPrefixOf` completionPrefix =
            liftIO completeEnvironmentImport

          -- local import
          | any (`isPrefixOf` completionPrefix) [ "/", "./", "../", "~/" ] = do
            let relativeTo | Just path <- uriToFilePath uri_ = path
                         | otherwise = "."
            liftIO $ completeLocalImport relativeTo (Text.unpack completionPrefix)

          -- record projection / union constructor
          | (target_, _) <- Text.breakOnEnd "." completionPrefix
          , not (Text.null target_) = do
            let bindersExpr = binderExprFromText completionLeadup

            fileIdentifier <- fileIdentifierFromUri uri_
            cache <- use importCache
            loadedBinders <- liftIO $ load fileIdentifier bindersExpr cache

            (cache', bindersExpr') <-
              case loadedBinders of
                Right (cache', binders) ->
                  return (cache', binders)
                Left _ -> throwE (Log, "Could not complete projection; failed to load binders expression.")

            let completionContext = buildCompletionContext bindersExpr'

            targetExpr <- case parse (Text.dropEnd 1 target_) of
              Right e -> return e
              Left _ -> throwE (Log, "Could not complete projection; prefix did not parse.")

            loaded' <- liftIO $ load fileIdentifier targetExpr cache'
            case loaded' of
              Right (cache'', targetExpr') -> do
                assign importCache cache''
                return (completeProjections completionContext targetExpr')
              Left _ -> return []

          -- complete identifiers in scope
          | otherwise = do
            let bindersExpr = binderExprFromText completionLeadup

            fileIdentifier <- fileIdentifierFromUri uri_
            cache <- use importCache  -- todo save cache afterwards
            loadedBinders <- liftIO $ load fileIdentifier bindersExpr cache

            bindersExpr' <-
              case loadedBinders of
                Right (cache', binders) -> do
                  assign importCache cache'
                  return binders
                Left _ -> throwE (Log, "Could not complete projection; failed to load binders expression.")

            let context_ = buildCompletionContext bindersExpr'

            return (completeFromContext context_)

    completions <- computeCompletions

    let toCompletionItem (Completion {..}) = CompletionItem {..}
         where
          _label = completeText
          _kind = Nothing
          _tags = mempty
          _detail = fmap pretty completeType
          _documentation = Nothing
          _deprecated = Nothing
          _preselect = Nothing
          _sortText = Nothing
          _filterText = Nothing
          _insertText = Nothing
          _insertTextFormat = Nothing
          _insertTextMode = Nothing
          _textEdit = Nothing
          _additionalTextEdits = Nothing
          _commitCharacters = Nothing
          _command = Nothing
          _xdata = Nothing

    let _items = List (map toCompletionItem completions)

    let _isIncomplete = False

    respond (Right (InR CompletionList{..}))

nullHandler :: a -> LspT ServerConfig IO ()
nullHandler _ = return ()

didOpenTextDocumentNotificationHandler :: Handlers HandlerM
didOpenTextDocumentNotificationHandler =
    LSP.notificationHandler STextDocumentDidOpen \notification -> do
        let _uri = notification^.params.textDocument.uri
        diagnosticsHandler _uri

didSaveTextDocumentNotificationHandler :: Handlers HandlerM
didSaveTextDocumentNotificationHandler =
    LSP.notificationHandler STextDocumentDidSave \notification -> do
        let _uri = notification^.params.textDocument.uri
        diagnosticsHandler _uri
