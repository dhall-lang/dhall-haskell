{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP            #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE MultiWayIf     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE ViewPatterns   #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Dhall.LSP.Handlers where

import Data.Void    (Void)
import Dhall        (EvaluateSettings)
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
    , loadCollected
    , parse
    , parseWithHeader
    , typecheck
    , DhallError (..)
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
import Dhall.LSP.Backend.Parsing     (binderExprFromText, namesBeingDefined)
import Dhall.LSP.Backend.Typing      (annotateLet, exprAt, typeAt)
import Dhall.LSP.State

import Control.Applicative           ((<|>))
import Control.Lens                  (assign, modifying, toListOf, use, (^.))
import Control.Monad                 (forM, forM_, guard)
import Control.Monad.Trans           (lift, liftIO)
import Control.Concurrent            (forkIO, threadDelay)
import Control.Monad                 (foldM, void, when)
import Control.Monad.Trans.Except    (catchE, throwE)
import Control.Monad.Trans.State.Strict (get)
import Data.IORef                    (IORef)
import Data.Int                      (Int64)
import Data.List.NonEmpty            (NonEmpty (..))
import Data.Maybe                    (fromMaybe, isNothing, listToMaybe)
import Data.Aeson                    (FromJSON (..), Value (..))
import Data.Maybe                    (maybeToList)
import Data.Text                     (Text, isPrefixOf)
import Language.LSP.Protocol.Lens
    ( arguments
    , character
    , command
    , line
    , params
    , position
    , textDocument
    , uri
    )
import Language.LSP.Protocol.Message
    ( Method (..)
    , SMethod (..)
    , TRequestMessage
    )
import Language.LSP.Protocol.Types   hiding (Range (..))
import Language.LSP.Server           (Handlers, LspT)
import System.Directory              (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath               (takeDirectory, (<.>), (</>))
import System.IO                     (hPutStrLn, stderr)
import Text.Megaparsec               (SourcePos (..), unPos)

import qualified Control.Monad.Trans.Except       as Except
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Aeson                       as Aeson
import qualified Data.IORef                       as IORef
import qualified Data.Map.Strict                  as Map
import qualified Dhall.Bounded                   as Bounded
import qualified Dhall.Core                       as Core
import qualified Dhall.Import                     as Import
import qualified Dhall.Map                        as Dhall.Map
import qualified Dhall.Pretty                     as Pretty
import qualified Dhall.TypeCheck                  as TypeCheck
import qualified Data.Text                   as Text
import qualified Language.LSP.Protocol.Types as LSP.Types
import qualified Language.LSP.Server         as LSP
import qualified Language.LSP.VFS            as LSP
import qualified Network.URI                 as URI
import qualified Network.URI.Encode          as URI

#if MIN_VERSION_lsp(2,4,0)
import qualified Data.Text.Utf16.Rope.Mixed as Rope
#else
import qualified Data.Text.Utf16.Rope as Rope
#endif

liftLSP :: LspT ServerConfig IO a -> HandlerM a
liftLSP m = lift (lift m)

-- | A helper function to query haskell-lsp's VFS.
tryReadUri :: Uri -> HandlerM (Maybe Text)
tryReadUri uri_ = do
  mVirtualFile <- liftLSP (LSP.getVirtualFile (LSP.Types.toNormalizedUri uri_))
  return $ case mVirtualFile of
#if MIN_VERSION_lsp(2,8,0)
    Just (LSP.VirtualFile _ _ rope _) -> Just (Rope.toText rope)
#else
    Just (LSP.VirtualFile _ _ rope) -> Just (Rope.toText rope)
#endif
    Nothing -> Nothing

-- | Like `tryReadUri`, but fails if the URI was not found.
readUri :: Uri -> HandlerM Text
readUri uri_ = do
  mText <- tryReadUri uri_
  case mText of
    Just text -> return text
    Nothing ->
      throwE (Error, "Could not find " <> Text.pack (show uri_) <> " in VFS.")

loadFile :: EvaluateSettings -> Uri -> HandlerM (Expr Src Void)
loadFile settings uri_ = do
  txt <- readUri uri_
  fileIdentifier <- fileIdentifierFromUri uri_
  cache <- use importCache

  expr <- case parse txt of
    Right e -> return e
    _ -> throwE (Error, "Failed to parse Dhall file.")

  loaded <- liftIO $ load settings fileIdentifier expr cache
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
    LSP.Types.Range
      (Position (fromIntegral x1) (fromIntegral y1))
      (Position (fromIntegral x2) (fromIntegral y2))

hoverHandler :: EvaluateSettings -> Handlers HandlerM
hoverHandler settings =
    LSP.requestHandler SMethod_TextDocumentHover \request respond -> handleErrorWithDefault respond (InR LSP.Types.Null) do
        let uri_ = request^.params.textDocument.uri

        let Position{ _line = fromIntegral -> _line, _character = fromIntegral -> _character } = request^.params.position

        errorMap <- use errors

        case Map.lookup uri_ errorMap of
            Nothing -> do
                expr <- loadFile settings uri_
                (welltyped, _) <- case typecheck settings expr of
                    Left  _  -> throwE (Info, "Can't infer type; code does not type-check.")
                    Right wt -> return wt
                case typeAt (_line, _character) welltyped of
                    Left err -> throwE (Error, Text.pack err)
                    Right (mSrc, typ) -> do
                        let _range = fmap (rangeToJSON . rangeFromDhall) mSrc
                        ServerConfig { maxOutputSize } <- liftLSP LSP.getConfig
                        let rendered =
                                case Bounded.prettyBounded
                                        maxOutputSize
                                        (Pretty.prettyCharacterSet Pretty.Unicode typ) of
                                    Bounded.Complete text -> text
                                    Bounded.Truncated text -> text
                        let _contents = InL (mkPlainText rendered)
                        respond (Right (InL Hover{ _contents, _range }))
            Just err -> do
                let isHovered (Diagnosis _ (Just (Range left right)) _) =
                        left <= (_line, _character) && (_line, _character) <= right
                    isHovered _ =
                        False

                let hoverFromDiagnosis (Diagnosis _ (Just (Range left right)) diagnosis) = do
                        let _range = Just (rangeToJSON (Range left right))
                            encodedDiag = URI.encode (Text.unpack diagnosis)

                            _kind = MarkupKind_Markdown

                            _value =
                                    "[Explain error](dhall-explain:?"
                                <>  Text.pack encodedDiag
                                <>  " )"

                            _contents = InL MarkupContent{..}
                        Just Hover{ _contents, _range }
                    hoverFromDiagnosis _ =
                        Nothing

                let mHover = do
                        explanation <- explain err

                        guard (isHovered explanation)

                        hoverFromDiagnosis explanation

                respond (Right (maybeToNull mHover))

documentLinkHandler :: Handlers HandlerM
documentLinkHandler =
    LSP.requestHandler SMethod_TextDocumentDocumentLink \request respond -> handleErrorWithDefault respond (InL []) do
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
#if MIN_VERSION_lsp(2,5,0)
              let _target = Just (filePathToUri filePath')
#else
              let _target = Just (getUri (filePathToUri filePath'))
#endif
              let _tooltip = Nothing
              let _data_ = Nothing
              return [DocumentLink {..}]

            go (range_, Import (ImportHashed _ (Remote url)) _) = do
              let _range = rangeToJSON range_
              let url' = url { headers = Nothing }
#if MIN_VERSION_lsp(2,5,0)
              let _target = Just (Uri (pretty url'))
#else
              let _target = Just (pretty url')
#endif
              let _tooltip = Nothing
              let _data_ = Nothing
              return [DocumentLink {..}]

            go _ = return []

        links <- liftIO $ mapM go imports
        respond (Right (InL (concat links)))


diagnosticsHandler :: EvaluateSettings -> Uri -> HandlerM ()
diagnosticsHandler settings _uri = do
  mTxt <- tryReadUri _uri
  case mTxt of
    Just txt -> diagnoseDocument settings _uri txt
    Nothing ->
      liftIO $ hPutStrLn stderr
        (  "Warning: received textDocument/didSave for URI not present in VFS (missing prior didOpen?): "
        <> Text.unpack (getUri _uri)
        )

collectedDiagnostic :: Uri -> Import.CollectedImportError -> Diagnostic
collectedDiagnostic docUri err =
    let _range = rangeToJSON (rangeFromDhall (Import.collectedSrc err))
        _severity = Just DiagnosticSeverity_Error
        _source = Just "Dhall.Import"
        _code = Nothing
        _codeDescription = Nothing
        _tags = Nothing
        _message =
            Text.pack
                (concatMap
                    Import.plainShowImportError
                    (Import.collectedErrors err))
        _relatedInformation = importRelated docUri err
        _data_ = Nothing
    in Diagnostic {..}

-- | Point at the rest of the import stack when a failure is nested.
importRelated :: Uri -> Import.CollectedImportError -> Maybe [DiagnosticRelatedInformation]
importRelated docUri err =
    case Import.collectedStack err of
        _ :| [] ->
            Nothing
        _ :| nested ->
            Just
                [ DiagnosticRelatedInformation
                    { _location =
                        Location
                            { _uri = docUri
                            , _range = rangeToJSON (rangeFromDhall (Import.collectedSrc err))
                            }
                    , _message =
                        "Inside imported file: "
                            <> Text.intercalate " <- " (map chainText nested)
                    }
                ]

chainText :: Import.Chained -> Text
chainText chained = pretty (Import.chainedImport chained)

-- | Type-check top-level lets one at a time.
--
--   A prefix of bindings whose denoted values match the previous analysis
--   reuses that typing context.  Each binding that fails contributes its own
--   error.  Placeholders with a known type are bound first.  If any failure
--   has no known type, type-checking is skipped: those dependents are not
--   reported.
typecheckCollected
    :: [Import.CollectedImportError]
    -> Expr Src Void
    -> [Core.Expr Void Void]
    -> [TypeCheck.TypingContext Src]
    -> ([DhallError], [Core.Expr Void Void], [TypeCheck.TypingContext Src])
typecheckCollected collected expr prevValues prevCtxs
    | any (\e -> isNothing (Import.collectedKnownType e)) collected =
        ([], [], [])
    | otherwise =
        case foldM step TypeCheck.emptyTypingContext collected of
            Left err ->
                ([ErrorTypecheck err], [], [])
            Right ctx ->
                let (errs, values, ctxs) = checkLets prevValues prevCtxs ctx expr
                in (map ErrorTypecheck errs, values, ctxs)
  where
    step ctx err =
        case Import.collectedKnownType err of
            Nothing ->
                Right ctx
            Just known ->
                TypeCheck.extendBinder
                    (Import.collectedName err)
                    (knownImportType known)
                    ctx

    knownImportType Import.KnownText = Core.Text
    knownImportType Import.KnownBytes = Core.Bytes
    knownImportType Import.KnownLocation =
        Core.Union
            (Dhall.Map.fromList
                [ ("Environment", Just Core.Text)
                , ("Remote", Just Core.Text)
                , ("Local", Just Core.Text)
                , ("Missing", Nothing)
                ])

checkLets
    :: [Core.Expr Void Void]
    -> [TypeCheck.TypingContext Src]
    -> TypeCheck.TypingContext Src
    -> Expr Src Void
    -> ([TypeCheck.TypeError Src Void], [Core.Expr Void Void], [TypeCheck.TypingContext Src])
checkLets prevValues prevCtxs ctx0 expr =
    let (binds, rest) = topLets expr
        step (i, accCtx, accVals, accCtxs, accErrs, accFailed) (name, ann, value)
            | i < length prevValues
            , i < length prevCtxs
            , (Core.denote value :: Core.Expr Void Void) == prevValues !! i =
                ( i + 1
                , prevCtxs !! i
                , prevValues !! i : accVals
                , prevCtxs !! i : accCtxs
                , accErrs
                , accFailed
                )
            | otherwise =
                case TypeCheck.extendLet name value accCtx of
                    Right ctx' ->
                        ( i + 1
                        , ctx'
                        , (Core.denote value :: Core.Expr Void Void) : accVals
                        , ctx' : accCtxs
                        , accErrs
                        , accFailed
                        )
                    Left err ->
                        -- Keep the name in scope when its annotation is a
                        -- type, so uses are not reported as unbound.
                        let ctx' = case ann of
                                Just (_, typ) ->
                                    case TypeCheck.extendBinder name typ accCtx of
                                        Right ctx'' -> ctx''
                                        Left _ -> accCtx
                                Nothing ->
                                    accCtx
                        in (i + 1, ctx', accVals, accCtxs, err : accErrs, True)
        (_, ctx, vals, ctxs, errs, failed) =
            foldl step (0, ctx0, [], [], [], False) binds
        errs' =
            if failed
                then errs
                else case TypeCheck.typeWithContext ctx rest of
                    Left err -> err : errs
                    Right _ -> errs
    in (reverse errs', reverse vals, reverse ctxs)

topLets
    :: Expr Src Void
    -> ([(Text, Maybe (Maybe Src, Expr Src Void), Expr Src Void)], Expr Src Void)
topLets (Note _ expr) = topLets expr
topLets (Core.Let Core.Binding { Core.variable = name, Core.annotation = ann, Core.value = value } expr) =
    let (binds, rest) = topLets expr
    in ((name, ann, value) : binds, rest)
topLets expr = ([], expr)

diagnoseDocument :: EvaluateSettings -> Uri -> Text -> HandlerM ()
diagnoseDocument settings _uri txt = do
  fileIdentifier <- fileIdentifierFromUri _uri
  -- make sure we don't keep a stale version around
  modifying importCache (invalidate fileIdentifier)
  cache <- use importCache

  (importDiagnostics, typeErrors) <- case parse txt of
      Left err ->
          return ([], [err])
      Right parsed -> do
          negative <- use negativeImports
          (cache', resolved, collected, _) <-
              liftIO $ loadCollected settings fileIdentifier parsed cache negative
          assign importCache cache'
          let importDiags = map (collectedDiagnostic _uri) collected
          docs <- use documents
          previousSnap <- liftIO $ Map.lookup _uri <$> IORef.readIORef docs
          let prevValues = maybe [] snapPrefixValues previousSnap
              prevCtxs = maybe [] snapPrefixContexts previousSnap
              (typeErrs, prefixValues, prefixCtxs) =
                  typecheckCollected collected resolved prevValues prevCtxs
          liftIO $ IORef.modifyIORef' docs $ \m ->
              let previous = Map.lookup _uri m
                  snap = DocSnap
                      { snapVersion = maybe 0 snapVersion previous
                      , snapGeneration = maybe 0 snapGeneration previous
                      , snapText = txt
                      , snapLastGood = Just txt
                      , snapPrefixValues = prefixValues
                      , snapPrefixContexts = prefixCtxs
                      }
              in Map.insert _uri snap m
          return (importDiags, typeErrs)

  let suggestions =
        case parse txt of
          Right expr -> suggest expr
          _ -> []

      suggestionToDiagnostic Suggestion { range = range_, .. } =
        let _range = rangeToJSON range_
            _severity = Just DiagnosticSeverity_Hint
            _source = Just "Dhall.Lint"
            _code = Nothing
            _codeDescription = Nothing
            _message = suggestion
            _tags =
                if "Unused let binding" `Text.isPrefixOf` suggestion
                    then Just [DiagnosticTag_Unnecessary]
                    else Nothing
            _relatedInformation = Nothing
            _data_ = Nothing
        in Diagnostic {..}

      diagnosisToDiagnostic Diagnosis { range = range_, .. } =
        let _range = case range_ of
              Just range' -> rangeToJSON range'
              Nothing     -> LSP.Types.Range (Position 0 0) (Position 0 0)
            _severity = Just DiagnosticSeverity_Error
            _source = Just doctor
            _code = Nothing
            _codeDescription = Nothing
            _tags = Nothing
            _message = diagnosis
            _relatedInformation = Nothing
            _data_ = Nothing
        in Diagnostic {..}

  modifying errors (Map.alter (const (listToMaybe typeErrors)) _uri)

  let _version = Nothing
  let _diagnostics =
              importDiagnostics
              ++ concatMap (map diagnosisToDiagnostic . diagnose) typeErrors
              ++ map suggestionToDiagnostic suggestions


  liftLSP (LSP.sendNotification SMethod_TextDocumentPublishDiagnostics PublishDiagnosticsParams{ _uri, _version, _diagnostics })

documentFormattingHandler :: Handlers HandlerM
documentFormattingHandler =
    LSP.requestHandler SMethod_TextDocumentFormatting \request respond -> handleErrorWithDefault respond (InL []) do
        let _uri = request^.params.textDocument.uri

        txt <- readUri _uri

        (header, expr) <- case parseWithHeader txt of
          Right res -> return res
          _ -> throwE (Warning, "Failed to format dhall code; parse error.")

        ServerConfig{..} <- liftLSP LSP.getConfig

        let numLines = fromIntegral (Text.length txt)
        let _newText= formatExprWithHeader chosenCharacterSet expr header
        let _range = LSP.Types.Range (Position 0 0) (Position numLines 0)

        respond (Right (InL [TextEdit{..}]))


executeCommandHandler :: EvaluateSettings -> Handlers HandlerM
executeCommandHandler settings =
    LSP.requestHandler SMethod_WorkspaceExecuteCommand \request respond -> handleErrorWithDefault respond (InL Aeson.Null) do
        let command_ = request^.params.command
        if  | command_ == "dhall.server.lint" ->
                executeLintAndFormat request respond
            | command_ == "dhall.server.annotateLet" ->
                executeAnnotateLet settings request
            | command_ == "dhall.server.freezeImport" ->
                executeFreezeImport settings request
            | command_ == "dhall.server.freezeAllImports" ->
                executeFreezeAllImports settings request
            | command_ == "dhall.server.explain" ->
                executeExplain request respond
            | command_ == "dhall.server.normalize" ->
                executeNormalize request respond
            | command_ == "dhall.server.showOriginalSource" ->
                executeShowOriginal settings request respond
            | otherwise -> do
                throwE
                    ( Warning
                    , "Command '" <> command_ <> "' not known; ignored."
                    )

getCommandArguments
    :: FromJSON a => TRequestMessage 'Method_WorkspaceExecuteCommand -> HandlerM a
-- (HasParams s a, FromJSON a) => s -> HandlerM a
getCommandArguments request = do
  json <- case request ^. params . arguments of
    Just (x : _) -> return x
    _ -> throwE (Error, "Failed to execute command; arguments missing.")
  case Aeson.fromJSON json of
    Aeson.Success args ->
        return args
    _ ->
        throwE (Error, "Failed to execute command; failed to parse arguments.")

-- implements dhall.server.lint
executeLintAndFormat
    :: TRequestMessage 'Method_WorkspaceExecuteCommand
    -> (Either a (Value |? Null) -> HandlerM b)
    -> HandlerM ()
executeLintAndFormat request respond = do
  uri_ <- getCommandArguments request
  txt <- readUri uri_

  (header, expr) <- case parseWithHeader txt of
    Right res -> return res
    _ -> throwE (Warning, "Failed to lint dhall code; parse error.")

  ServerConfig{..} <- liftLSP LSP.getConfig

  let numLines = fromIntegral (Text.length txt)

  let _newText = formatExprWithHeader chosenCharacterSet (lint expr) header

  let _range = LSP.Types.Range (Position 0 0) (Position numLines 0)

  let _edit =
          WorkspaceEdit
              { _changes = Just (Map.singleton uri_ [TextEdit{..}])
              , _documentChanges = Nothing
              , _changeAnnotations = Nothing
              }

  let _label = Nothing

  _ <- respond (Right (InL Aeson.Null))

  _ <- liftLSP (LSP.sendRequest SMethod_WorkspaceApplyEdit ApplyWorkspaceEditParams{ _label, _edit } nullHandler)

  return ()

executeAnnotateLet
    :: EvaluateSettings
    -> TRequestMessage 'Method_WorkspaceExecuteCommand
    -> HandlerM ()
executeAnnotateLet settings request = do
  args <- getCommandArguments request :: HandlerM TextDocumentPositionParams
  let uri_ = args ^. textDocument . uri
      line_ = fromIntegral (args ^. position . line)
      col_ = fromIntegral (args ^. position . character)

  expr <- loadFile settings uri_
  (welltyped, _) <- case typecheck settings expr of
    Left _ -> throwE (Warning, "Failed to annotate let binding; not well-typed.")
    Right e -> return e

  ServerConfig{..} <- liftLSP LSP.getConfig

  (Src (SourcePos _ x1 y1) (SourcePos _ x2 y2) _, annotExpr)
    <- case annotateLet (line_, col_) welltyped of
      Right x -> return x
      Left msg -> throwE (Warning, Text.pack msg)

  let _range = LSP.Types.Range (Position (fromIntegral (unPos x1 - 1)) (fromIntegral (unPos y1 - 1)))
                      (Position (fromIntegral (unPos x2 - 1)) (fromIntegral (unPos y2 - 1)))

  let _newText= formatExpr chosenCharacterSet annotExpr

  let _edit = WorkspaceEdit
          { _changes = Just (Map.singleton uri_ [TextEdit{..}])
          , _documentChanges = Nothing
          , _changeAnnotations = Nothing
          }

  let _label = Nothing

  _ <- liftLSP (LSP.sendRequest SMethod_WorkspaceApplyEdit ApplyWorkspaceEditParams{ _label, _edit } nullHandler)

  return ()

executeFreezeAllImports
    :: EvaluateSettings
    -> TRequestMessage 'Method_WorkspaceExecuteCommand
    -> HandlerM ()
executeFreezeAllImports settings request = do
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

    hashResult <- liftIO $ computeSemanticHash settings fileIdentifier importExpr cache
    (cache', hash) <- case hashResult of
      Right (c, t) -> return (c, t)
      Left _ -> throwE (Error, "Could not freeze import; failed to evaluate import.")
    assign importCache cache'

    let _range = LSP.Types.Range (Position (fromIntegral x1) (fromIntegral y1)) (Position (fromIntegral x2) (fromIntegral y2))
    let _newText = " " <> hash
    return TextEdit{..}

  let _edit = WorkspaceEdit
          { _changes = Just (Map.singleton uri_ edits_)
          , _documentChanges = Nothing
          , _changeAnnotations = Nothing
          }

  let _label = Nothing

  _ <- liftLSP (LSP.sendRequest SMethod_WorkspaceApplyEdit ApplyWorkspaceEditParams{ _edit, _label } nullHandler)

  return ()

executeFreezeImport
    :: EvaluateSettings
    -> TRequestMessage 'Method_WorkspaceExecuteCommand
    -> HandlerM ()
executeFreezeImport settings request = do
  args <- getCommandArguments request :: HandlerM TextDocumentPositionParams
  let uri_  = args ^. textDocument . uri
  let line_ = fromIntegral (args ^. position . line)
  let col_  = fromIntegral (args ^. position . character)

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

  hashResult <- liftIO $ computeSemanticHash settings fileIdentifier importExpr cache
  (cache', hash) <- case hashResult of
    Right (c, t) -> return (c, t)
    Left _ -> throwE (Error, "Could not freeze import; failed to evaluate import.")
  assign importCache cache'

  let _range = LSP.Types.Range (Position (fromIntegral x1) (fromIntegral y1)) (Position (fromIntegral x2) (fromIntegral y2))
  let _newText = " " <> hash

  let _edit = WorkspaceEdit
          { _changes = Just (Map.singleton uri_ [TextEdit{..}])
          , _documentChanges = Nothing
          , _changeAnnotations = Nothing
          }

  let _label = Nothing

  _ <- liftLSP (LSP.sendRequest SMethod_WorkspaceApplyEdit ApplyWorkspaceEditParams{ _edit, _label } nullHandler)

  return ()

completionHandler :: EvaluateSettings -> Handlers HandlerM
completionHandler settings =
  LSP.requestHandler SMethod_TextDocumentCompletion \request respond -> handleErrorWithDefault respond (InR (InL (CompletionList False Nothing []))) do
    let uri_  = request ^. params . textDocument . uri
        line_ = fromIntegral (request ^. params . position . line)
        col_  = fromIntegral (request ^. params . position . character)

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
            loadedBinders <- liftIO $ load settings fileIdentifier bindersExpr cache

            (cache', bindersExpr') <-
              case loadedBinders of
                Right (cache', binders) ->
                  return (cache', binders)
                Left _ -> throwE (Log, "Could not complete projection; failed to load binders expression.")

            let completionContext = buildCompletionContext bindersExpr'

            targetExpr <- case parse (Text.dropEnd 1 target_) of
              Right e -> return e
              Left _ -> throwE (Log, "Could not complete projection; prefix did not parse.")

            loaded' <- liftIO $ load settings fileIdentifier targetExpr cache'
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
            loadedBinders <- liftIO $ load settings fileIdentifier bindersExpr cache

            bindersExpr' <-
              case loadedBinders of
                Right (cache', binders) -> do
                  assign importCache cache'
                  return binders
                Left _ -> throwE (Log, "Could not complete projection; failed to load binders expression.")

            let context_ = buildCompletionContext bindersExpr'
                banned = namesBeingDefined bindersExpr

            return
                [ item
                | item <- completeFromContext context_
                , completeText item `notElem` banned
                ]

    completions <- computeCompletions

    let toCompletionItem (Completion {..}) = CompletionItem {..}
         where
          _label = completeText
          _labelDetails = Nothing
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
          _textEditText = Nothing
          _additionalTextEdits = Nothing
          _commitCharacters = Nothing
          _command = Nothing
          _data_ = Nothing

    let _items = (map toCompletionItem completions)
    let _itemDefaults = Nothing
    let _isIncomplete = False

    respond (Right (InR (InL CompletionList{..})))

nullHandler :: a -> LspT ServerConfig IO ()
nullHandler _ = return ()

executeExplain
    :: TRequestMessage 'Method_WorkspaceExecuteCommand
    -> (Either a (Value |? Null) -> HandlerM b)
    -> HandlerM ()
executeExplain request respond = do
    uri_ <- getCommandArguments request
    errorMap <- use errors
    explanation <- case Map.lookup uri_ errorMap >>= explain of
        Just diagnosis_ -> return diagnosis_
        Nothing -> throwE (Info, "There is no type error to explain in this file.")
    dir <- liftIO (getXdgDirectory XdgCache "dhall-lsp")
    liftIO (createDirectoryIfMissing True dir)
    let path = dir </> "explain.txt"
        body = diagnosis explanation
    liftIO (writeFile path (Text.unpack body))
    let _uri = filePathToUri path
        _external = Just False
        _takeFocus = Just True
        _selection = Nothing
    _ <- respond (Right (InL Aeson.Null))
    _ <- liftLSP $
            LSP.sendRequest
                SMethod_WindowShowDocument
                ShowDocumentParams { _uri, _external, _takeFocus, _selection }
                nullHandler
    return ()

executeNormalize
    :: TRequestMessage 'Method_WorkspaceExecuteCommand
    -> (Either a (Value |? Null) -> HandlerM b)
    -> HandlerM ()
executeNormalize request respond = do
    (uri_, range_, givenBytes) <- case request ^. params . arguments of
        Just [u, r] ->
            case (Aeson.fromJSON u, Aeson.fromJSON r) of
                (Aeson.Success uri_, Aeson.Success range_) ->
                    return (uri_, range_, Nothing)
                _ -> throwE (Error, "Could not parse normalize arguments.")
        Just [u, r, n] ->
            case (Aeson.fromJSON u, Aeson.fromJSON r, Aeson.fromJSON n) of
                (Aeson.Success uri_, Aeson.Success range_, Aeson.Success bytes) ->
                    return (uri_, range_, Just bytes)
                _ -> throwE (Error, "Could not parse normalize arguments.")
        _ -> throwE (Error, "Normalize selection is missing arguments.")
    txt <- readUri uri_
    let selected = textInRange txt range_
    expr <- case parse selected of
        Right e -> return e
        Left _ -> throwE (Warning, "The selection did not parse, so it was not normalized.")
    ServerConfig { maxOutputSize, chosenCharacterSet } <- liftLSP LSP.getConfig
    let bytes = fromMaybe maxOutputSize givenBytes
        -- Same safety limits the editor used before the output cap became
        -- optional: 256MiB of allocation and 30 seconds.
        allocationBytes = 256 * 1024 * 1024 :: Int64
        timeoutMicros = 30 * 1000 * 1000
    outcome <- liftIO $
        Bounded.normalizeLimited
            (Just allocationBytes)
            (Just timeoutMicros)
            (Just bytes)
            expr
    case outcome of
        Left _ ->
            throwE (Warning, "Evaluation limit exceeded; the edit was refused.")
        Right (_, True) ->
            throwE (Warning, "Normal form exceeded maxOutputSize; the edit was refused.")
        Right (nf, False) -> do
            let _newText = formatExpr chosenCharacterSet nf
                _range = range_
                _edit = WorkspaceEdit
                    { _changes = Just (Map.singleton uri_ [TextEdit { _range, _newText }])
                    , _documentChanges = Nothing
                    , _changeAnnotations = Nothing
                    }
                _label = Nothing
            _ <- respond (Right (InL Aeson.Null))
            _ <- liftLSP $
                    LSP.sendRequest
                        SMethod_WorkspaceApplyEdit
                        ApplyWorkspaceEditParams { _label, _edit }
                        nullHandler
            return ()

executeShowOriginal
    :: EvaluateSettings
    -> TRequestMessage 'Method_WorkspaceExecuteCommand
    -> (Either a (Value |? Null) -> HandlerM b)
    -> HandlerM ()
executeShowOriginal settings request respond = do
    uri_ <- getCommandArguments request :: HandlerM Uri
    txt <- readUri uri_
    parsed <- case parse txt of
        Right e -> return e
        Left _ -> throwE (Warning, "Could not parse this file.")
    let hashed =
            [ imp
            | (Core.Note _ (Core.Embed imp@(Import (ImportHashed (Just _) _) _))) <-
                toListOf Core.subExpressions parsed
            ]
    (imp, hash) <- case hashed of
        imp@(Import (ImportHashed (Just hash) _) _) : _ -> return (imp, hash)
        _ -> throwE (Info, "This file has no hashed import.")
    decoded <- liftIO (Import.decodeSemanticCache hash)
    ServerConfig { maxOutputSize } <- liftLSP LSP.getConfig
    let decodedText = case decoded of
            Nothing ->
                "-- semantic cache has no entry for this hash\n"
            Just expr ->
                case Bounded.prettyBounded
                        maxOutputSize
                        (Pretty.prettyCharacterSet Pretty.Unicode (Core.renote expr :: Expr Src Void)) of
                    Bounded.Complete text -> text
                    Bounded.Truncated text -> text
    negative <- use negativeImports
    cache <- use importCache
    fileIdentifier <- fileIdentifierFromUri uri_
    fetched <- liftIO $
        loadCollected settings fileIdentifier (Core.Embed imp) cache negative
    let (_, _, failures, sources) = fetched
        original = listToMaybe
            [ text
            | Import.ResolvedImportSource { Import.resolvedSourceText = Just text } <-
                Map.elems sources
            ]
        body = case (failures, original) of
            ([], Just text) -> text
            _ ->
                "-- showing the decoded semantic-cache entry; the original source was not fetched or did not match the hash\n"
                    <> decodedText
    dir <- liftIO (getXdgDirectory XdgCache ("dhall-lsp" </> "sources"))
    liftIO (createDirectoryIfMissing True dir)
    let path = dir </> "import" <.> "dhall"
    liftIO (writeFile path (Text.unpack body))
    let _uri = filePathToUri path
        _external = Just False
        _takeFocus = Just True
        _selection = Nothing
    _ <- respond (Right (InL Aeson.Null))
    _ <- liftLSP $
            LSP.sendRequest
                SMethod_WindowShowDocument
                ShowDocumentParams { _uri, _external, _takeFocus, _selection }
                nullHandler
    return ()

textInRange :: Text -> LSP.Types.Range -> Text
textInRange txt (LSP.Types.Range (Position startLine startCol) (Position endLine endCol)) =
    Text.unlines (take lineCount (drop (fromIntegral startLine) rows))
  where
    rows = Text.lines txt
    lineCount = fromIntegral (endLine - startLine) + 1
    _ = (startCol, endCol)

didOpenTextDocumentNotificationHandler :: EvaluateSettings -> Handlers HandlerM
didOpenTextDocumentNotificationHandler settings =
    LSP.notificationHandler SMethod_TextDocumentDidOpen \notification -> do
        let _uri = notification^.params.textDocument.uri
        diagnosticsHandler settings _uri

didSaveTextDocumentNotificationHandler :: EvaluateSettings -> Handlers HandlerM
didSaveTextDocumentNotificationHandler settings =
    LSP.notificationHandler SMethod_TextDocumentDidSave \notification -> do
        let _uri = notification^.params.textDocument.uri
        diagnosticsHandler settings _uri


-- this handler is a stab to prevent `lsp:no handler for:` messages.
initializedHandler :: Handlers HandlerM
initializedHandler =
    LSP.notificationHandler SMethod_Initialized \_ -> return ()

-- this handler is a stab to prevent `lsp:no handler for:` messages.
workspaceChangeConfigurationHandler :: Handlers HandlerM
workspaceChangeConfigurationHandler =
    LSP.notificationHandler SMethod_WorkspaceDidChangeConfiguration \_ -> return ()

-- | Re-analyse after a short pause so typing does not block other requests.
textDocumentChangeHandler :: EvaluateSettings -> Handlers HandlerM
textDocumentChangeHandler settings =
    LSP.notificationHandler SMethod_TextDocumentDidChange \notification -> do
        let _uri = notification ^. params . textDocument . uri
        mTxt <- tryReadUri _uri
        case mTxt of
            Nothing -> return ()
            Just txt -> do
                docs <- use documents
                generation <- liftIO $ IORef.atomicModifyIORef' docs $ \m ->
                    let previous = Map.lookup _uri m
                        generation = maybe 1 (\s -> snapGeneration s + 1) previous
                        snap = DocSnap
                            { snapVersion = 0
                            , snapGeneration = generation
                            , snapText = txt
                            , snapLastGood = snapLastGood =<< previous
                            , snapPrefixValues = maybe [] snapPrefixValues previous
                            , snapPrefixContexts = maybe [] snapPrefixContexts previous
                            }
                    in (Map.insert _uri snap m, generation)
                envRef <- use lspEnv
                snapshot <- lift State.get
                liftIO $ void $ forkIO $ do
                    threadDelay 300000
                    current <- IORef.readIORef docs
                    let still =
                            case Map.lookup _uri current of
                                Just snap -> snapGeneration snap == generation
                                Nothing -> False
                    when still $ do
                        menv <- IORef.readIORef envRef
                        case menv of
                            Nothing -> return ()
                            Just env ->
                                void $ LSP.runLspT env $
                                    State.evalStateT
                                        (Except.runExceptT (diagnoseDocument settings _uri txt))
                                        snapshot

-- this handler is a stab to prevent `lsp:no handler for:` messages.
cancelationHandler :: Handlers HandlerM
cancelationHandler =
    LSP.notificationHandler SMethod_CancelRequest \_ -> return ()

-- This handler is a stub to prevent `lsp:no handler for:` messages.
documentDidCloseHandler :: Handlers HandlerM
documentDidCloseHandler =
    LSP.notificationHandler SMethod_TextDocumentDidClose \notification -> do
        let _uri = notification ^. params . textDocument . uri
        docs <- use documents
        liftIO $ IORef.modifyIORef' docs (Map.delete _uri)
        modifying errors (Map.delete _uri)
        let _version = Nothing
            _diagnostics = []
        liftLSP $
            LSP.sendNotification
                SMethod_TextDocumentPublishDiagnostics
                PublishDiagnosticsParams { _uri, _version, _diagnostics }

handleErrorWithDefault :: (Either a1 b -> HandlerM a2)
 -> b
 -> HandlerM a2
 -> HandlerM a2
handleErrorWithDefault respond _default = flip catchE handler
  where
    handler (Log, _message)  = do
                    let _type_ = MessageType_Log
                    liftLSP $ LSP.sendNotification SMethod_WindowLogMessage LogMessageParams{..}
                    respond (Right _default)

    handler (severity_, _message) = do
                    let _type_ = case severity_ of
                          Error   -> MessageType_Error
                          Warning -> MessageType_Warning
                          Info    -> MessageType_Info
#if !MIN_TOOL_VERSION_ghc(9,2,0)
                          Log     -> MessageType_Log
#endif

                    liftLSP $ LSP.sendNotification SMethod_WindowShowMessage ShowMessageParams{..}
                    respond (Right _default)
