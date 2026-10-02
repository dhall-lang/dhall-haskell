{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP            #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE MultiWayIf     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE ViewPatterns   #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Dhall.LSP.Handlers where

import Data.Void    (Void, absurd)
import Dhall        (EvaluateSettings)
import Dhall.Core
    ( Expr (Embed, Note)
    , Import (..)
    , ImportHashed (..)
    , ImportType (..)
    , headers
    , pretty
    )
import Dhall.Import (chainedImport, localToPath)
import Dhall.Parser (Src (..))

import Dhall.LSP.Backend.Completion
    ( Completion (..)
    , buildCompletionContext
    , completeEnvironmentImport
    , completeFromContext
    , completeLocalImport
    , completeProjections
    , completionQueryAt
    , expressionBefore
    )
import Dhall.LSP.Backend.Dhall
    ( FileIdentifier
    , fileIdentifierFromFilePath
    , fileIdentifierFromURI
    , invalidate
    , load
    , loadCollected
    , indexImportBodies
    , indexImportChains
    , fileIdentifierFromChained
    , parse
    , parseWithHeader
    , typecheck
    , DhallError (..)
    )
import Dhall.LSP.Backend.Diagnostics
    ( Diagnosis (..)
    , Range (..)
    , clipUserText
    , diagnose
    , embedsWithRanges
    , positionToOffset
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
import Dhall.LSP.Backend.Prefix
    ( resumeTypingContext
    , topLevelPrefixLength
    , topLets
    )
import Dhall.LSP.Backend.Typing
    ( annotateLet
    , exprAt
    , scopedNormalize
    , typeAtExpr
    , typeAtExprWithContextBounded
    )
import Dhall.LSP.State

import Control.Applicative           ((<|>))
import Control.DeepSeq               (force)
import Control.Exception             (SomeAsyncException, SomeException, evaluate, fromException)
import Control.Lens                  (assign, modifying, over, toListOf, use, (^.))
import Control.Monad                 (forM, forM_, guard)
import Control.Monad.Trans           (lift, liftIO)
import Control.Concurrent            (forkIO, threadDelay)
import Control.Monad                 (void, when)
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
import Unsafe.Coerce                 (unsafeCoerce)
import System.Directory              (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath               (takeDirectory, takeFileName, (<.>), (</>))
import System.IO                     (hPutStrLn, stderr)
import Text.Megaparsec               (SourcePos (..), unPos)

import qualified Control.Exception                as Exception
import qualified Control.Monad.Catch              as MC
import qualified Control.Monad.Trans.Except       as Except
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Aeson                       as Aeson
import qualified Data.ByteString                  as ByteString
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
import qualified Network.URI.Encode          as URIEncode

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
  cache <- readImportCache

  expr <- case parse txt of
    Right e -> return e
    _ -> throwE (Error, "Failed to parse Dhall file.")

  negative <- use negativeImports
  loaded <- liftIO $ loadCollected settings fileIdentifier expr cache negative
  (cache', expr', errs) <- case loaded of
    Left err ->
      throwE (Error, Text.intercalate "\n" [ msg | Diagnosis _ _ msg <- diagnose err ])
    Right (cache', expr', errs, _) ->
      return (cache', expr', errs)
  -- Update cache. Don't cache current expression because it might not have been
  -- written to disk yet (readUri reads from the VFS).
  writeImportCache cache'
  if null errs
    then return (unsafeCoerce expr')
    else throwE (Error, "Failed to resolve imports." <> importFailureText errs)
  where
    importFailureText failed =
        Text.pack
            (concatMap
                (\err ->
                    concatMap
                        (\ex -> "\n" <> Import.plainShowImportError ex)
                        (Import.collectedErrors err))
                failed)

-- | Load the buffer for typing or scoped evaluation.  A missing import
--   becomes a hole rather than aborting.
loadForTyping :: EvaluateSettings -> Uri -> HandlerM (Maybe (Expr Src Void))
loadForTyping settings uri_ = do
    txt <- readUri uri_
    case parse txt of
        Right parsed ->
            tryLoadForTyping settings uri_ parsed
        Left _ -> do
            docs <- use documents
            snaps <- liftIO (IORef.readIORef docs)
            case Map.lookup uri_ snaps >>= snapLastGood of
                Just good ->
                    case parse good of
                        Right parsed ->
                            tryLoadForTyping settings uri_ parsed
                        Left _ ->
                            return Nothing
                Nothing ->
                    return Nothing

tryLoadForTyping
    :: EvaluateSettings -> Uri -> Expr Src Import -> HandlerM (Maybe (Expr Src Void))
tryLoadForTyping settings uri_ parsed = do
    fileIdentifier <- fileIdentifierFromUri uri_
    cache <- readImportCache
    negative <- use negativeImports
    loaded <- liftIO $ loadCollected settings fileIdentifier parsed cache negative
    case loaded of
        Left _ ->
            return Nothing
        Right (cache', expr, collected, _) -> do
            writeImportCache cache'
            return (Just (fillImportHoles collected expr))

-- helper
fileIdentifierFromUri :: Uri -> HandlerM FileIdentifier
fileIdentifierFromUri uri_ = do
  originsRef <- use mirrorOrigins
  origins <- liftIO (IORef.readIORef originsRef)
  case listToMaybe [ chained | key <- mirrorKeys uri_, Just chained <- [Map.lookup key origins] ] of
    Just chained ->
      return (fileIdentifierFromChained chained)
    Nothing ->
      let mFileIdentifier = fmap fileIdentifierFromFilePath (uriToFilePath uri_)
                            <|> (do uri' <- (URI.parseURI . Text.unpack . getUri) uri_
                                    fileIdentifierFromURI uri')
      in case mFileIdentifier of
        Just fileIdentifier -> return fileIdentifier
        Nothing -> throwE (Error, getUri uri_ <> " is not a valid name for a dhall file.")

-- | Cache path or 'dhall-import:' file name under which a mirror was published.
mirrorKeys :: Uri -> [FilePath]
mirrorKeys uri_
    | Just rest <- Text.stripPrefix "dhall-import:" (getUri uri_) =
        [takeFileName (Text.unpack (Text.dropWhile (== '/') rest))]
    | Just path <- uriToFilePath uri_ =
        [path]
    | otherwise =
        []

-- helper
rangeToJSON :: Range -> LSP.Types.Range
rangeToJSON (Range (x1,y1) (x2,y2)) =
    LSP.Types.Range
      (Position (fromIntegral x1) (fromIntegral y1))
      (Position (fromIntegral x2) (fromIntegral y2))

-- helper
rangeFromJSON :: LSP.Types.Range -> Range
rangeFromJSON (LSP.Types.Range (Position x1 y1) (Position x2 y2)) =
    Range (fromIntegral x1, fromIntegral y1) (fromIntegral x2, fromIntegral y2)

-- helper
rangesOverlap :: Range -> Range -> Bool
rangesOverlap (Range left1 right1) (Range left2 right2) =
    left1 <= right2 && left2 <= right1

-- | Render a type hover.
typeToHover :: Int -> Maybe Src -> Expr Src Void -> Hover
typeToHover maxOutputSize mSrc typ = Hover{ _contents, _range }
  where
    _range = fmap (rangeToJSON . rangeFromDhall) mSrc
    rendered =
        case Bounded.prettyBounded
                maxOutputSize
                (Pretty.prettyCharacterSet Pretty.Unicode typ) of
            Bounded.Complete text -> text
            Bounded.Truncated text -> text
    _contents = InL (mkPlainText rendered)

-- | The source text covered by a range.
sliceInRange :: Text -> Range -> Text
sliceInRange source (Range left right) =
    Text.take (max 0 (to - from)) (Text.drop from source)
  where
    from = positionToOffset source left
    to = positionToOffset source right

-- | Type of the expression at `pos`, when that source slice is unchanged
--   between `fromText` (the expression's origin) and `current`.
hoverFromExpr
    :: Int
    -> (Int, Int)
    -> Text
    -> Text
    -> [Text]
    -> [Core.Expr Void Void]
    -> [TypeCheck.TypingContext Src]
    -> Expr Src Void
    -> Maybe Hover
hoverFromExpr maxOutputSize pos fromText current prefixNames prefixValues prefixCtxs expr =
    let ctx = resumeTypingContext prefixNames prefixValues prefixCtxs expr
    in case typeAtExprWithContextBounded (Just maxOutputSize) ctx pos expr of
        Right (Just src, typ, _)
            | let range_ = rangeFromDhall src
            , sliceInRange fromText range_ == sliceInRange current range_ ->
                Just (typeToHover maxOutputSize (Just src) typ)
        Right (Nothing, typ, _) ->
            Just (typeToHover maxOutputSize Nothing typ)
        _ ->
            Nothing

-- | Type hover over the last text that parsed, used while the current
--   buffer has a syntax error.  Only answers when the hovered expression's
--   source slice is unchanged in the current text, so the type still
--   describes the code under the cursor.
lastGoodTypeHover
    :: EvaluateSettings -> Uri -> (Int, Int) -> Text -> HandlerM (Maybe Hover)
lastGoodTypeHover settings uri_ pos current = do
    docs <- use documents
    snaps <- liftIO (IORef.readIORef docs)
    ServerConfig { maxOutputSize } <- liftLSP LSP.getConfig
    case Map.lookup uri_ snaps of
        Just snap | Just good <- snapLastGood snap, good /= current ->
            case parse good of
                Left _ ->
                    return Nothing
                Right parsed -> do
                    loaded <- tryLoadForTyping settings uri_ parsed
                    return
                        ( loaded
                            >>= hoverFromExpr
                                maxOutputSize
                                pos
                                good
                                current
                                (snapPrefixNames snap)
                                (snapPrefixValues snap)
                                (snapPrefixContexts snap)
                        )
        _ ->
            return Nothing

currentTypeHover
    :: EvaluateSettings -> Uri -> (Int, Int) -> Text -> HandlerM (Maybe Hover)
currentTypeHover settings uri_ pos current = do
    docs <- use documents
    snaps <- liftIO (IORef.readIORef docs)
    case parse current of
        Left _ ->
            return Nothing
        Right parsed -> do
            ServerConfig { maxOutputSize } <- liftLSP LSP.getConfig
            loaded <- tryLoadForTyping settings uri_ parsed
            let prefix = case Map.lookup uri_ snaps of
                    Just snap ->
                        ( snapPrefixNames snap
                        , snapPrefixValues snap
                        , snapPrefixContexts snap
                        )
                    Nothing ->
                        ([], [], [])
            return
                ( loaded
                    >>= hoverFromExpr
                        maxOutputSize
                        pos
                        current
                        current
                        (fst3 prefix)
                        (snd3 prefix)
                        (thd3 prefix)
                )
  where
    fst3 (a, _, _) = a
    snd3 (_, b, _) = b
    thd3 (_, _, c) = c

hoverHandler :: EvaluateSettings -> Handlers HandlerM
hoverHandler settings =
    LSP.requestHandler SMethod_TextDocumentHover \request respond -> handleErrorWithDefault respond (InR LSP.Types.Null) do
        let uri_ = request^.params.textDocument.uri

        let Position{ _line = fromIntegral -> _line, _character = fromIntegral -> _character } = request^.params.position

        txt <- readUri uri_
        fromCurrent <- currentTypeHover settings uri_ (_line, _character) txt
        mHover <- case fromCurrent of
            Just hover ->
                return (Just hover)
            Nothing ->
                lastGoodTypeHover settings uri_ (_line, _character) txt
        respond (Right (maybeToNull mHover))

documentLinkHandler :: Handlers HandlerM
documentLinkHandler =
    LSP.requestHandler SMethod_TextDocumentDocumentLink \request respond -> handleErrorWithDefault respond (InL []) do
        let uri_ = request^.params.textDocument.uri

        originsRef <- use mirrorOrigins
        origins <- liftIO (IORef.readIORef originsRef)
        let mOrigin = listToMaybe
                [ chained
                | key <- mirrorKeys uri_
                , Just chained <- [Map.lookup key origins]
                ]
        path <- case (mOrigin, uriToFilePath uri_) of
            (Just _, _) ->
                return "."
            (_, Just p) ->
                return p
            (Nothing, Nothing) ->
                throwE (Log, "Could not process document links; failed to convert URI to file path.")

        txt <- readUri uri_

        -- A syntax error has no AST.  Navigation already reuses the last
        -- buffer that parsed; document links do the same.
        source <- case parse txt of
            Right _ ->
                return txt
            Left _ -> do
                snaps <- liftIO . IORef.readIORef =<< use documents
                return (fromMaybe txt (Map.lookup uri_ snaps >>= snapLastGood))

        case parse source of
            Left _ ->
                respond (Right (InL []))
            Right expr -> do
                let imports = embedsWithRanges expr :: [(Range, Import)]

                let basePath = takeDirectory path

                -- A mirror is the text of some other import.  Links chain onto that
                -- import, so ./Bool/package.dhall inside the Prelude stays on the
                -- Prelude's location.
                let adjust imp = case mOrigin of
                        Just parent ->
                            chainedImport parent <> imp
                        Nothing ->
                            imp

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

                links <- liftIO $ mapM go (map (\(range_, imp) -> (range_, adjust imp)) imports)
                respond (Right (InL (concat links)))


-- | Log line for a document whose links could not be extracted: names the
--   document and where the parse failed.
documentLinkParseError :: Uri -> DhallError -> Text
documentLinkParseError uri_ err =
    "Could not process document links for "
        <> getUri uri_
        <> "; did not parse"
        <> location
        <> ": "
        <> message
  where
    firstDiagnosis = listToMaybe (diagnose err)
    location = case firstDiagnosis of
        Just (Diagnosis _ (Just (Range (line_, col) _)) _) ->
            " at line " <> Text.pack (show (line_ + 1)) <> ", column " <> Text.pack (show (col + 1))
        _ ->
            ""
    message = maybe "" (\(Diagnosis _ _ text_) -> text_) firstDiagnosis

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
            clipUserText
                (Text.pack
                    (concatMap
                        Import.plainShowImportError
                        (Import.collectedErrors err)))
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
--   A prefix of bindings whose names and denoted values match the previous
--   analysis reuses that typing context.  Each binding that fails contributes
--   its own error.  Placeholders with a known type are bound first.  If any
--   failure has no known type, type-checking is skipped: those dependents are
--   not reported.
typecheckCollected
    :: [Import.CollectedImportError]
    -> Expr Src Import.ImportHole
    -> [Text]
    -> [Core.Expr Void Void]
    -> [TypeCheck.TypingContext Src]
    -> ([DhallError], [Text], [Core.Expr Void Void], [TypeCheck.TypingContext Src])
typecheckCollected collected expr prevNames prevValues prevCtxs
    | any (\e -> isNothing (Import.collectedKnownType e)) collected =
        ([], [], [], [])
    | any (not . (`Map.member` knownTypes)) (holesIn expr) =
        ([], [], [], [])
    | otherwise =
        let erased = fillImportHoles collected expr
            (errs, names, values, ctxs) =
                checkLets
                    prevNames
                    prevValues
                    prevCtxs
                    TypeCheck.emptyTypingContext
                    erased
        in (map ErrorTypecheck errs, names, values, ctxs)
  where
    holesIn (Core.Embed hole) =
        [hole]
    holesIn (Core.Note _ child) =
        holesIn child
    holesIn other =
        concatMap holesIn (toListOf Core.subExpressions other)

    knownTypes =
        Map.fromList
            [ (Import.collectedHole err, known)
            | err <- collected
            , Just known <- [Import.collectedKnownType err]
            ]

-- | Replace each import hole with a typed placeholder.  A hole whose
--   type is unknown becomes 'Sort', which cannot be used as a value, so
--   later typing skips that binding instead of inventing a type.
fillImportHoles
    :: [Import.CollectedImportError]
    -> Expr Src Import.ImportHole
    -> Expr Src Void
fillImportHoles collected expr =
    unsafeCoerce (eraseHoles expr)
  where
    knownTypes =
        Map.fromList
            [ (Import.collectedHole err, known)
            | err <- collected
            , Just known <- [Import.collectedKnownType err]
            ]

    eraseHoles (Core.Embed hole) =
        case Map.lookup hole knownTypes of
            Just known ->
                knownValue known
            Nothing ->
                unsafeCoerce (Core.Const Core.Sort :: Expr Src Void)
    eraseHoles (Core.Note src child) =
        Core.Note src (eraseHoles child)
    eraseHoles other =
        over Core.subExpressions eraseHoles other

    knownValue Import.KnownText =
        unsafeCoerce (Core.TextLit (Core.Chunks [] "") :: Expr Src Void)
    knownValue Import.KnownBytes =
        unsafeCoerce (Core.BytesLit ByteString.empty :: Expr Src Void)
    knownValue Import.KnownLocation =
        unsafeCoerce
            (Core.Field
                (Core.Union
                    (Dhall.Map.fromList
                        [ ("Environment", Just Core.Text)
                        , ("Remote", Just Core.Text)
                        , ("Local", Just Core.Text)
                        , ("Missing", Nothing)
                        ]))
                (Core.FieldSelection Nothing "Missing" Nothing)
                :: Expr Src Void)

checkLets
    :: [Text]
    -> [Core.Expr Void Void]
    -> [TypeCheck.TypingContext Src]
    -> TypeCheck.TypingContext Src
    -> Expr Src Void
    -> ( [TypeCheck.TypeError Src Void]
       , [Text]
       , [Core.Expr Void Void]
       , [TypeCheck.TypingContext Src]
       )
checkLets prevNames prevValues prevCtxs ctx0 expr =
    let (binds, rest) = topLets expr
        prefixLen = topLevelPrefixLength prevNames prevValues binds
        step (i, accCtx, accNames, accVals, accCtxs, accErrs, accFailed) (name, ann, value)
            | i < prefixLen =
                ( i + 1
                , prevCtxs !! i
                , name : accNames
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
                        , name : accNames
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
                        in (i + 1, ctx', accNames, accVals, accCtxs, err : accErrs, True)
        (_, ctx, names, vals, ctxs, errs, failed) =
            foldl step (0, ctx0, [], [], [], [], False) binds
        errs' =
            if failed
                then errs
                else case TypeCheck.typeWithContext ctx rest of
                    Left err -> err : errs
                    Right _ -> errs
    in (reverse errs', reverse names, reverse vals, reverse ctxs)

-- | Semantic diagnostics that survive a later parse error: their source
--   slice is unchanged in the new text and ends before the parse error
--   starts.
survivingErrors
    :: Text                      -- ^ current text
    -> DocErrors                 -- ^ errors of the previous analysis
    -> (Int, Int)                -- ^ where the parse error starts
    -> ([Import.CollectedImportError], [DhallError])
survivingErrors txt previous parseStart =
    ( [ err | err <- errImports previous, keeps (rangeFromDhall (Import.collectedSrc err)) ]
    , [ err | err <- errTypes previous, Just range_ <- [errorRange err], keeps range_ ]
    )
  where
    keeps range_@(Range _ right) =
        right <= parseStart
            && sliceInRange (errSemanticText previous) range_ == sliceInRange txt range_

-- | The range of the first diagnosis of an error, if it has one.
errorRange :: DhallError -> Maybe Range
errorRange err =
    case diagnose err of
        Diagnosis _ range_ _ : _ -> range_
        [] -> Nothing

diagnoseDocument :: EvaluateSettings -> Uri -> Text -> HandlerM ()
diagnoseDocument settings _uri txt = do
  fileIdentifier <- fileIdentifierFromUri _uri
  -- make sure we don't keep a stale version around
  modifyImportCache (invalidate fileIdentifier)
  cache <- readImportCache

  errorsRef <- use errors
  previousErrors <- liftIO $ Map.lookup _uri <$> IORef.readIORef errorsRef

  (parseErrors, importErrors, typeErrors, semanticText) <- case parse txt of
      Left err -> do
          let parseStart =
                  case [ left | Diagnosis _ (Just (Range left _)) _ <- diagnose err ] of
                      [] -> (0, 0)
                      starts -> minimum starts
              (keptImports, keptTypes) =
                  case previousErrors of
                      Nothing -> ([], [])
                      Just previous -> survivingErrors txt previous parseStart
              previousText =
                  maybe txt errSemanticText previousErrors
          return ([err], keptImports, keptTypes, previousText)
      Right parsed -> do
          negative <- use negativeImports
          loaded <- liftIO $ loadCollected settings fileIdentifier parsed cache negative
          case loaded of
            Left err ->
              return ([], [], [err], txt)
            Right (cache', resolved, collected, sources) -> do
              writeImportCache cache'
              bodiesRef <- use importBodies
              chainsRef <- use importChains
              originsRef <- use mirrorOrigins
              let chains = indexImportChains sources
              liftIO $ do
                  IORef.modifyIORef' bodiesRef
                      (Map.union (indexImportBodies sources))
                  IORef.modifyIORef' chainsRef (Map.union chains)
                  -- Absolute locations only.  A bare file name would collide, and the
                  -- dhall-import name is recorded when the view is published.
                  IORef.modifyIORef' originsRef $
                      \old ->
                        Map.union old $
                          Map.mapKeys Text.unpack $
                            Map.filterWithKey (\key _ -> '/' `elem` Text.unpack key) chains
              docs <- use documents
              previousSnap <- liftIO $ Map.lookup _uri <$> IORef.readIORef docs
              let prevNames = maybe [] snapPrefixNames previousSnap
                  prevValues = maybe [] snapPrefixValues previousSnap
                  prevCtxs = maybe [] snapPrefixContexts previousSnap
                  (typeErrs, prefixNames, prefixValues, prefixCtxs) =
                      typecheckCollected collected resolved prevNames prevValues prevCtxs
              liftIO $ IORef.modifyIORef' docs $ \m ->
                  let previous = Map.lookup _uri m
                      snap = DocSnap
                          { snapVersion = maybe 0 snapVersion previous
                          , snapGeneration = maybe 0 snapGeneration previous
                          , snapText = txt
                          , snapLastGood = Just txt
                          , snapPrefixNames = prefixNames
                          , snapPrefixValues = prefixValues
                          , snapPrefixContexts = prefixCtxs
                          }
                  in Map.insert _uri snap m
              return ([], collected, typeErrs, txt)

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

  liftIO $ IORef.modifyIORef' errorsRef $ \errorMap ->
      if null parseErrors && null importErrors && null typeErrors
          then Map.delete _uri errorMap
          else Map.insert _uri (DocErrors parseErrors importErrors typeErrors semanticText) errorMap

  let _version = Nothing
  let _diagnostics =
              concatMap (map diagnosisToDiagnostic . diagnose) parseErrors
              ++ map (collectedDiagnostic _uri) importErrors
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
            | command_ == "dhall.server.unfreezeImport" ->
                executeUnfreezeImport request
            | command_ == "dhall.server.unfreezeAllImports" ->
                executeUnfreezeAllImports request
            | command_ == "dhall.server.checkImportHash" ->
                executeCheckImportHash settings request
            | command_ == "dhall.server.explain" ->
                executeExplain request respond
            | command_ == "dhall.server.normalize" ->
                executeNormalize settings request respond
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

  -- The annotation Src starts right before the colon (or, for an
  -- unannotated let, is a zero-width span right before the @=@) and runs
  -- through the whitespace before @=@.  Replacing it with colon, type, and
  -- one trailing space keeps both sides spaced: @let a : Natural = 2@.
  let _newText = ": " <> formatExpr chosenCharacterSet annotExpr <> " "

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
    cache <- readImportCache
    let importExpr = Embed (stripHash import_)

    hashResult <- liftIO $ computeSemanticHash settings fileIdentifier importExpr cache
    (cache', hash) <- case hashResult of
      Right (c, t) -> return (c, t)
      Left _ -> throwE (Error, "Could not freeze import; failed to evaluate import.")
    writeImportCache cache'

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

-- | The import whose span contains the cursor, or a warning for the user.
--   Shared by the freeze, unfreeze, and check-hash commands.
importAtCursor :: Expr Src Import -> (Int, Int) -> HandlerM (Src, Import)
importAtCursor expr pos =
    case exprAt pos expr of
        Just (Note src (Embed i)) -> return (src, i)
        _ -> throwE (Warning, "You weren't pointing at an import!")

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

  (src, import_) <- importAtCursor expr (line_, col_)

  Range (x1, y1) (x2, y2) <- case getImportHashPosition src of
      Just range_ -> return range_
      Nothing -> throwE (Error, "Failed to re-parse import!")

  fileIdentifier <- fileIdentifierFromUri uri_
  cache <- readImportCache
  let importExpr = Embed (stripHash import_)

  hashResult <- liftIO $ computeSemanticHash settings fileIdentifier importExpr cache
  (cache', hash) <- case hashResult of
    Right (c, t) -> return (c, t)
    Left _ -> throwE (Error, "Could not freeze import; failed to evaluate import.")
  writeImportCache cache'

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

-- | Normalize one hashed import and compare the result with its annotation.
executeCheckImportHash
    :: EvaluateSettings
    -> TRequestMessage 'Method_WorkspaceExecuteCommand
    -> HandlerM ()
executeCheckImportHash settings request = do
  args <- getCommandArguments request :: HandlerM TextDocumentPositionParams
  let uri_  = args ^. textDocument . uri
  let line_ = fromIntegral (args ^. position . line)
  let col_  = fromIntegral (args ^. position . character)

  txt <- readUri uri_
  expr <- case parse txt of
    Right e -> return e
    Left _ -> throwE (Warning, "Could not check import hash; did not parse.")

  (_, import_) <- importAtCursor expr (line_, col_)

  digest <- case import_ of
    Import (ImportHashed (Just digest) _) _ ->
        return digest
    _ ->
        throwE (Info, "This import has no hash to check.")

  fileIdentifier <- fileIdentifierFromUri uri_
  cache <- readImportCache
  hashResult <-
    liftIO $ computeSemanticHash settings fileIdentifier (Embed (stripHash import_)) cache
  (cache', actual) <- case hashResult of
    Right found -> return found
    Left _ -> throwE (Error, "Could not check import hash; failed to evaluate import.")
  writeImportCache cache'

  let expected = "sha256:" <> Text.pack (show digest)
  if actual == expected
    then throwE (Info, "Import hash matches.")
    else throwE
        ( Error
        , "Import hash does not match.\nExpected "
            <> expected
            <> "\nActual "
            <> actual
        )

-- | Delete one import hash.  A @missing@ import is left alone: without the
--   hash it does not resolve.
executeUnfreezeImport
    :: TRequestMessage 'Method_WorkspaceExecuteCommand
    -> HandlerM ()
executeUnfreezeImport request = do
  args <- getCommandArguments request :: HandlerM TextDocumentPositionParams
  let uri_  = args ^. textDocument . uri
  let line_ = fromIntegral (args ^. position . line)
  let col_  = fromIntegral (args ^. position . character)

  txt <- readUri uri_
  expr <- case parse txt of
    Right e -> return e
    Left _ -> throwE (Warning, "Could not unfreeze import; did not parse.")

  (src, import_) <- importAtCursor expr (line_, col_)

  case import_ of
    Import (ImportHashed _ Missing) _ ->
      throwE (Warning, "A missing import is left unchanged, because without its hash it always fails.")
    _ -> return ()

  Range (x1, y1) (x2, y2) <- case getImportHashPosition src of
      Just range_ -> return range_
      Nothing -> throwE (Error, "Failed to re-parse import!")

  let _range = LSP.Types.Range (Position (fromIntegral x1) (fromIntegral y1)) (Position (fromIntegral x2) (fromIntegral y2))
      _newText = ""
      _edit = WorkspaceEdit
          { _changes = Just (Map.singleton uri_ [TextEdit{..}])
          , _documentChanges = Nothing
          , _changeAnnotations = Nothing
          }
      _label = Nothing

  _ <- liftLSP (LSP.sendRequest SMethod_WorkspaceApplyEdit ApplyWorkspaceEditParams{ _edit, _label } nullHandler)
  return ()

executeUnfreezeAllImports
    :: TRequestMessage 'Method_WorkspaceExecuteCommand
    -> HandlerM ()
executeUnfreezeAllImports request = do
  uri_ <- getCommandArguments request
  txt <- readUri uri_
  expr <- case parse txt of
    Right e -> return e
    Left _ -> throwE (Warning, "Could not unfreeze imports; did not parse.")

  let edits_ =
        [ TextEdit
            { _range = LSP.Types.Range
                (Position (fromIntegral x1) (fromIntegral y1))
                (Position (fromIntegral x2) (fromIntegral y2))
            , _newText = ""
            }
        | (import_, Range (x1, y1) (x2, y2)) <- getAllImportsWithHashPositions expr
        , case import_ of
            Import (ImportHashed _ Missing) _ -> False
            _ -> True
        ]
      _edit = WorkspaceEdit
          { _changes = Just (Map.singleton uri_ edits_)
          , _documentChanges = Nothing
          , _changeAnnotations = Nothing
          }
      _label = Nothing

  _ <- liftLSP (LSP.sendRequest SMethod_WorkspaceApplyEdit ApplyWorkspaceEditParams{ _edit, _label } nullHandler)
  return ()

-- | Complete a record or union that is not a plain dotted name, such as
--   `(f x).` or `{ a = 1 }.`.  The completion target is the balanced
--   expression before the dot, typechecked in the context of the binders
--   leading up to it, so the surrounding code does not have to parse.
completeBeforeDot
    :: EvaluateSettings
    -> Uri
    -> Text
    -> (Int, Int)
    -> HandlerM [Completion]
completeBeforeDot settings uri_ txt (line_, col_) = do
    let off = positionToOffset txt (line_, col_)
        before = Text.take off txt
        (lead, typed) = Text.breakOnEnd "." before
        baseCol = col_ - fromIntegral (Text.length typed) - 1
    if Text.null lead || Text.any (== '\n') typed || baseCol < 0
        then return []
        else do
            let beforeDot = Text.dropEnd (Text.length typed + 1) before
            case expressionBefore beforeDot of
                Nothing ->
                    return []
                Just (start, targetText) -> do
                    fileIdentifier <- fileIdentifierFromUri uri_
                    cache <- readImportCache
                    let bindersExpr = binderExprFromText (Text.take start beforeDot)
                    loadedBinders <- liftIO $ load settings fileIdentifier bindersExpr cache
                    case loadedBinders of
                        Left _ ->
                            return []
                        Right (cache', bindersExpr') ->
                            case parse targetText of
                                Left _ ->
                                    return []
                                Right targetExpr -> do
                                    loaded <- liftIO $ load settings fileIdentifier targetExpr cache'
                                    case loaded of
                                        Left _ ->
                                            return []
                                        Right (cache'', targetExpr') -> do
                                            writeImportCache cache''
                                            return $
                                                completeProjections
                                                    (buildCompletionContext bindersExpr')
                                                    targetExpr'

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
            cache <- readImportCache
            loadedBinders <- liftIO $ load settings fileIdentifier bindersExpr cache

            (cache', bindersExpr') <-
              case loadedBinders of
                Right (cache', binders) ->
                  return (cache', binders)
                Left _ -> throwE (Log, "Could not complete projection; failed to load binders expression.")

            let completionContext = buildCompletionContext bindersExpr'

            case parse (Text.dropEnd 1 target_) of
              Left _ ->
                  return []
              Right targetExpr -> do
                loaded' <- liftIO $ load settings fileIdentifier targetExpr cache'
                case loaded' of
                  Right (cache'', targetExpr') -> do
                    writeImportCache cache''
                    return (completeProjections completionContext targetExpr')
                  Left _ -> return []

          -- complete identifiers in scope
          | otherwise = do
            let bindersExpr = binderExprFromText completionLeadup

            fileIdentifier <- fileIdentifierFromUri uri_
            cache <- readImportCache  -- todo save cache afterwards
            loadedBinders <- liftIO $ load settings fileIdentifier bindersExpr cache

            bindersExpr' <-
              case loadedBinders of
                Right (cache', binders) -> do
                  writeImportCache cache'
                  return binders
                Left _ -> throwE (Log, "Could not complete projection; failed to load binders expression.")

            let context_ = buildCompletionContext bindersExpr'
                banned = namesBeingDefined bindersExpr

            return
                [ item
                | item <- completeFromContext context_
                , completeText item `notElem` banned
                ]

    dotted <- computeCompletions
    typed <-
        if null dotted
            then completeBeforeDot settings uri_ txt (line_, col_)
            else return []
    let completions = dotted ++ typed

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

-- implements dhall.server.explain
--
-- The first argument is the document URI.  An optional second argument is
-- the range of the diagnostic the client wants explained; without it the
-- first explainable error is used.
executeExplain
    :: TRequestMessage 'Method_WorkspaceExecuteCommand
    -> (Either a (Value |? Null) -> HandlerM b)
    -> HandlerM ()
executeExplain request respond = do
    (uri_, wantedRange) <- case request ^. params . arguments of
        Just [u] ->
            case Aeson.fromJSON u of
                Aeson.Success uri_ ->
                    return (uri_, Nothing)
                _ ->
                    throwE (Error, "Failed to execute command; failed to parse arguments.")
        Just [u, r] ->
            case (Aeson.fromJSON u, Aeson.fromJSON r) of
                (Aeson.Success uri_, Aeson.Success range_) ->
                    return (uri_, Just range_)
                _ ->
                    throwE (Error, "Failed to execute command; failed to parse arguments.")
        _ ->
            throwE (Error, "Failed to execute command; arguments missing.")
    ServerConfig { maxOutputSize } <- liftLSP LSP.getConfig
    errorsRef <- use errors
    errorMap <- liftIO (IORef.readIORef errorsRef)
    explanations <- case Map.lookup uri_ errorMap of
        Nothing ->
            throwE (Info, "There is no error to explain in this file.")
        Just docErrors ->
            return
                [ diagnosis_
                | err <- errParse docErrors ++ errTypes docErrors
                , Just diagnosis_ <- [explain maxOutputSize err]
                ]
    let overlaps wanted (Diagnosis _ (Just range_) _) =
            rangesOverlap (rangeFromJSON wanted) range_
        overlaps _ _ =
            False
        chosen = case wantedRange of
            Just wanted ->
                case filter (overlaps wanted) explanations of
                    (diagnosis_ : _) -> Just diagnosis_
                    [] -> listToMaybe explanations
            Nothing ->
                listToMaybe explanations
    explanation <- case chosen of
        Just diagnosis_ -> return diagnosis_
        Nothing -> throwE (Info, "There is no error to explain in this file.")
    -- The VS Code client on the winitzki-vscode-dhall-lsp-server branch
    -- serves `dhall-explain:?…` from memory via ExplainProvider.
    let body = diagnosis explanation
        _uri = Uri ("dhall-explain:?" <> Text.pack (URIEncode.encode (Text.unpack body)))
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
    :: EvaluateSettings
    -> TRequestMessage 'Method_WorkspaceExecuteCommand
    -> (Either a (Value |? Null) -> HandlerM b)
    -> HandlerM ()
executeNormalize settings request respond = do
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
    if Text.null (Text.strip selected)
        then throwE (Info, "The selection is empty, so there is nothing to normalize.")
        else return ()
    expr <- case parse selected of
        Right e -> return e
        Left err ->
            throwE
                ( Warning
                , "The selection was not normalized because it does not parse:\n"
                    <> parseErrorText err
                )
    ServerConfig { maxOutputSize, chosenCharacterSet } <- liftLSP LSP.getConfig
    let bytes = fromMaybe maxOutputSize givenBytes
        -- Same safety limits the editor used before the output cap became
        -- optional: 256MiB of allocation and 30 seconds.
        allocationBytes = 256 * 1024 * 1024 :: Int64
        timeoutMicros = 30 * 1000 * 1000
    -- Normalizing with the enclosing let bindings uses the current
    -- buffer even when the file does not type-check as a whole.
    scoped <- catchE
        (do  mExpr <- loadForTyping settings uri_
             case mExpr of
                 Nothing ->
                     return Nothing
                 Just typed -> do
                     let LSP.Types.Range (Position line_ col) _ = range_
                     return
                         (scopedNormalize
                             bytes
                             (fromIntegral line_, fromIntegral col)
                             selected
                             typed))
        (\_ -> return Nothing)
    outcome <- liftIO $ case scoped of
        Just (scopedNF, cut) ->
            Bounded.runLimited
                (Just allocationBytes)
                (Just timeoutMicros)
                (evaluate (force (fmap absurd scopedNF, cut)))
        Nothing ->
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
    cache <- readImportCache
    fileIdentifier <- fileIdentifierFromUri uri_
    fetched <- liftIO $
        loadCollected settings fileIdentifier (Core.Embed imp) cache negative
    let original = case fetched of
            Right (_, _, [], sources) ->
                listToMaybe
                    [ text
                    | Import.ResolvedImportSource { Import.resolvedSourceText = Just text } <-
                        Map.elems sources
                    ]
            _ ->
                Nothing
        body = case original of
            Just text -> text
            Nothing ->
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
textInRange txt (LSP.Types.Range (Position startLine startCol) (Position endLine endCol))
    | startLine > endLine = ""
    | otherwise =
        case drop (fromIntegral startLine) (Text.lines txt) of
            [] ->
                ""
            row : rest
                | startLine == endLine ->
                    clip row startCol endCol
                | otherwise ->
                    let middle = fromIntegral (endLine - startLine) - 1
                        lastRow = case drop middle rest of
                            next : _ -> clip next 0 endCol
                            [] -> ""
                    in Text.intercalate "\n"
                        (Text.drop (fromIntegral startCol) row : take middle rest ++ [lastRow])

clip :: Text -> LSP.Types.UInt -> LSP.Types.UInt -> Text
clip rowText from to =
    Text.take (max 0 (fromIntegral to - fromIntegral from))
        (Text.drop (fromIntegral from) rowText)

parseErrorText :: DhallError -> Text
parseErrorText err =
    Text.intercalate "\n" [ message | Diagnosis { diagnosis = message } <- diagnose err ]

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

-- | The client tells us its trace level here; there is nothing to adjust.
--   This handler is a stub to prevent `lsp:no handler for:` messages.
setTraceHandler :: Handlers HandlerM
setTraceHandler =
    LSP.notificationHandler SMethod_SetTrace \_ -> return ()

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
                            , snapPrefixNames = maybe [] snapPrefixNames previous
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
                            Just env -> do
                                outcome <- Exception.try $ LSP.runLspT env $
                                    State.evalStateT
                                        (Except.runExceptT (diagnoseDocument settings _uri txt))
                                        snapshot
                                case outcome of
                                    Left ex ->
                                        hPutStrLn stderr
                                            ("Warning: background analysis failed: "
                                                <> Import.plainShowImportError ex)
                                    Right _ ->
                                        return ()

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
        errorsRef <- use errors
        liftIO $ IORef.modifyIORef' errorsRef (Map.delete _uri)
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
handleErrorWithDefault respond _default action =
    MC.catch (catchE action handler) ioHandler
  where
    -- An unexpected exception must not kill the server: log it and answer
    -- the pending request with the default value.
    ioHandler ex
        | Just async <- fromException ex =
            MC.throwM (async :: SomeAsyncException)
        | otherwise = do
            let _type_ = MessageType_Error
                _message =
                    "Internal error while processing the request: "
                        <> Text.pack (Import.plainShowImportError ex)
            liftLSP $ LSP.sendNotification SMethod_WindowLogMessage LogMessageParams{..}
            respond (Right _default)

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
