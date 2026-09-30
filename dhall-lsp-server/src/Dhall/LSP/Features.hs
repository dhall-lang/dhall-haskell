{-| Navigation, refactors and import-source commands.

    These handlers are registered beside the original ones.  They read the
    current buffer, so they work from the last successful parse when the
    buffer currently has a syntax error.
-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Dhall.LSP.Features (featureHandlers) where

import Control.Lens (assign, toListOf, use, (^.))
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import System.Directory
    ( XdgDirectory (..)
    , createDirectoryIfMissing
    , getXdgDirectory
    , listDirectory
    )
import System.FilePath ((</>))
import Dhall (EvaluateSettings)
import Dhall.Core (Binding (..), Expr, Import)
import Dhall.Scope
    ( NameDecl (..)
    , ScopeFragment (..)
    , ScopeKind (..)
    , scopeFragments
    )
import Dhall.Parser (Src)
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types hiding (Range (..))
import Language.LSP.Server (Handlers)

import Dhall.LSP.Backend.Dhall (emptyCache, parse)
import Dhall.LSP.Backend.Diagnostics (Range (..), rangeFromDhall)
import Dhall.LSP.Backend.Formatting (formatExpr)
import Dhall.LSP.Handlers
    ( handleErrorWithDefault
    , liftLSP
    , rangeToJSON
    , readUri
    , textInRange
    )
import Dhall.LSP.State

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Dhall.Core as Core
import qualified Language.LSP.Protocol.Types as J
import qualified Language.LSP.Server as LSP

featureHandlers :: EvaluateSettings -> Handlers HandlerM
featureHandlers evalSettings = mconcat
    [ definitionHandler
    , referencesHandler
    , highlightHandler
    , renameHandler
    , symbolsHandler
    , foldingHandler
    , semanticTokensHandler
    , inlayHandler evalSettings
    , codeActionHandler evalSettings
    , watchedFilesHandler
    , importSourceHandler
    ]

posOf :: J.Position -> (Int, Int)
posOf J.Position { _line, _character } =
    (fromIntegral _line, fromIntegral _character)

srcContains :: Src -> (Int, Int) -> Bool
srcContains src (row, col) =
    let Range left right = rangeFromDhall src
    in left <= (row, col) && (row, col) <= right

srcToRange :: Src -> J.Range
srcToRange = rangeToJSON . rangeFromDhall

sites :: Text -> [ScopeFragment Import]
sites txt =
    case parse txt of
        Right expr -> scopeFragments expr
        Left _ -> []

-- | Text whose names match the request.
--
--   A syntax error has no names.  Use the last buffer that parsed.  Positions
--   from that text still match the editor where the edit did not shift them.
--   Rename keeps using the current buffer, because its edits have to land on
--   the text the editor has now.
sourceForNav :: J.Uri -> Text -> HandlerM Text
sourceForNav uri_ current =
    case parse current of
        Right _ ->
            return current
        Left _ -> do
            docsRef <- use documents
            snaps <- liftIO (readIORef docsRef)
            return $ case Map.lookup uri_ snaps >>= snapLastGood of
                Just good -> good
                Nothing -> current

declAt :: [ScopeFragment a] -> (Int, Int) -> Maybe NameDecl
declAt fragments pos = foldr pick Nothing fragments
  where
    pick (ScopeFragment src (NameUse decl)) acc
        | srcContains src pos = Just decl
        | otherwise = acc
    pick (ScopeFragment src (NameDeclaration decl)) acc
        | srcContains src pos = Just decl
        | otherwise = acc
    pick _ acc = acc

sameDecl :: NameDecl -> ScopeFragment a -> Bool
sameDecl wanted (ScopeFragment _ (NameUse decl)) = decl == wanted
sameDecl wanted (ScopeFragment _ (NameDeclaration decl)) = decl == wanted
sameDecl _ _ = False

declSrc :: NameDecl -> Src
declSrc (NameDecl src _ _) = src

boundValue :: Binding s a -> Expr s a
boundValue Binding { value = bound } = bound

locationOf :: J.Uri -> Src -> J.Location
locationOf docUri src = J.Location { _uri = docUri, _range = srcToRange src }

definitionHandler :: Handlers HandlerM
definitionHandler =
    LSP.requestHandler SMethod_TextDocumentDefinition \request respond ->
        handleErrorWithDefault respond (InR (InR J.Null)) do
            let docUri = request ^. params . textDocument . uri
                pos = posOf (request ^. params . position)
            txt <- sourceForNav docUri =<< readUri docUri
            case declAt (sites txt) pos of
                Nothing ->
                    respond (Right (InR (InR J.Null)))
                Just decl ->
                    respond (Right (InL (J.Definition (InL (locationOf docUri (declSrc decl))))))

referencesHandler :: Handlers HandlerM
referencesHandler =
    LSP.requestHandler SMethod_TextDocumentReferences \request respond ->
        handleErrorWithDefault respond (InR J.Null) do
            let docUri = request ^. params . textDocument . uri
                pos = posOf (request ^. params . position)
            txt <- sourceForNav docUri =<< readUri docUri
            let found = sites txt
            case declAt found pos of
                Nothing ->
                    respond (Right (InR J.Null))
                Just decl ->
                    let locations =
                            [ locationOf docUri src
                            | ScopeFragment src scopeKind <- found
                            , sameDecl decl (ScopeFragment src scopeKind)
                            ]
                    in respond (Right (InL locations))

highlightHandler :: Handlers HandlerM
highlightHandler =
    LSP.requestHandler SMethod_TextDocumentDocumentHighlight \request respond ->
        handleErrorWithDefault respond (InR J.Null) do
            let docUri = request ^. params . textDocument . uri
                pos = posOf (request ^. params . position)
            txt <- sourceForNav docUri =<< readUri docUri
            let found = sites txt
            case declAt found pos of
                Nothing ->
                    respond (Right (InR J.Null))
                Just decl ->
                    let highlights =
                            [ J.DocumentHighlight
                                { _range = srcToRange src
                                , _kind = Just J.DocumentHighlightKind_Read
                                }
                            | ScopeFragment src scopeKind <- found
                            , sameDecl decl (ScopeFragment src scopeKind)
                            ]
                    in respond (Right (InL highlights))

renameHandler :: Handlers HandlerM
renameHandler =
    LSP.requestHandler SMethod_TextDocumentRename \request respond ->
        handleErrorWithDefault respond (InR J.Null) do
            let docUri = request ^. params . textDocument . uri
                pos = posOf (request ^. params . position)
                replacement = request ^. params . newName
            txt <- readUri docUri
            let found = sites txt
            case declAt found pos of
                Nothing ->
                    respond (Right (InR J.Null))
                Just decl -> do
                    let renameEdits =
                            [ J.TextEdit
                                { _range = srcToRange src
                                , _newText = replacement
                                }
                            | ScopeFragment src scopeKind <- found
                            , sameDecl decl (ScopeFragment src scopeKind)
                            ]
                        _changes = Just (Map.singleton docUri renameEdits)
                        _documentChanges = Nothing
                        _changeAnnotations = Nothing
                    respond (Right (InL J.WorkspaceEdit {..}))

symbolsHandler :: Handlers HandlerM
symbolsHandler =
    LSP.requestHandler SMethod_TextDocumentDocumentSymbol \request respond ->
        handleErrorWithDefault respond (InR (InR J.Null)) do
            let docUri = request ^. params . textDocument . uri
            txt <- sourceForNav docUri =<< readUri docUri
            let infos =
                    [ J.SymbolInformation
                        { _name = boundName
                        , _kind = J.SymbolKind_Variable
                        , _tags = Nothing
                        , _deprecated = Nothing
                        , _location = locationOf docUri src
                        , _containerName = Nothing
                        }
                    | ScopeFragment _ (NameDeclaration (NameDecl src boundName _)) <- sites txt
                    ]
            respond (Right (InL infos))

foldingHandler :: Handlers HandlerM
foldingHandler =
    LSP.requestHandler SMethod_TextDocumentFoldingRange \request respond ->
        handleErrorWithDefault respond (InR J.Null) do
            let docUri = request ^. params . textDocument . uri
            txt <- sourceForNav docUri =<< readUri docUri
            case parse txt of
                Left _ ->
                    respond (Right (InR J.Null))
                Right expr ->
                    respond (Right (InL (foldRanges expr)))

foldRanges :: Expr Src a -> [J.FoldingRange]
foldRanges expr = go expr
  where
    go (Core.Note src (Core.Let binding body)) =
        rangeOf src : go (boundValue binding) ++ go body
    go (Core.Note src (Core.Lam _ _ body)) =
        rangeOf src : go body
    go (Core.Note _ e) =
        go e
    go e =
        concatMap go (toListOf Core.subExpressions e)

    rangeOf src =
        let J.Range (J.Position firstLine _) (J.Position lastLine _) = srcToRange src
        in J.FoldingRange
            { _startLine = firstLine
            , _startCharacter = Nothing
            , _endLine = lastLine
            , _endCharacter = Nothing
            , _kind = Just J.FoldingRangeKind_Region
            , _collapsedText = Nothing
            }

semanticTokensHandler :: Handlers HandlerM
semanticTokensHandler =
    LSP.requestHandler SMethod_TextDocumentSemanticTokensFull \request respond ->
        handleErrorWithDefault respond (InR J.Null) do
            let docUri = request ^. params . textDocument . uri
            txt <- sourceForNav docUri =<< readUri docUri
            let encoded = encodeNameTokens (sites txt)
                tokens = J.SemanticTokens { _resultId = Nothing, _data_ = encoded }
            respond (Right (InL tokens))

encodeNameTokens :: [ScopeFragment a] -> [J.UInt]
encodeNameTokens = snd . foldl step ((0, 0), [])
  where
    step ((prevLine, prevCol), acc) (ScopeFragment src _) =
        let J.Range (J.Position tokenLine tokenCol) (J.Position _ endCol) = srcToRange src
            lineDelta = tokenLine - prevLine
            deltaCol = if lineDelta == 0 then tokenCol - prevCol else tokenCol
            len = if endCol >= tokenCol then endCol - tokenCol else 1
            piece =
                [ fromIntegral lineDelta
                , fromIntegral deltaCol
                , fromIntegral len
                , 0
                , 0
                ]
        in ((tokenLine, tokenCol), acc ++ piece)

inlayHandler :: EvaluateSettings -> Handlers HandlerM
inlayHandler _settings =
    LSP.requestHandler SMethod_TextDocumentInlayHint \request respond ->
        handleErrorWithDefault respond (InR J.Null) do
            let docUri = request ^. params . textDocument . uri
            txt <- readUri docUri
            case parse txt of
                Left _ ->
                    respond (Right (InR J.Null))
                Right expr ->
                    respond (Right (InL (inlayHints expr)))

inlayHints :: Expr Src a -> [J.InlayHint]
inlayHints = go
  where
    go (Core.Note _ (Core.Let binding body)) =
        hint binding ++ go (boundValue binding) ++ go body
    go (Core.Note _ e) =
        go e
    go e =
        concatMap go (toListOf Core.subExpressions e)

    hint Core.Binding { Core.annotation = Just _ } = []
    hint Core.Binding
        { Core.bindingSrc1 = Just src
        , Core.variable = varName
        } =
        let J.Range _ (J.Position hintLine col) = srcToRange src
        in  [ J.InlayHint
                { _position = J.Position hintLine col
                , _label = InL (": " <> varName)
                , _kind = Just J.InlayHintKind_Type
                , _textEdits = Nothing
                , _tooltip = Nothing
                , _paddingLeft = Just True
                , _paddingRight = Nothing
                , _data_ = Nothing
                }
            ]
    hint _ = []

codeActionHandler :: EvaluateSettings -> Handlers HandlerM
codeActionHandler evalSettings =
    LSP.requestHandler SMethod_TextDocumentCodeAction \request respond ->
        handleErrorWithDefault respond (InR J.Null) do
            let docUri = request ^. params . textDocument . uri
                selected = request ^. params . range
            txt <- readUri docUri
            ServerConfig { maxOutputSize, chosenCharacterSet } <- liftLSP LSP.getConfig
            let normalize = J.CodeAction
                    { _title = "Normalize selection"
                    , _kind = Just J.CodeActionKind_RefactorRewrite
                    , _diagnostics = Nothing
                    , _isPreferred = Nothing
                    , _disabled = Nothing
                    , _edit = Nothing
                    , _command = Just J.Command
                        { _title = "Normalize selection"
                        , _command = "dhall.server.normalize"
                        , _arguments = Just
                            [ Aeson.toJSON docUri
                            , Aeson.toJSON selected
                            , Aeson.toJSON maxOutputSize
                            ]
                        }
                    , _data_ = Nothing
                    }
            let selectedText = textInRange txt selected
                explain = J.CodeAction
                    { _title = "Explain error"
                    , _kind = Just J.CodeActionKind_QuickFix
                    , _diagnostics = Nothing
                    , _isPreferred = Nothing
                    , _disabled = Nothing
                    , _edit = Nothing
                    , _command = Just J.Command
                        { _title = "Explain error"
                        , _command = "dhall.server.explain"
                        , _arguments = Just [Aeson.toJSON docUri]
                        }
                    , _data_ = Nothing
                    }
                extracted = "let extracted = " <> selectedText <> " in extracted"
                _range = selected
                _newText = extracted
                extract = J.CodeAction
                    { _title = "Extract let"
                    , _kind = Just J.CodeActionKind_RefactorExtract
                    , _diagnostics = Nothing
                    , _isPreferred = Nothing
                    , _disabled = Nothing
                    , _edit = Just J.WorkspaceEdit
                        { _changes = Just (Map.singleton docUri [J.TextEdit { _range, _newText }])
                        , _documentChanges = Nothing
                        , _changeAnnotations = Nothing
                        }
                    , _command = Nothing
                    , _data_ = Nothing
                    }
                alphaActions = case parse selectedText of
                    Left _ -> []
                    Right expr ->
                        let _newText' = formatExpr
                                chosenCharacterSet
                                (Core.alphaNormalize expr)
                            action = J.CodeAction
                                { _title = "Alpha-normalize selection"
                                , _kind = Just J.CodeActionKind_RefactorRewrite
                                , _diagnostics = Nothing
                                , _isPreferred = Nothing
                                , _disabled = Nothing
                                , _edit = Just J.WorkspaceEdit
                                    { _changes = Just
                                        (Map.singleton docUri [J.TextEdit { _range, _newText = _newText' }])
                                    , _documentChanges = Nothing
                                    , _changeAnnotations = Nothing
                                    }
                                , _command = Nothing
                                , _data_ = Nothing
                                }
                        in [InR action]
            let _ = evalSettings
            respond (Right (InL (InR normalize : InR explain : InR extract : alphaActions)))

watchedFilesHandler :: Handlers HandlerM
watchedFilesHandler =
    LSP.notificationHandler SMethod_WorkspaceDidChangeWatchedFiles \_ -> do
        -- A file outside the editor changed. Drop cached imports so the next
        -- analysis reads them again.
        assign importCache emptyCache

-- | `dhall/importSource`: contents of a decoded cached import, or a directory
--   listing of the on-disk mirror.  Editors that do not opt into
--   `dhall-import:` open that mirror as a `file://` URI instead.
importSourceHandler :: Handlers HandlerM
importSourceHandler =
    LSP.requestHandler (SMethod_CustomMethod (Proxy @"dhall/importSource")) \request respond ->
        handleErrorWithDefault respond Aeson.Null do
            let payload = request ^. params
            dir <- liftIO (getXdgDirectory XdgCache ("dhall-lsp" </> "sources"))
            liftIO (createDirectoryIfMissing True dir)
            entries <- liftIO (listDirectory dir)
            let wanted = requestedPath payload
                safe = case wanted of
                    Just entryName
                        | '/' `notElem` entryName
                        , entryName /= ".."
                        , not (null entryName) ->
                            Just entryName
                    _ -> Nothing
            body <- case safe of
                Nothing ->
                    return ("" :: Text)
                Just entryName ->
                    liftIO (Text.pack <$> readFile (dir </> entryName))
            respond
                (Right
                    (Aeson.object
                        [ "root" Aeson..= dir
                        , "entries" Aeson..= entries
                        , "text" Aeson..= body
                        ]
                    ))

requestedPath :: Aeson.Value -> Maybe String
requestedPath (Aeson.String pathText) = Just (Text.unpack pathText)
requestedPath _ = Nothing
