{-| Navigation, refactors and import-source commands.

    These handlers are registered beside the original ones.  They read the
    current buffer, so they work from the last successful parse when the
    buffer currently has a syntax error.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Dhall.LSP.Features (featureHandlers) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Lens (assign, toListOf, universeOf, use, (^.))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.IORef (modifyIORef', readIORef)
import Data.List (foldl', sortOn)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (Down (..))
#if !MIN_VERSION_lsp_types(2,2,0)
import Data.Row (Label (..), Rec, type (.==), (.==))
#endif
import Data.Void (Void)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import System.Directory
    ( XdgDirectory (..)
    , createDirectoryIfMissing
    , getXdgDirectory
    , listDirectory
    )
import System.FilePath (normalise, takeDirectory, takeFileName, (</>))
import Dhall (EvaluateSettings)
import Dhall.Import (localToPath)
import Dhall.Core
    ( Binding (..)
    , Expr
    , FieldSelection (..)
    , FunctionBinding (..)
    , freeIn
    , File (..)
    , FilePrefix (..)
    , Import (..)
    , ImportHashed (..)
    , ImportType (..)
    , RecordField (..)
    , URL (..)
    , Var (..)
    , Directory (..)
    )
import Dhall.Scope
    ( NameDecl (..)
    , ScopeFragment (..)
    , ScopeKind (..)
    , makeSrcForLabel
    , scopeFragments
    )
import Dhall.Parser (Src (..))
import Language.LSP.Protocol.Lens hiding (length)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types hiding (Range (..))
import Language.LSP.Server (Handlers)

import Dhall.LSP.Backend.Dhall
    ( FileIdentifier
    , emptyCache
    , identifierChained
    , importTextKey
    , load
    , parse
    , typecheck
    , WellTyped
    )
import Dhall.LSP.Backend.Typing (letTypes)
import Dhall.LSP.Backend.Diagnostics
    ( Diagnosis (Diagnosis)
    , Range (..)
    , explain
    , positionToOffset
    , rangeFromDhall
    )
import Dhall.LSP.Backend.Linting (unusedBindingEdits)
import qualified Dhall.Bounded as Bounded
import qualified Dhall.Pretty as Pretty
import Dhall.LSP.Handlers
    ( fileIdentifierFromUri
    , handleErrorWithDefault
    , liftLSP
    , rangeToJSON
    , readUri
    , textInRange
    )
import Dhall.LSP.State

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Text.Megaparsec.Pos as Pos
import qualified Data.Text.IO as Text.IO
import qualified Dhall.Core as Core
import qualified Dhall.Import as Import
import qualified Dhall.Map as DMap
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

locationOf :: J.Uri -> Src -> J.Location
locationOf docUri src = J.Location { _uri = docUri, _range = srcToRange src }

definitionHandler :: Handlers HandlerM
definitionHandler =
    LSP.requestHandler SMethod_TextDocumentDefinition \request respond ->
        handleErrorWithDefault respond (InR (InR J.Null)) do
            let docUri = request ^. params . textDocument . uri
                pos = posOf (request ^. params . position)
            txt <- sourceForNav docUri =<< readUri docUri
            imported <- case parse txt of
                Right expr ->
                    case importJump expr pos of
                        Nothing ->
                            return Nothing
                        Just (imp, path) ->
                            openImported docUri imp path
                Left _ ->
                    return Nothing
            case imported of
                Just loc ->
                    respond (Right (InL (J.Definition (InL loc))))
                Nothing ->
                    case declAt (sites txt) pos of
                        Nothing ->
                            respond (Right (InR (InR J.Null)))
                        Just decl ->
                            respond (Right (InL (J.Definition (InL (locationOf docUri (declSrc decl))))))

-- | A field projection whose root is an import, and the labels from that
--   import out to the field under the cursor.
importJump :: Expr Src Import -> (Int, Int) -> Maybe (Import, [Text])
importJump root pos = go [] root
  where
    go ctx (Core.Note _ expr) =
        go ctx expr
    go ctx (Core.Let Binding { Core.variable = boundName, Core.annotation = ann, Core.value = letValue } body) =
        let inAnn = case ann of
                Just (_, typ) -> go ctx typ
                Nothing -> Nothing
        in inAnn <|> go ctx letValue <|> go ((boundName, letValue) : ctx) body
    go ctx (Core.Field base (FieldSelection (Just Src { srcEnd = labelStart }) fieldLabel (Just Src { srcStart = labelEnd }))) =
        let labelSrc = makeSrcForLabel labelStart labelEnd fieldLabel
        in if srcContains labelSrc pos
            then do
                (imp, path) <- resolve ctx base
                return (imp, path ++ [fieldLabel])
            else go ctx base
    go ctx expr =
        foldr (\child acc -> go ctx child <|> acc) Nothing (toListOf Core.subExpressions expr)

    resolve ctx (Core.Note _ expr) =
        resolve ctx expr
    resolve _ (Core.Embed imp) =
        Just (imp, [])
    resolve ctx (Core.Annot expr _) =
        resolve ctx expr
    resolve ctx (Core.Field expr (FieldSelection _ fieldLabel _)) = do
        (imp, path) <- resolve ctx expr
        return (imp, path ++ [fieldLabel])
    resolve ctx (Core.Var (V varName index)) =
        resolve ctx =<< lookupBind varName index ctx
    resolve _ _ =
        Nothing

    lookupBind varName index ctx =
        case [letValue | (bound, letValue) <- ctx, bound == varName] of
            values | index < length values ->
                Just (values !! index)
            _ ->
                Nothing

-- | Where a field path is bound inside an imported file.
data ImportedField = Landed Src | Deeper Import [Text]

locateField :: Expr Src Import -> [Text] -> Maybe ImportedField
locateField expr path = go [] expr path
  where
    go ctx (Core.Note _ inner) remaining =
        go ctx inner remaining
    go ctx (Core.Let Binding { Core.bindingSrc0 = before, Core.variable = boundName, Core.bindingSrc1 = after, Core.value = letValue } body) remaining =
        go ((boundName, (letValue, binderSrc before after boundName)) : ctx) body remaining
    go ctx (Core.Annot inner _) remaining =
        go ctx inner remaining
    go ctx (Core.RecordLit fields) (fieldLabel : rest) =
        case DMap.lookup fieldLabel fields of
            Just RecordField { recordFieldSrc0 = Just Src { srcEnd = labelStart }, recordFieldValue = fieldValue, recordFieldSrc1 = Just Src { srcStart = labelEnd } } ->
                let keySrc = makeSrcForLabel labelStart labelEnd fieldLabel
                    landed = case follow ctx fieldValue of
                        Just found ->
                            found
                        Nothing ->
                            Landed keySrc
                in if null rest
                    then Just landed
                    else case denote fieldValue of
                        Core.Embed imp ->
                            Just (Deeper imp rest)
                        _ ->
                            go ctx fieldValue rest <|> Just (Landed keySrc)
            _ ->
                Nothing
    go ctx (Core.Var (V varName index)) remaining =
        case lookupBind varName index ctx of
            Just (letValue, decl)
                | null remaining ->
                    Just (Landed decl)
                | otherwise ->
                    go ctx letValue remaining
            Nothing ->
                Nothing
    -- The fetched text is itself an import, such as an environment variable
    -- whose value is `./lib.dhall`.  Follow that import; if its source was
    -- not fetched, the caller opens this text instead.
    go _ (Core.Embed imp) remaining
        | not (null remaining) =
            Just (Deeper imp remaining)
    go _ _ _ =
        Nothing

    follow ctx followed =
        case denote followed of
            Core.Var (V varName index) ->
                Landed . snd <$> lookupBind varName index ctx
            _ ->
                Nothing

    lookupBind varName index ctx =
        case [found | (bound, found) <- ctx, bound == varName] of
            values | index < length values ->
                Just (values !! index)
            _ ->
                Nothing

    binderSrc (Just Src { srcEnd = labelStart }) (Just Src { srcStart = labelEnd }) boundName =
        makeSrcForLabel labelStart labelEnd boundName
    binderSrc _ _ boundName =
        Src (Pos.initialPos "<binder>") (Pos.initialPos "<binder>") boundName

    denote (Core.Note _ inner) = denote inner
    denote other = other

openImported :: J.Uri -> Import -> [Text] -> HandlerM (Maybe J.Location)
openImported docUri imp path = do
    parentId <- fileIdentifierFromUri docUri
    bodiesRef <- use importBodies
    bodies <- liftIO (readIORef bodiesRef)
    let parentPath = case uriToFilePath docUri of
            Just file ->
                takeDirectory file
            Nothing ->
                "."
    openFrom parentPath parentId imp path bodies

openFrom
    :: FilePath
    -> FileIdentifier
    -> Import
    -> [Text]
    -> Map.Map Text Text
    -> HandlerM (Maybe J.Location)
openFrom parentPath parentId imp path bodies =
    let found =
            listToMaybe
                [ bodyText
                | key <- importKeys parentPath parentId imp
                , Just bodyText <- [Map.lookup key bodies]
                ]
    in case found of
        Nothing ->
            case imp of
                Import (ImportHashed (Just hash) _) _ -> do
                    decoded <- liftIO (Import.decodeSemanticCache hash)
                    case decoded of
                        Nothing ->
                            return Nothing
                        Just expr ->
                            showImported parentPath parentId imp (renderDecoded expr) path bodies
                _ ->
                    return Nothing
        Just bodyText ->
            showImported parentPath parentId imp bodyText path bodies

showImported
    :: FilePath
    -> FileIdentifier
    -> Import
    -> Text
    -> [Text]
    -> Map.Map Text Text
    -> HandlerM (Maybe J.Location)
showImported parentPath parentId imp bodyText path bodies =
    case parse bodyText of
        Left _ ->
            return Nothing
        Right expr ->
            case locateField expr path of
                Just (Landed src) ->
                    Just <$> openLocated parentPath parentId imp bodyText src
                Just (Deeper nested rest) -> do
                    nestedLoc <- openFrom parentPath parentId nested rest bodies
                    case nestedLoc of
                        Just loc ->
                            return (Just loc)
                        Nothing ->
                            Just <$> openLocated parentPath parentId imp bodyText (startSrc expr)
                Nothing ->
                    return Nothing

-- | Keys that may name this import in 'importBodies'.
importKeys :: FilePath -> FileIdentifier -> Import -> [Text]
importKeys parentPath parentId imp =
    importTextKey parentId imp
        : Core.pretty imp
        : localKey parentPath imp

localKey :: FilePath -> Import -> [Text]
localKey parentPath (Import (ImportHashed _ (Local prefix file)) _) =
    [ Text.pack (normalise (prefixPath prefix </> renderFile file))
    , Text.pack (takeFileName (renderFile file))
    ]
  where
    prefixPath Absolute =
        "/"
    prefixPath Home =
        parentPath
    prefixPath Here =
        parentPath
    prefixPath Parent =
        parentPath </> ".."
localKey _ (Import (ImportHashed _ (Remote url)) _) =
    [Core.pretty (url { headers = Nothing })]
localKey _ (Import (ImportHashed _ (Env envName)) _) =
    ["env:" <> envName]
localKey _ _ =
    []

renderFile :: File -> FilePath
renderFile (File (Directory components) file) =
    foldl (</>) "" (map Text.unpack (reverse (file : components)))

startSrc :: Expr Src Import -> Src
startSrc (Core.Note src _) =
    src
startSrc _ =
    Src (Pos.initialPos "<import>") (Pos.initialPos "<import>") ""

-- | A local import opens the file itself.  Anything else opens a read-only
--   'dhall-import:' view of the fetched text.  The cache file stays, because
--   the editor reads it through 'dhall/importSource'.
openLocated
    :: FilePath
    -> FileIdentifier
    -> Import
    -> Text
    -> Src
    -> HandlerM J.Location
openLocated parentPath parentId imp bodyText src = do
    mLocal <- liftIO (localImportFile parentPath imp)
    case mLocal of
        Just path ->
            return (locationOf (filePathToUri path) src)
        Nothing ->
            publishImport parentPath parentId imp bodyText src

localImportFile :: FilePath -> Import -> IO (Maybe FilePath)
localImportFile base (Import (ImportHashed _ (Local prefix file)) _) = do
    rel <- localToPath prefix file
    return (Just (normalise (base </> rel)))
localImportFile _ _ =
    return Nothing

publishImport
    :: FilePath
    -> FileIdentifier
    -> Import
    -> Text
    -> Src
    -> HandlerM J.Location
publishImport parentPath parentId imp bodyText src = do
    let key = importTextKey parentId imp
    dir <- liftIO (getXdgDirectory XdgCache ("dhall-lsp" </> "sources"))
    liftIO (createDirectoryIfMissing True dir)
    let mirrorName = fileName key
        mirrorPath = dir </> mirrorName
    liftIO (Text.IO.writeFile mirrorPath bodyText)
    chainsRef <- use importChains
    originsRef <- use mirrorOrigins
    chains <- liftIO (readIORef chainsRef)
    let lookedUp =
            listToMaybe
                [ chained
                | candidate <- importKeys parentPath parentId imp
                , Just chained <- [Map.lookup candidate chains]
                ]
        -- `env:` and `missing` do not keep the directory of the file that
        -- imported them, so a relative path in the mirror would resolve from
        -- the server's working directory.  Keep that file as the origin.
        origin = case imp of
            Import (ImportHashed _ (Env _)) _ ->
                Just (identifierChained parentId)
            Import (ImportHashed _ Missing) _ ->
                Just (identifierChained parentId)
            _ ->
                lookedUp
    case origin of
        Just chained ->
            liftIO $
                modifyIORef' originsRef $
                    Map.insert mirrorName chained . Map.insert mirrorPath chained
        Nothing ->
            return ()
    return (locationOf (J.Uri ("dhall-import:///" <> Text.pack mirrorName)) src)

renderDecoded :: Core.Expr Void Void -> Text
renderDecoded expr =
    let noted = Core.renote expr :: Expr Src Void
        doc = Pretty.prettyCharacterSet Pretty.Unicode noted
    in case Bounded.prettyBounded defaultOutputBytes doc of
        Bounded.Complete rendered ->
            rendered
        Bounded.Truncated rendered ->
            rendered

fileName :: Text -> FilePath
fileName key =
    show (foldl (\n c -> n * 33 + fromEnum c) (5381 :: Int) (Text.unpack key))
        ++ ".dhall"

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
foldRanges expr = dedupe (go expr)
  where
    go (Core.Note src e) =
        [rangeOf src | spansLines src, foldable (Core.shallowDenote e), not (tighter e)]
            ++ childNodes e
    go e =
        childNodes e

    childNodes (Core.Note src e) =
        go (Core.Note src e)
    childNodes e =
        concatMap childNodes (toListOf Core.subExpressions e)

    -- A let value is often wrapped in a second note that runs up to the next
    -- binding.  Fold the inner note, which is the expression itself.
    tighter inner =
        case inner of
            Core.Note src' _ ->
                spansLines src' && foldable (Core.shallowDenote inner)
            _ ->
                False

    spansLines src =
        let J.Range (J.Position firstLine _) (J.Position lastLine _) = srcToRange src
        in firstLine /= lastLine

    foldable (Core.Let _ _) = True
    foldable (Core.Lam _ _ _) = True
    foldable (Core.Pi _ _ _ _) = True
    foldable (Core.Record _) = True
    foldable (Core.RecordLit _) = True
    foldable (Core.Union _) = True
    foldable (Core.BoolIf _ _ _) = True
    foldable (Core.Merge _ _ _) = True
    foldable (Core.TextLit _) = True
    foldable (Core.ListLit _ xs) = not (null (toList xs))
    foldable _ = False

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

    dedupe = snd . foldl' keep (Set.empty, [])
    keep (seen, acc) foldRange
        | Set.member (foldRange ^. startLine) seen =
            (seen, acc)
        | otherwise =
            (Set.insert (foldRange ^. startLine) seen, acc ++ [foldRange])

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
inlayHandler evalSettings =
    LSP.requestHandler SMethod_TextDocumentInlayHint \request respond ->
        handleErrorWithDefault respond (InR J.Null) do
            let docUri = request ^. params . textDocument . uri
                wanted = request ^. params . range
            txt <- readUri docUri
            fileIdentifier <- fileIdentifierFromUri docUri
            cache <- use importCache
            case parse txt of
                Left _ ->
                    respond (Right (InR J.Null))
                Right parsed -> do
                    loaded <- liftIO $ load evalSettings fileIdentifier parsed cache
                    case loaded of
                        Left _ ->
                            respond (Right (InR J.Null))
                        Right (cache', expr) -> do
                            assign importCache cache'
                            case typecheck evalSettings expr of
                                Left _ ->
                                    respond (Right (InR J.Null))
                                Right (wt, _) -> do
                                    ServerConfig { maxOutputSize } <- liftLSP LSP.getConfig
                                    let hints =
                                            filter (hintIn wanted) (inlayHints maxOutputSize wt)
                                    respond (Right (InL hints))

hintIn :: J.Range -> J.InlayHint -> Bool
hintIn (J.Range startPos endPos) hint =
    let pos = hint ^. position
    in startPos <= pos && pos <= endPos

inlayHints :: Int -> WellTyped -> [J.InlayHint]
inlayHints limit wt =
    [ hint limit src ty | (src, ty) <- letTypes wt ]
  where
    hint limit_ src ty =
        let J.Range _ endPos = srcToRange src
            doc = Pretty.prettyCharacterSet Pretty.Unicode ty
            rendered = case Bounded.prettyBounded limit_ doc of
                Bounded.Complete typeText -> typeText
                Bounded.Truncated typeText -> typeText
            short =
                if Text.length rendered <= 60
                    then rendered
                    else Text.take 59 rendered <> "…"
            typeEdits = case Bounded.prettyBounded limit_ doc of
                Bounded.Complete typeText ->
                    Just
                        [ J.TextEdit
                            { _range = J.Range endPos endPos
                            , _newText = " : " <> typeText
                            }
                        ]
                Bounded.Truncated _ ->
                    Nothing
        in J.InlayHint
            { _position = endPos
            , _label = InL (": " <> short)
            , _kind = Just J.InlayHintKind_Type
            , _textEdits = typeEdits
            , _tooltip = Just (InL rendered)
            , _paddingLeft = Just True
            , _paddingRight = Nothing
            , _data_ = Nothing
            }

-- | Reorder and drop unused import bindings in the top-level @let@ chain.
--
--   'Nothing' means there is nothing to do, or an import binding does not
--   occupy whole lines.  'Left' is a refusal the editor can show.
organizeImports :: Text -> Maybe (Either Text Text)
organizeImports txt = do
    expr <- either (const Nothing) Just (parse txt)
    let blocks = topLetBlocks expr
    guard (any blockImport blocks)
    let names = map blockName blocks
    if length names /= length (Set.fromList names)
        then return (Left "A top-level name is bound twice.")
        else if any indexed (universeOf Core.subExpressions expr)
            then return (Left "This file uses a variable of the form name@n.")
            else if not (all wholeLines blocks)
                then Nothing
                else do
                    let ls = Text.lines txt
                        firstLine = blockStart (head blocks)
                        lastLine = blockEnd (last blocks)
                        prefix = take firstLine ls
                        suffix = drop lastLine ls
                        imports =
                            sortOn blockName
                                [ block | block <- blocks, blockImport block, blockUsed block ]
                        others = [ block | block <- blocks, not (blockImport block) ]
                        chunk block =
                            take (blockEnd block - blockStart block) (drop (blockStart block) ls)
                        rebuilt =
                            Text.unlines
                                (prefix ++ concatMap chunk (imports ++ others) ++ suffix)
                    guard (rebuilt /= txt)
                    return (Right rebuilt)
  where
    indexed (Core.Var (V _ n)) = n > 0
    indexed _ = False

    -- An import binding occupies whole lines when `let` is at column 0 and
    -- the following expression starts on a later line.  The body after `in`
    -- is often indented, so its column is not 0.
    wholeLines block =
        not (blockImport block)
            || ( blockCol block == 0
                    && (blockNextCol block == 0 || blockStart block < blockEnd block)
               )

data LetBlock = LetBlock
    { blockName :: Text
    , blockImport :: Bool
    , blockUsed :: Bool
    , blockStart :: Int
    , blockEnd :: Int
    , blockCol :: Int
    , blockNextCol :: Int
    }

topLetBlocks :: Expr Src Import -> [LetBlock]
topLetBlocks expr =
    case collect expr of
        ([], _) ->
            []
        (bindings, bodyPos) ->
            let nextStarts = map (\(bindingStart, _, _) -> bindingStart) (tail bindings) ++ [bodyPos]
            in zipWith block bindings nextStarts
  where
    -- The first @let@ is wrapped in a note that starts at @let@.  Later
    -- bindings in the same chain are not, so their start is three characters
    -- before the whitespace that follows @let@.
    collect (Core.Note src (Core.Let binding body)) =
        let (rest, endPos) = collect body
            Range startPos _ = rangeFromDhall src
        in ((startPos, binding, body) : rest, endPos)
    collect (Core.Let binding body) =
        let (rest, endPos) = collect body
        in ( maybe rest (\bindingStart -> (bindingStart, binding, body) : rest) (keywordPos binding)
           , endPos
           )
    collect (Core.Note src _) =
        ([], rangeStart src)
    collect _ =
        ([], (0, 0))

    keywordPos binding = do
        Src { srcStart = keywordEnd } <- Core.bindingSrc0 binding
        let Range (line_, col) _ = rangeFromDhall (Src keywordEnd keywordEnd "")
        return (line_, max 0 (col - 3))

    rangeStart src =
        let Range startPos _ = rangeFromDhall src in startPos

    block ((fromLine, startCol), binding, body) (toLine, endCol) =
        let name_ = Core.variable binding
        in LetBlock
            { blockName = name_
            , blockImport = importExpr (Core.value binding)
            , blockUsed = freeIn (V name_ 0) body
            , blockStart = fromLine
            , blockEnd = toLine
            , blockCol = startCol
            , blockNextCol = endCol
            }

importExpr :: Expr Src Import -> Bool
importExpr (Core.Note _ expr) = importExpr expr
importExpr (Core.Embed _) = True
importExpr _ = False

-- | Inline the @let@ binder under the selection.
--
--   'Nothing' means the selection is not a @let@ binder.  'Left' is a refusal.
inlineLet :: Text -> J.Range -> Maybe (Either Text Text)
inlineLet txt selected = do
    expr <- either (const Nothing) Just (parse txt)
    (letSrc, binding, body) <- findLetBinder (selectionStart selected) expr
    let name_ = Core.variable binding
        letValue = Core.value binding
    binderSrc <- letName binding
    if containsAssert letValue
        then return (Left "The binding contains an assert.")
        else do
            let fragments = scopeFragments expr
                uses =
                    [ src
                    | ScopeFragment src (NameUse (NameDecl boundSrc usedName _)) <- fragments
                    , boundSrc == binderSrc
                    , usedName == name_
                    ]
                indexed =
                    any (indexedUse name_ body) fragments
            if indexed
                then return (Left "The body uses a variable of the form name@n.")
                else if any (captures letValue body) uses
                    then return (Left "Inlining would capture a variable.")
                    else do
                        bodySrc <- case body of
                            Core.Note src _ -> Just src
                            _ -> Nothing
                        let Range prefixStart _ = rangeFromDhall letSrc
                            Range prefixEnd _ = rangeFromDhall bodySrc
                            valueText = Text.strip $ case letValue of
                                Core.Note (Src _ _ slice) _ -> slice
                                _ -> Core.pretty letValue
                            wrapped = parenthesize valueText
                            replacements =
                                (Range prefixStart prefixEnd, "")
                                    : [ (rangeFromDhall src, wrapped) | src <- uses ]
                        return (Right (applyTextEdits txt replacements))

selectionStart :: J.Range -> (Int, Int)
selectionStart (J.Range (J.Position lineNo col) _) =
    (fromIntegral lineNo, fromIntegral col)

-- | The binder name, between the whitespace after @let@ and the whitespace
--   before @:@ or @=@.
letName :: Binding Src Import -> Maybe Src
letName
    Binding
        { bindingSrc0 = Just Src { srcEnd = nameStart }
        , bindingSrc1 = Just Src { srcStart = nameEnd }
        , variable = name_
        } =
        Just (makeSrcForLabel nameStart nameEnd name_)
letName _ =
    Nothing

findLetBinder
    :: (Int, Int)
    -> Expr Src Import
    -> Maybe (Src, Binding Src Import, Expr Src Import)
findLetBinder pos (Core.Note src (Core.Let binding body))
    | Just nameSrc <- letName binding
    , srcContains nameSrc pos =
        Just (src, binding, body)
    | otherwise =
        findLetBinder pos (Core.value binding) <|> findLetBinder pos body
findLetBinder pos (Core.Note _ expr) =
    findLetBinder pos expr
findLetBinder pos (Core.Let binding body)
    | Just nameSrc <- letName binding
    , srcContains nameSrc pos =
        Just (fromMaybe nameSrc (Core.bindingSrc0 binding), binding, body)
    | otherwise =
        findLetBinder pos (Core.value binding) <|> findLetBinder pos body
findLetBinder pos expr =
    listToMaybe
        (mapMaybe (findLetBinder pos) (toListOf Core.subExpressions expr))

containsAssert :: Expr s a -> Bool
containsAssert (Core.Assert _) = True
containsAssert (Core.Note _ expr) = containsAssert expr
containsAssert expr = any containsAssert (toListOf Core.subExpressions expr)

indexedUse :: Text -> Expr Src Import -> ScopeFragment Import -> Bool
indexedUse name_ body (ScopeFragment src _) =
    case body of
        Core.Note bodySrc _ ->
            rangeContains bodySrc src
                && case Text.stripPrefix (name_ <> "@") (srcSlice src) of
                    Just rest ->
                        case reads (Text.unpack rest) of
                            [(n, "")] -> n > (0 :: Int)
                            _ -> False
                    Nothing ->
                        False
        _ ->
            False

captures :: Expr Src Import -> Expr Src Import -> Src -> Bool
captures letValue body useSrc =
    let Range left _ = rangeFromDhall useSrc
    in not (Set.null (Set.intersection (freeNames letValue) (enclosed left body)))

freeNames :: Expr s a -> Set.Set Text
freeNames = go Map.empty
  where
    go counts (Core.Note _ expr) =
        go counts expr
    go counts (Core.Var (V name_ index))
        | index >= Map.findWithDefault 0 name_ counts =
            Set.singleton name_
        | otherwise =
            Set.empty
    go counts (Core.Lam _ FunctionBinding { functionBindingVariable = x, functionBindingAnnotation = ann } body) =
        go counts ann <> go (Map.insertWith (+) x 1 counts) body
    go counts (Core.Pi _ x ann body) =
        go counts ann <> go (Map.insertWith (+) x 1 counts) body
    go counts (Core.Let Binding { variable = x, annotation = ann, value = bound } body) =
        goAnn counts ann <> go counts bound <> go (Map.insertWith (+) x 1 counts) body
    go counts expr =
        foldMap (go counts) (toListOf Core.subExpressions expr)

    goAnn _ Nothing = Set.empty
    goAnn counts (Just (_, expr)) = go counts expr

enclosed :: (Int, Int) -> Expr Src a -> Set.Set Text
enclosed pos (Core.Note _ (Core.Lam _ FunctionBinding { functionBindingVariable = x, functionBindingAnnotation = ann } body))
    | exprContains pos body =
        Set.insert x (enclosed pos body) <> enclosed pos ann
    | otherwise =
        enclosed pos ann
enclosed pos (Core.Note _ (Core.Pi _ x ann body))
    | exprContains pos body =
        Set.insert x (enclosed pos body)
    | otherwise =
        enclosed pos ann
enclosed pos (Core.Note _ (Core.Let Binding { variable = x, annotation = ann, value = bound } body))
    | exprContains pos body =
        Set.insert x (enclosed pos body)
    | otherwise =
        enclosedAnn pos ann <> enclosed pos bound
enclosed pos (Core.Note _ expr) =
    enclosed pos expr
enclosed pos expr =
    foldMap (enclosed pos) (toListOf Core.subExpressions expr)

enclosedAnn :: (Int, Int) -> Maybe (Maybe Src, Expr Src a) -> Set.Set Text
enclosedAnn _ Nothing = Set.empty
enclosedAnn pos (Just (_, expr)) = enclosed pos expr

exprContains :: (Int, Int) -> Expr Src a -> Bool
exprContains pos (Core.Note src _) = srcContains src pos
exprContains _ _ = False

rangeContains :: Src -> Src -> Bool
rangeContains outer inner =
    let Range left right = rangeFromDhall outer
        Range innerLeft innerRight = rangeFromDhall inner
    in left <= innerLeft && innerRight <= right

srcSlice :: Src -> Text
srcSlice (Src _ _ slice) = slice

parenthesize :: Text -> Text
parenthesize valueText
    | Text.null valueText = valueText
    | Text.head valueText `elem` ['(', '{', '[', '<', '"'] = valueText
    | Text.any (\c -> c == ' ' || c == '\n') valueText = "(" <> valueText <> ")"
    | otherwise = valueText

applyTextEdits :: Text -> [(Range, Text)] -> Text
applyTextEdits txt replacements =
    foldl' apply txt (sortOn (Down . startOffset) located)
  where
    located =
        [ (fromOff, toOff, new)
        | (range_, new) <- replacements
        , let Range left right = range_
        , let fromOff = positionToOffset txt left
        , let toOff = positionToOffset txt right
        ]
    startOffset (fromOff, _, _) = fromOff
    apply acc (fromOff, toOff, new) =
        Text.take fromOff acc <> new <> Text.drop toOff acc

codeActionHandler :: EvaluateSettings -> Handlers HandlerM
codeActionHandler _evalSettings =
    LSP.requestHandler SMethod_TextDocumentCodeAction \request respond ->
        handleErrorWithDefault respond (InR J.Null) do
            let docUri = request ^. params . textDocument . uri
                selected = request ^. params . range
            txt <- readUri docUri
            ServerConfig { maxOutputSize } <- liftLSP LSP.getConfig
            errorMap <- use errors
            -- Explain is offered for the client-reported diagnostic under the
            -- cursor (which covers an empty selection on a squiggle), or when
            -- the selection meets a known error range.
            let contextExplainable =
                    [ diag
                    | diag@J.Diagnostic { _source = Just source_ } <-
                        request ^. params . context . diagnostics
                    , source_ == "Dhall.TypeCheck" || source_ == "Dhall.Parser"
                    ]
                explained =
                    [ diagnosis_
                    | Just docErrors <- [Map.lookup docUri errorMap]
                    , err <- errParse docErrors ++ errTypes docErrors
                    , Just diagnosis_ <- [explain maxOutputSize err]
                    ]
                explainTarget =
                    case contextExplainable of
                        (diag : _) ->
                            Just (diag ^. range)
                        [] ->
                            case [ rangeToJSON range_
                                 | Diagnosis _ (Just range_) _ <- explained
                                 , rangesMeet selected range_
                                 ] of
                                (jrange : _) -> Just jrange
                                [] -> Nothing
                explainOffered = isJust explainTarget || not (null explained)
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
                selectionParses = case parse selectedText of
                    Right _ -> True
                    Left _ -> False
                explainAction = J.CodeAction
                    { _title = "Explain error"
                    , _kind = Just J.CodeActionKind_QuickFix
                    , _diagnostics =
                        if null contextExplainable
                            then Nothing
                            else Just contextExplainable
                    , _isPreferred = Nothing
                    , _disabled = Nothing
                    , _edit = Nothing
                    , _command = Just J.Command
                        { _title = "Explain error"
                        , _command = "dhall.server.explain"
                        , _arguments = Just
                            (Aeson.toJSON docUri
                                : [ Aeson.toJSON jrange | Just jrange <- [explainTarget] ])
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
            let removeOffered = listToMaybe
                    [ deleteRange
                    | Right expr <- [parse txt]
                    , (matchRange, deleteRange) <- unusedBindingEdits txt expr
                    , rangesMeet selected matchRange
                    ]
                removeAction deleteRange = J.CodeAction
                    { _title = "Remove unused let"
                    , _kind = Just J.CodeActionKind_QuickFix
                    , _diagnostics = Nothing
                    , _isPreferred = Just True
                    , _disabled = Nothing
                    , _edit = Just J.WorkspaceEdit
                        { _changes = Just
                            (Map.singleton docUri
                                [ J.TextEdit
                                    { _range = rangeToJSON deleteRange
                                    , _newText = ""
                                    }
                                ])
                        , _documentChanges = Nothing
                        , _changeAnnotations = Nothing
                        }
                    , _command = Nothing
                    , _data_ = Nothing
                    }
                onImport = importUnderCursor txt selected
                onHashedImport = hashedImportUnderCursor txt selected
                J.Range startPos _ = selected
                importPos = J.TextDocumentPositionParams
                    { _textDocument = J.TextDocumentIdentifier docUri
                    , _position = startPos
                    }
                freezeAction =
                    importCommand "Freeze import" "dhall.server.freezeImport" importPos
                unfreezeAction =
                    importCommand "Unfreeze import" "dhall.server.unfreezeImport" importPos
                checkHashAction =
                    importCommand "Check import hash" "dhall.server.checkImportHash" importPos
                unfreezeAllAction = J.CodeAction
                    { _title = "Unfreeze all imports"
                    , _kind = Just J.CodeActionKind_RefactorRewrite
                    , _diagnostics = Nothing
                    , _isPreferred = Nothing
                    , _disabled = Nothing
                    , _edit = Nothing
                    , _command = Just J.Command
                        { _title = "Unfreeze all imports"
                        , _command = "dhall.server.unfreezeAllImports"
                        , _arguments = Just [Aeson.toJSON docUri]
                        }
                    , _data_ = Nothing
                    }
                onLets = case parse txt of
                    Right expr ->
                        case topLetBlocks expr of
                            [] ->
                                False
                            blocks ->
                                rangesMeet
                                    selected
                                    (Range (blockStart (head blocks), 0) (blockEnd (last blocks), 0))
                    Left _ ->
                        False
                organizeAction
                    | not onLets = []
                    | otherwise = case organizeImports txt of
                        Just (Left reason_) ->
                            [disabledOrganize reason_]
                        Just (Right replacement) ->
                            [readyOrganize docUri txt replacement]
                        Nothing ->
                            []
                inlineAction = case inlineLet txt selected of
                    Just (Left reason_) ->
                        [disabledInline reason_]
                    Just (Right replacement) ->
                        [readyInline docUri txt replacement]
                    Nothing ->
                        []
                offered =
                    [InR normalize | selectionParses]
                        ++ [InR extract | selectionParses]
                        ++ [InR explainAction | explainOffered]
                        ++ [InR (removeAction deleteRange) | deleteRange <- maybeToList removeOffered]
                        ++ [ InR action
                           | onImport
                           , action <- [freezeAction, unfreezeAction, unfreezeAllAction]
                           ]
                        ++ [InR checkHashAction | onHashedImport]
                        ++ map InR organizeAction
                        ++ map InR inlineAction
            respond (Right (InL offered))

-- | lsp-types 2.2 replaced the anonymous @"reason" row with this record.
--   LTS 22 (GHC 9.6) is still on lsp-types 2.1, which keeps the row.
#if MIN_VERSION_lsp_types(2,2,0)
disabledReason :: Text -> J.CodeActionDisabled
disabledReason = J.CodeActionDisabled
#else
disabledReason :: Text -> Rec ("reason" .== Text)
disabledReason reason_ = Label @"reason" .== reason_
#endif

disabledInline :: Text -> J.CodeAction
disabledInline reason_ = J.CodeAction
    { _title = "Inline let: " <> reason_
    , _kind = Just J.CodeActionKind_RefactorInline
    , _diagnostics = Nothing
    , _isPreferred = Nothing
    , _disabled = Just (disabledReason reason_)
    , _edit = Nothing
    , _command = Nothing
    , _data_ = Nothing
    }

readyInline :: J.Uri -> Text -> Text -> J.CodeAction
readyInline docUri txt replacement =
    let lineCount = fromIntegral (length (Text.lines txt))
        _range = J.Range (J.Position 0 0) (J.Position lineCount 0)
        _newText = replacement
    in J.CodeAction
        { _title = "Inline let"
        , _kind = Just J.CodeActionKind_RefactorInline
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

disabledOrganize :: Text -> J.CodeAction
disabledOrganize reason_ = J.CodeAction
    { _title = "Organize imports"
    , _kind = Just J.CodeActionKind_SourceOrganizeImports
    , _diagnostics = Nothing
    , _isPreferred = Nothing
    , _disabled = Just (disabledReason reason_)
    , _edit = Nothing
    , _command = Nothing
    , _data_ = Nothing
    }

readyOrganize :: J.Uri -> Text -> Text -> J.CodeAction
readyOrganize docUri txt replacement =
    let lineCount = fromIntegral (length (Text.lines txt))
        _range = J.Range (J.Position 0 0) (J.Position lineCount 0)
        _newText = replacement
    in J.CodeAction
        { _title = "Organize imports"
        , _kind = Just J.CodeActionKind_SourceOrganizeImports
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

importCommand :: Text -> Text -> J.TextDocumentPositionParams -> J.CodeAction
importCommand title_ command_ pos = J.CodeAction
    { _title = title_
    , _kind = Just J.CodeActionKind_RefactorRewrite
    , _diagnostics = Nothing
    , _isPreferred = Nothing
    , _disabled = Nothing
    , _edit = Nothing
    , _command = Just J.Command
        { _title = title_
        , _command = command_
        , _arguments = Just [Aeson.toJSON pos]
        }
    , _data_ = Nothing
    }

importUnderCursor :: Text -> J.Range -> Bool
importUnderCursor txt selected =
    case parse txt of
        Left _ ->
            False
        Right expr ->
            not $ null
                [ ()
                | Core.Note src (Core.Embed _) <- universeOf Core.subExpressions expr
                , rangesMeet selected (rangeFromDhall src)
                ]

hashedImportUnderCursor :: Text -> J.Range -> Bool
hashedImportUnderCursor txt selected =
    case parse txt of
        Left _ ->
            False
        Right expr ->
            not $ null
                [ ()
                | Core.Note src (Core.Embed (Import (ImportHashed (Just _) _) _)) <-
                    universeOf Core.subExpressions expr
                , rangesMeet selected (rangeFromDhall src)
                ]

rangesMeet :: J.Range -> Range -> Bool
rangesMeet (J.Range startPos endPos) (Range left right) =
    let point (J.Position lineNo col) =
            (fromIntegral lineNo, fromIntegral col)
    in point startPos <= right && left <= point endPos

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
