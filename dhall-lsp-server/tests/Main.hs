{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Applicative.Combinators (skipManyTill)
import Control.Lens                ((^.))
import Control.Monad.IO.Class      (liftIO)
import Data.Int                    (Int32)
import Data.Maybe                  (fromJust, isJust)
import Data.Time.Clock             (diffUTCTime, getCurrentTime)
import Language.LSP.Protocol.Types
    ( ClientCapabilities
    , CodeAction (..)
    , CodeActionContext (..)
    , CodeActionKind (..)
    , CodeActionParams (..)
    , Command (..)
    , CompletionItem (..)
    , Diagnostic (..)
    , ExecuteCommandParams (..)
    , DiagnosticSeverity (..)
    , DiagnosticTag (..)
    , DocumentLink (..)
    , DocumentLinkParams (..)
    , FoldingRange (..)
    , FoldingRangeParams (..)
    , Hover (..)
    , InlayHint (..)
    , InlayHintParams (..)
    , TextEdit (..)
    , getUri
    , MarkupContent (..)
    , Definition (..)
    , Location (..)
    , Position (..)
    , Range (..)
    , SetTraceParams (SetTraceParams)
#if MIN_VERSION_lsp_types(2,2,0)
    , TraceValue (..)
#else
    , TraceValues (..)
#endif
    , TextDocumentContentChangeEvent (..)
    , TextDocumentIdentifier (..)
    , TextDocumentItem (..)
    , TextDocumentPositionParams (..)
    , DidOpenTextDocumentParams (..)
    , Uri (..)
    , WorkspaceEdit (..)
    , type (|?) (..)
    , toEither
    )
#if MIN_VERSION_lsp_types(2,2,0)
import Language.LSP.Protocol.Types
    ( TextDocumentContentChangeWholeDocument (..)
    )
#else
import Data.Row ((.==))
#endif
import Language.LSP.Protocol.Lens (result)
import Test.Tasty
import Test.Tasty.Hspec

#if MIN_VERSION_lsp_types(2,3,0)
import Language.LSP.Test hiding (fullLatestClientCaps)
#else
import Language.LSP.Test
#endif

#if MIN_VERSION_tasty_hspec(1,1,7)
import Test.Hspec
#endif

import qualified Data.Aeson         as Aeson
import qualified Data.Map.Strict    as Map
import Data.Default               (def)
import Dhall.LSP.Backend.Diagnostics (clipUserText, stripTrailingComments)
import Dhall.LSP.State            (ServerConfig (..))
import qualified Dhall.LSP.Backend.Dhall as Backend
import qualified Dhall.LSP.Handlers      as Handlers
import Dhall.Pretty               (CharacterSet (..), ChooseCharacterSet (..))
import System.Environment          (setEnv)
import qualified Data.Text       as T
import qualified GHC.IO.Encoding
import qualified Language.LSP.Protocol.Capabilities
import qualified Language.LSP.Protocol.Message as LSP

itemLabel :: CompletionItem -> T.Text
itemLabel (CompletionItem { _label = label }) = label

actionTitle :: CodeAction -> T.Text
actionTitle (CodeAction { _title = title_ }) = title_

baseDir :: FilePath -> FilePath
baseDir d = "tests/fixtures/" <> d

fullLatestClientCaps :: ClientCapabilities
#if MIN_VERSION_lsp_types(2,3,0)
fullLatestClientCaps = Language.LSP.Protocol.Capabilities.fullLatestClientCaps
#else
fullLatestClientCaps = Language.LSP.Protocol.Capabilities.fullCaps
#endif

hoveringSpec :: FilePath -> Spec
hoveringSpec dir =
  describe "Dhall.Hover" $ do
    it "reports types on hover"
      $ runSession "dhall-lsp-server" fullLatestClientCaps dir
      $ do
        docId <- openDoc "Types.dhall" "dhall"
        let typePos = Position 0 5
            functionPos = Position 2 7
            extractContents = toEither . _contents . fromJust
            getValue = T.unpack . _value
        typeHover <- getHover docId typePos
        funcHover <- getHover docId functionPos
        liftIO $ do
          case (extractContents typeHover, extractContents funcHover) of
            (Left typeContent, Left functionContent) -> do
              getValue typeContent `shouldBe` "Type"
              getValue functionContent `shouldBe` "\8704(_isAdmin : Bool) \8594 { home : Text, name : Text }"
            _ -> error "test failed"
          pure ()
    -- The imported file is itself a let block.  After resolution that block
    -- keeps the source span of the import path, which is not a let.  Hover in
    -- the importing file used to fail for the whole file.
    it "reports a type beside an imported let block"
      $ runSession "dhall-lsp-server" fullLatestClientCaps dir
      $ do
        docId <- openDoc "ImportLet.dhall" "dhall"
        let limitPos = Position 4 5
            importedPos = Position 2 6
        limitHover <- getHover docId limitPos
        importedHover <- getHover docId importedPos
        liftIO $ do
          hoverText limitHover `shouldBe` "Natural"
          hoverText importedHover `shouldBe` "Natural"
    -- defaultOutputBytes is 16KiB.  LargeType.dhall's type prints at about 63KB.
    it "truncates a type longer than the output cap"
      $ runSession "dhall-lsp-server" fullLatestClientCaps dir
      $ do
        docId <- openDoc "LargeType.dhall" "dhall"
        hover <- getHover docId (Position 49 6)
        liftIO $ do
          let text = hoverContents hover
          T.isPrefixOf "{ l :" text `shouldBe` True
          T.isSuffixOf "…" text `shouldBe` True
          (T.length text <= 16 * 1024 + 1) `shouldBe` True
          (T.length text >= 16 * 1024 - 64) `shouldBe` True
    -- A trailing syntax error must not take down hover for the part of the
    -- document that still matches the last good parse.
    it "reports types on hover while the document has a syntax error"
      $ runSession "dhall-lsp-server" fullLatestClientCaps dir
      $ do
        docId <- openDoc "Types.dhall" "dhall"
        _ <- waitForDiagnostics
        let replacement =
#if MIN_VERSION_lsp_types(2,2,0)
                TextDocumentContentChangeEvent
                    (InR (TextDocumentContentChangeWholeDocument
                        "let User = { name : Text, home : Text }\n\nlet mkUser =\n        \955(_isAdmin : Bool)\n      \8594       if _isAdmin\n\n        then  { name = \"admin\", home = \"/home/admin\" }\n\n        else  { name = \"default\", home = \"/home/user\" }\n\nin  mkUser True : User $\n"))
#else
                TextDocumentContentChangeEvent (InR (#text .== "let User = { name : Text, home : Text }\n\nlet mkUser =\n        \955(_isAdmin : Bool)\n      \8594       if _isAdmin\n\n        then  { name = \"admin\", home = \"/home/admin\" }\n\n        else  { name = \"default\", home = \"/home/user\" }\n\nin  mkUser True : User $\n"))
#endif
        changeDoc docId [replacement]
        let waitForParser = do
                diags <- waitForDiagnostics
                if any (\d -> _source d == Just "Dhall.Parser") diags
                    then return ()
                    else waitForParser
        waitForParser
        hover <- getHover docId (Position 0 5)
        liftIO $ hoverText hover `shouldBe` "Type"
    it "reports types on hover while the document has a type error" $
      runSession "dhall-lsp-server" fullLatestClientCaps dir $ do
        docId <- openDoc "DespiteError.dhall" "dhall"
        _ <- waitForDiagnosticsSource "Dhall.TypeCheck"
        hover <- getHover docId (Position 0 5)
        liftIO $ hoverText hover `shouldBe` "Type"
    it "reports types on hover while an import is missing" $
      runSession "dhall-lsp-server" fullLatestClientCaps dir $ do
        docId <- openDoc "DespiteImport.dhall" "dhall"
        _ <- waitForDiagnosticsSource "Dhall.Import"
        hover <- getHover docId (Position 2 5)
        liftIO $ hoverText hover `shouldBe` "Type"

hoverContents :: Maybe Hover -> T.Text
hoverContents Nothing = error "no hover"
hoverContents (Just hover) =
  case toEither (_contents hover) of
    Left content -> _value content
    Right _ -> error "hover was not plain text"

hoverText :: Maybe Hover -> String
hoverText = T.unpack . hoverContents

lintingSpec :: FilePath -> Spec
lintingSpec fixtureDir =
  describe "Dhall.Lint" $ do
    it "reports unused bindings"
      $ runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir
      $ do
        _ <- openDoc "UnusedBindings.dhall" "dhall"

        diags <- waitForDiagnosticsSource "Dhall.Lint"

        liftIO $ diags `shouldBe`
            [ Diagnostic
                { _range = Range
                    {_start = Position { _line = 2, _character = 4 }
                    , _end = Position { _line = 2, _character = 7 }
                    }
                , _severity = Just DiagnosticSeverity_Hint
                , _code = Nothing
                , _codeDescription = Nothing
                , _source = Just "Dhall.Lint"
                , _message = "Unused let binding 'bob'"
                , _tags = Just [DiagnosticTag_Unnecessary]
                , _relatedInformation = Nothing
                , _data_ = Nothing
                }
            , Diagnostic
                { _range = Range
                    { _start = Position { _line = 4, _character = 4 }
                    , _end = Position { _line = 4, _character = 8 }
                    }
                , _severity = Just DiagnosticSeverity_Hint
                , _code = Nothing
                , _codeDescription = Nothing
                , _source = Just "Dhall.Lint"
                , _message = "Unused let binding 'carl'"
                , _tags = Just [DiagnosticTag_Unnecessary]
                , _relatedInformation = Nothing
                , _data_ = Nothing
                }
            ]

        pure ()
    it "reports multiple hints"
      $ runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir
      $ do
        _ <- openDoc "SuperfluousIn.dhall" "dhall"
        diags <- waitForDiagnosticsSource "Dhall.Lint"
        liftIO $ length diags `shouldBe` 2
        let diag1 = head diags
            diag2 = diags !! 1
        liftIO $ do
          _severity diag1 `shouldBe` Just DiagnosticSeverity_Hint
          T.unpack (_message diag1) `shouldContain` "Superfluous 'in'"
          _severity diag2 `shouldBe` Just DiagnosticSeverity_Hint
          T.unpack (_message diag2) `shouldContain` "Unused let binding"

codeCompletionSpec :: FilePath -> Spec
codeCompletionSpec fixtureDir =
  describe "Dhall.Completion" $ do
    it "suggests user defined types"
      $ runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir
      $ do
        docId <- openDoc "CustomTypes.dhall" "dhall"
        cs <- getCompletions docId (Position {_line = 2, _character = 35})
        liftIO $ do
          let firstItem = head cs
          itemLabel firstItem `shouldBe` "Config"
          _detail firstItem `shouldBe` Just "Type"
    it "suggests user defined functions"
      $ runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir
      $ do
        docId <- openDoc "CustomFunctions.dhall" "dhall"
        cs <- getCompletions docId (Position {_line = 6, _character = 7})
        liftIO $ do
          let firstItem = head cs
          itemLabel firstItem `shouldBe` "makeUser"
          _detail firstItem `shouldBe` Just "\8704(user : Text) \8594 { home : Text }"
    it "suggests user defined bindings"
      $ runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir
      $ do
        docId <- openDoc "Bindings.dhall" "dhall"
        cs <- getCompletions docId (Position {_line = 0, _character = 59})
        liftIO $ do
          let firstItem = head cs
          itemLabel firstItem `shouldBe` "bob"
          _detail firstItem `shouldBe` Just "Text"
    it "suggests functions from imports"
      $ runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir
      $ do
        docId <- openDoc "ImportedFunctions.dhall" "dhall"
        cs <- getCompletions docId (Position {_line = 0, _character = 33})
        liftIO $ do
          let [ firstItem, secondItem ] = cs
          itemLabel firstItem `shouldBe` "`make user`"
          itemLabel secondItem `shouldBe` "makeUser"
          _detail firstItem `shouldBe` Just "\8704(user : Text) \8594 { home : Text }"
          _detail secondItem `shouldBe` Just "\8704(user : Text) \8594 { home : Text }"
    it "suggests union alternatives"
      $ runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir
      $ do
        docId <- openDoc "Union.dhall" "dhall"
        cs <- getCompletions docId (Position {_line = 2, _character = 10})
        liftIO $ do
          let [ firstItem, secondItem ] = cs
          itemLabel firstItem `shouldBe` "A"
          itemLabel secondItem `shouldBe` "`B C`"
          _detail firstItem `shouldBe` Just "\8704(A : Text) \8594 < A : Text | `B C` >"
          _detail secondItem `shouldBe` Just "< A : Text | `B C` >"
    it "suggests a field of an applied function" $
      runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
        docId <- openDoc "RecordApp.dhall" "dhall"
        cs <- getCompletions docId (Position {_line = 0, _character = 32})
        liftIO $ do
          let labels = map itemLabel cs
          labels `shouldContain` ["a"]
    it "suggests constructors of a union expression" $
      runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
        docId <- openDoc "UnionExpr.dhall" "dhall"
        cs <- getCompletions docId (Position {_line = 0, _character = 10})
        liftIO $ do
          let labels = map itemLabel cs
          labels `shouldContain` ["A", "B"]
    it "suggests record fields while the document is incomplete" $
      runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
        docId <- openDoc "IncompleteRecord.dhall" "dhall"
        cs <- getCompletions docId (Position {_line = 0, _character = 18})
        liftIO $ do
          let labels = map itemLabel cs
          labels `shouldContain` ["a"]
    it "suggests union constructors while the document is incomplete" $
      runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
        docId <- openDoc "IncompleteUnion.dhall" "dhall"
        cs <- getCompletions docId (Position {_line = 0, _character = 18})
        liftIO $ do
          let labels = map itemLabel cs
          labels `shouldContain` ["A", "B"]
    it "suggests identifiers while the document is incomplete" $
      runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
        docId <- openDoc "IncompleteBindings.dhall" "dhall"
        cs <- getCompletions docId (Position {_line = 0, _character = 59})
        liftIO $ do
          let labels = map itemLabel cs
          labels `shouldContain` ["alice"]

diagnosticsSpec :: FilePath -> Spec
diagnosticsSpec fixtureDir = do
  describe "Dhall.TypeCheck" $ do
    it "reports unbound variables"
      $ runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir
      $ do
        _ <- openDoc "UnboundVar.dhall" "dhall"
        [diag] <- waitForDiagnosticsSource "Dhall.TypeCheck"
        liftIO $ do
          _severity diag `shouldBe` Just DiagnosticSeverity_Error
          T.unpack (_message diag) `shouldContain` "Unbound variable"
    it "reports wrong type"
      $ runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir
      $ do
        _ <- openDoc "WrongType.dhall" "dhall"
        [diag] <- waitForDiagnosticsSource "Dhall.TypeCheck"
        liftIO $ do
          _severity diag `shouldBe` Just DiagnosticSeverity_Error
          T.unpack (_message diag) `shouldContain` "Expression doesn't match annotation"
    it "shows both sides of a failed assertion" $
      runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
        docId <- openDoc "Assert.dhall" "dhall"
        [diag@Diagnostic { _range = diagRange }] <- waitForDiagnosticsSource "Dhall.TypeCheck"
        hover <- getHover docId (Position 0 18)
        actions <- getCodeActions docId diagRange
        liftIO $ do
          let diagText = T.unpack (_message diag)
          diagText `shouldContain` "[ 1, 2 ]"
          diagText `shouldContain` "[ 1, 1 ]"
          hoverText hover `shouldBe` "Natural"
          hoverText hover `shouldNotContain` "Assertion failed"
          let found =
                [ codeAction
                | InR codeAction <- actions
                , actionTitle codeAction == "Explain error"
                ]
          length found `shouldBe` 1
          case head found of
            CodeAction { _command = Just Command { _arguments = Just (_ : rangeArg : _) } } ->
              Aeson.fromJSON rangeArg `shouldBe` Aeson.Success diagRange
            _ ->
              expectationFailure "Explain error should carry the diagnostic range"
    it "names both types of an equivalence mismatch" $
      runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
        docId <- openDoc "EquivTypes.dhall" "dhall"
        [diag] <- waitForDiagnosticsSource "Dhall.TypeCheck"
        hover <- getHover docId (Position 0 17)
        liftIO $ do
          let diagText = T.unpack (_message diag)
          diagText `shouldContain` "different types"
          diagText `shouldContain` "Natural"
          diagText `shouldContain` "Bool"
          hoverText hover `shouldBe` "Natural"
    it "offers Explain error for the diagnostic under an empty cursor" $
      runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
        docId <- openDoc "Assert.dhall" "dhall"
        [diag@Diagnostic { _range = diagRange }] <- waitForDiagnosticsSource "Dhall.TypeCheck"
        let Range start _ = diagRange
        rsp <- request LSP.SMethod_TextDocumentCodeAction CodeActionParams
            { _workDoneToken = Nothing
            , _partialResultToken = Nothing
            , _textDocument = docId
            , _range = Range start start
            , _context = CodeActionContext
                { _diagnostics = [diag]
                , _only = Nothing
                , _triggerKind = Nothing
                }
            }
        let actions = case rsp ^. result of
                Right (InL xs) -> xs
                _ -> []
        liftIO $ do
          let found =
                [ codeAction
                | InR codeAction <- actions
                , actionTitle codeAction == "Explain error"
                ]
          length found `shouldBe` 1
    it "keeps an earlier type error when a later edit breaks the syntax" $
      runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
        docId <- openDoc "AssertComment.dhall" "dhall"
        [Diagnostic { _range = assertRange }] <- waitForDiagnosticsSource "Dhall.TypeCheck"
        -- The trailing comment is not part of the underline.
        liftIO $ assertRange `shouldBe` Range (Position 0 8) (Position 0 34)
        let replacement =
#if MIN_VERSION_lsp_types(2,2,0)
                TextDocumentContentChangeEvent
                    (InR (TextDocumentContentChangeWholeDocument
                        "let _ = assert : [1, 2] === [1, 1] -- check\n\nin 0 $\n"))
#else
                TextDocumentContentChangeEvent (InR (#text .== "let _ = assert : [1, 2] === [1, 1] -- check\n\nin 0 $\n"))
#endif
        changeDoc docId [replacement]
        let waitForParser = do
                diags <- waitForDiagnostics
                if any (\d -> _source d == Just "Dhall.Parser") diags
                    then return diags
                    else waitForParser
        diags <- waitForParser
        liftIO $ do
          let sources = map _source diags
          sources `shouldContain` [Just "Dhall.Parser"]
          [Diagnostic { _range = keptRange }] <- return
            [ d | d <- diags, _source d == Just "Dhall.TypeCheck" ]
          keptRange `shouldBe` assertRange
    -- Same as above, but the type error is introduced by an edit after a
    -- clean open.  The background analysis of the first edit must publish
    -- its result into the shared error state, or the second edit has no
    -- earlier error to preserve.
    it "keeps a type error introduced by an edit when a later edit breaks the syntax" $
      runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
        docId <- openDoc "CleanStart.dhall" "dhall"
        _ <- waitForDiagnostics
        let edit1 =
#if MIN_VERSION_lsp_types(2,2,0)
                TextDocumentContentChangeEvent
                    (InR (TextDocumentContentChangeWholeDocument
                        "let _ = assert : [1, 2] === [1, 1] -- check\n\nin 0\n"))
#else
                TextDocumentContentChangeEvent (InR (#text .== "let _ = assert : [1, 2] === [1, 1] -- check\n\nin 0\n"))
#endif
        changeDoc docId [edit1]
        [Diagnostic { _range = assertRange }] <- waitForDiagnosticsSource "Dhall.TypeCheck"
        let edit2 =
#if MIN_VERSION_lsp_types(2,2,0)
                TextDocumentContentChangeEvent
                    (InR (TextDocumentContentChangeWholeDocument
                        "let _ = assert : [1, 2] === [1, 1] -- check\n\nin 0 $\n"))
#else
                TextDocumentContentChangeEvent (InR (#text .== "let _ = assert : [1, 2] === [1, 1] -- check\n\nin 0 $\n"))
#endif
        changeDoc docId [edit2]
        let waitForParser = do
                diags <- waitForDiagnostics
                if any (\d -> _source d == Just "Dhall.Parser") diags
                    then return diags
                    else waitForParser
        diags <- waitForParser
        liftIO $ do
          [Diagnostic { _range = keptRange }] <- return
            [ d | d <- diags, _source d == Just "Dhall.TypeCheck" ]
          keptRange `shouldBe` assertRange
  describe "Dhall.Import" $ do
    it "reports invalid imports"
      $ runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir
      $ do
        _ <- openDoc "InvalidImport.dhall" "dhall"
        [diag] <- waitForDiagnosticsSource "Dhall.Import"
        liftIO $ do
          _severity diag `shouldBe` Just DiagnosticSeverity_Error
          T.unpack (_message diag) `shouldContain` "Invalid input"
    it "reports missing imports"
      $ runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir
      $ do
        _ <- openDoc "MissingImport.dhall" "dhall"
        [diag] <- waitForDiagnosticsSource "Dhall.Import"
        liftIO $ do
          _severity diag `shouldBe` Just DiagnosticSeverity_Error
          T.unpack (_message diag) `shouldContain` "Missing file"
  describe "Dhall.Parser"
    $ it "reports invalid syntax"
    $ runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir
    $ do
      _ <- openDoc "InvalidSyntax.dhall" "dhall"
      [diag] <- waitForDiagnosticsSource "Dhall.Parser"
      liftIO $ _severity diag `shouldBe` Just DiagnosticSeverity_Error

stabilitySpec :: FilePath -> Spec
stabilitySpec fixtureDir =
  describe "Stability" $ do
    it "survives a failing remote import" $
      runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
        docId <- openDoc "RemoteImport.dhall" "dhall"
        [diag] <- waitForDiagnosticsSource "Dhall.Import"
        liftIO $ _severity diag `shouldBe` Just DiagnosticSeverity_Error
        -- The server must still answer requests after the failure.
        hover <- getHover docId (Position 0 0)
        liftIO $ hover `shouldBe` Nothing
    it "accepts a $/setTrace notification" $
      runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
        _ <- openDoc "UnboundVar.dhall" "dhall"
        sendNotification LSP.SMethod_SetTrace
#if MIN_VERSION_lsp_types(2,2,0)
            (SetTraceParams TraceValue_Off)
#else
            (SetTraceParams TraceValues_Off)
#endif
        [diag] <- waitForDiagnosticsSource "Dhall.TypeCheck"
        liftIO $ _severity diag `shouldBe` Just DiagnosticSeverity_Error

configSpec :: Spec
configSpec =
  describe "ServerConfig" $ do
    it "uses defaults for null" $
      Aeson.fromJSON Aeson.Null `shouldBe` Aeson.Success (def :: ServerConfig)
    it "uses defaults for an empty object" $
      Aeson.fromJSON (Aeson.object []) `shouldBe` Aeson.Success (def :: ServerConfig)
    it "uses defaults for a null section" $
      Aeson.fromJSON (Aeson.object ["vscode-dhall-lsp-server" Aeson..= Aeson.Null])
        `shouldBe` Aeson.Success (def :: ServerConfig)
    it "reads the character set and output size" $ do
      let parsed =
            Aeson.fromJSON
              (Aeson.object
                [ "vscode-dhall-lsp-server" Aeson..= Aeson.object
                    [ "character-set" Aeson..= ("ascii" :: T.Text)
                    , "maxOutputSize" Aeson..= (42 :: Int)
                    ]
                ])
      parsed `shouldBe`
        Aeson.Success (ServerConfig (Specify ASCII) 42)
    it "rejects a malformed section" $
      case Aeson.fromJSON (Aeson.object ["vscode-dhall-lsp-server" Aeson..= (5 :: Int)]) of
        Aeson.Error _ -> return ()
        Aeson.Success c ->
          expectationFailure ("expected a parse error, got " ++ show (c :: ServerConfig))

commentTrimSpec :: Spec
commentTrimSpec =
  describe "stripTrailingComments" $ do
    it "trims trailing whitespace" $
      stripTrailingComments "1 + 2  \n" `shouldBe` "1 + 2"
    it "trims a trailing line comment" $
      stripTrailingComments "1 + 2 -- check" `shouldBe` "1 + 2"
    it "trims a trailing block comment" $
      stripTrailingComments "1 + 2 {- check -}" `shouldBe` "1 + 2"
    it "trims nested comments in sequence" $
      stripTrailingComments "x {- a {- b -} c -} -- d" `shouldBe` "x"
    it "keeps -- inside a string" $
      stripTrailingComments "\"a--b\"" `shouldBe` "\"a--b\""
    it "keeps -- inside a multi-line string" $
      stripTrailingComments "'' a--b ''" `shouldBe` "'' a--b ''"

clipTextSpec :: Spec
clipTextSpec =
  describe "clipUserText" $ do
    it "keeps a short message" $
      clipUserText "ok" `shouldBe` "ok"
    it "cuts a message at 2048 characters" $ do
      let long = T.replicate 3000 "a"
          clipped = clipUserText long
      T.length clipped `shouldBe` 2049
      T.last clipped `shouldBe` '…'

documentLinkSpec :: Spec
documentLinkSpec =
  describe "document links" $
    it "logs the URI and the parse error location" $
      case Backend.parse "let x = \n" of
        Right _ ->
          expectationFailure "expected a parse error"
        Left err -> do
          let logged = Handlers.documentLinkParseError (Uri "file:///tmp/broken.dhall") err
          T.unpack logged `shouldContain` "file:///tmp/broken.dhall"
          T.unpack logged `shouldContain` "line 1, column 9"

foldingSpec :: FilePath -> Spec
foldingSpec fixtureDir =
  describe "folding" $ do
    it "folds a record, a list, an if and a merge" $
      runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
        docId <- openDoc "Regions.dhall" "dhall"
        rsp <- request LSP.SMethod_TextDocumentFoldingRange FoldingRangeParams
            { _workDoneToken = Nothing
            , _partialResultToken = Nothing
            , _textDocument = docId
            }
        let ranges = case rsp ^. result of
                Right (InL xs) -> xs
                _ -> []
        liftIO $ do
            let pairs = map (\r -> (_startLine r, _endLine r)) ranges
            pairs `shouldContain` [(0, 3)]
            pairs `shouldContain` [(1, 3)]
            pairs `shouldContain` [(5, 7)]
            pairs `shouldContain` [(9, 11)]
            pairs `shouldContain` [(13, 17)]
    it "folds each let in a multi-let chain" $
      runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
        docId <- openDoc "Consecutive.dhall" "dhall"
        rsp <- request LSP.SMethod_TextDocumentFoldingRange FoldingRangeParams
            { _workDoneToken = Nothing
            , _partialResultToken = Nothing
            , _textDocument = docId
            }
        let ranges = case rsp ^. result of
                Right (InL xs) -> xs
                _ -> []
        liftIO $
            map (\r -> (_startLine r, _endLine r)) ranges
                `shouldContain` [(0, 1), (2, 3)]
    it "folds a record without a same-line field projection" $
      runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
        docId <- openDoc "RecordField.dhall" "dhall"
        rsp <- request LSP.SMethod_TextDocumentFoldingRange FoldingRangeParams
            { _workDoneToken = Nothing
            , _partialResultToken = Nothing
            , _textDocument = docId
            }
        let ranges = case rsp ^. result of
                Right (InL xs) -> xs
                _ -> []
            recordFold = [ r | r <- ranges, _startLine r == 0 ]
        liftIO $ do
            recordFold `shouldSatisfy` (not . null)
            let r = head recordFold
            _endLine r `shouldBe` 1
            _endCharacter r `shouldSatisfy` isJust
            -- `, b = 2 }.a` — the fold must stop at `}`, not at the end of `.a`.
            let Just endCol = _endCharacter r
            endCol `shouldSatisfy` (< 11)

inlaySpec :: FilePath -> Spec
inlaySpec fixtureDir =
  describe "inlay" $
    it "shows the type of an unannotated let" $
      runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
        docId <- openDoc "Let.dhall" "dhall"
        rsp <- request LSP.SMethod_TextDocumentInlayHint InlayHintParams
            { _workDoneToken = Nothing
            , _textDocument = docId
            , _range = Range (Position 0 0) (Position 1 0)
            }
        let hints = case rsp ^. result of
                Right (InL xs) -> xs
                _ -> []
        liftIO $ do
            let hint = head hints
                edits = maybe [] id (_textEdits hint)
                label = case hint of
                    InlayHint { _label = found } -> found
            label `shouldBe` InL ": Natural"
            map _newText edits `shouldContain` [" : Natural"]

-- | Open a file, replace it, and wait until the new diagnostics arrive.
--   The bound is a tripwire for a stuck analysis, not a performance target.
editReplaySpec :: FilePath -> Spec
editReplaySpec dir =
  describe "edit replay" $
    it "publishes diagnostics after a change" $
      runSession "dhall-lsp-server" fullLatestClientCaps dir $ do
        docId <- openDoc "UnboundVar.dhall" "dhall"
        _ <- waitForDiagnostics
        started <- liftIO getCurrentTime
        let replacement =
#if MIN_VERSION_lsp_types(2,2,0)
                TextDocumentContentChangeEvent
                    (InR (TextDocumentContentChangeWholeDocument "1\n"))
#else
                TextDocumentContentChangeEvent (InR (#text .== "1\n"))
#endif
        changeDoc docId [replacement]
        _ <- waitForDiagnostics
        finished <- liftIO getCurrentTime
        liftIO $ diffUTCTime finished started `shouldSatisfy` (< 30)

main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
  diagnostics <- testSpec "Diagnostics" (diagnosticsSpec (baseDir "diagnostics"))
  linting <- testSpec "Linting" (lintingSpec (baseDir "linting"))
  completion <- testSpec "Completion" (codeCompletionSpec (baseDir "completion"))
  hovering <- testSpec "Hovering" (hoveringSpec (baseDir "hovering"))
  replay <- testSpec "Edit replay" (editReplaySpec (baseDir "diagnostics"))
  definition <- testSpec "Definition" (definitionSpec (baseDir "definition"))
  folding <- testSpec "Folding" (foldingSpec (baseDir "folding"))
  inlay <- testSpec "Inlay" (inlaySpec (baseDir "inlay"))
  unfreeze <- testSpec "Unfreeze" (unfreezeSpec (baseDir "unfreeze"))
  organize <- testSpec "Organize" (organizeSpec (baseDir "organize"))
  inline <- testSpec "Inline" (inlineSpec (baseDir "inline"))
  annotate <- testSpec "Annotate" (annotateSpec (baseDir "annotate"))
  normalize <- testSpec "Normalize" (normalizeSpec (baseDir "normalize"))
  stability <- testSpec "Stability" (stabilitySpec (baseDir "diagnostics"))
  config <- testSpec "Config" configSpec
  commentTrim <- testSpec "Comment trimming" commentTrimSpec
  clipText <- testSpec "Clip user text" clipTextSpec
  documentLink <- testSpec "Document links" documentLinkSpec
  defaultMain
    ( testGroup "Tests"
        [ diagnostics,
          linting,
          completion,
          hovering,
          replay,
          definition,
          folding,
          inlay,
          unfreeze,
          organize,
          inline,
          annotate,
          normalize,
          stability,
          config,
          commentTrim,
          clipText,
          documentLink
        ]
    )

-- | Send a command and read the document once its workspace edit is applied.
--
--   'executeCommand' does not wait, and waiting for the command response
--   deadlocks: the server is still inside the command when it asks the client
--   to apply the edit.
applyCommand :: TextDocumentIdentifier -> Command -> Session T.Text
applyCommand docId (Command { _command = command_, _arguments = arguments_ }) = do
    let args = Aeson.decode $ Aeson.encode $ fromJust arguments_
    _ <- sendRequest LSP.SMethod_WorkspaceExecuteCommand (ExecuteCommandParams Nothing command_ args)
    _ <- skipManyTill anyMessage (message LSP.SMethod_WorkspaceApplyEdit)
    documentContents docId

unfreezeSpec :: FilePath -> Spec
unfreezeSpec fixtureDir = describe "unfreeze" $ do
  it "puts back the text freeze changed" $
    runSessionWithConfig (defaultConfig { messageTimeout = 15 }) "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
      docId <- openDoc "Plain.dhall" "dhall"
      original <- documentContents docId
      let params = TextDocumentPositionParams
            { _textDocument = docId
            , _position = Position 0 0
            }
          freeze = Command
            { _title = "Freeze import"
            , _command = "dhall.server.freezeImport"
            , _arguments = Just [Aeson.toJSON params]
            }
          thaw = Command
            { _title = "Unfreeze import"
            , _command = "dhall.server.unfreezeImport"
            , _arguments = Just [Aeson.toJSON params]
            }
      frozen <- applyCommand docId freeze
      thawed <- applyCommand docId thaw
      liftIO $ do
        frozen `shouldNotBe` original
        thawed `shouldBe` original
  it "leaves a missing import hashed" $
    runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
      docId <- openDoc "Missing.dhall" "dhall"
      original <- documentContents docId
      let TextDocumentIdentifier uri_ = docId
          command_ = Command
            { _title = "Unfreeze all imports"
            , _command = "dhall.server.unfreezeAllImports"
            , _arguments = Just [Aeson.toJSON uri_]
            }
      executeCommand command_
      thawedAll <- documentContents docId
      liftIO $ thawedAll `shouldBe` original
  it "freezes an import when the selection starts before it" $
    runSessionWithConfig (defaultConfig { messageTimeout = 15 }) "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
      docId <- openDoc "Spaced.dhall" "dhall"
      -- The selection starts on the whitespace before the import.
      actions <- getCodeActions docId (Range (Position 0 7) (Position 0 20))
      let found =
            [ cmd
            | InR codeAction@CodeAction { _command = Just cmd } <- actions
            , actionTitle codeAction == "Freeze import"
            ]
      frozen <- applyCommand docId (head found)
      liftIO $ T.unpack frozen `shouldContain` "sha256:"
  it "offers Check import hash when the selection starts before a hashed import" $
    runSessionWithConfig (defaultConfig { messageTimeout = 15 }) "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
      docId <- openDoc "Spaced.dhall" "dhall"
      let aroundImport = Range (Position 0 7) (Position 0 20)
      freezeActions <- getCodeActions docId aroundImport
      let freezeCmd =
            [ cmd
            | InR codeAction@CodeAction { _command = Just cmd } <- freezeActions
            , actionTitle codeAction == "Freeze import"
            ]
      _ <- applyCommand docId (head freezeCmd)
      checkActions <- getCodeActions docId aroundImport
      liftIO $ do
        let titles = [ actionTitle codeAction | InR codeAction <- checkActions ]
        titles `shouldContain` ["Check import hash"]

annotateSpec :: FilePath -> Spec
annotateSpec fixtureDir = describe "annotate let" $ do
  it "annotates an unannotated let" $
    expectAnnotate fixtureDir "Unannotated.dhall" (Position 0 4)
      "let a : Natural = 2 in a\n"
  it "replaces an existing annotation" $
    expectAnnotate fixtureDir "Annotated.dhall" (Position 0 4)
      "let a : Natural = 2 in a\n"
  it "annotates a later binding in a multi-let block" $
    expectAnnotate fixtureDir "MultiLet.dhall" (Position 0 14)
      "let a = 2 let b : Text = \"x\" in a\n"

expectAnnotate :: FilePath -> FilePath -> Position -> T.Text -> IO ()
expectAnnotate fixtureDir file pos expected =
  runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
    docId <- openDoc file "dhall"
    let params = TextDocumentPositionParams
          { _textDocument = docId
          , _position = pos
          }
        annotate = Command
          { _title = "Annotate let binding with type"
          , _command = "dhall.server.annotateLet"
          , _arguments = Just [Aeson.toJSON params]
          }
    annotated <- applyCommand docId annotate
    liftIO $ annotated `shouldBe` expected

normalizeSpec :: FilePath -> Spec
normalizeSpec fixtureDir = describe "normalize selection" $ do
  it "normalizes with the enclosing let bindings" $
    expectNormalize fixtureDir "Scoped.dhall" (Range (Position 0 13) (Position 0 18))
      "let a = 2 in 3\n"
  it "falls back to the plain selection when the document does not typecheck" $
    expectNormalize fixtureDir "TypeError.dhall" (Range (Position 0 19) (Position 0 24))
      "let x = \"s\" + 1 in 2\n"

expectNormalize :: FilePath -> FilePath -> Range -> T.Text -> IO ()
expectNormalize fixtureDir file selection expected =
  runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
    docId <- openDoc file "dhall"
    let TextDocumentIdentifier uri_ = docId
        normalize = Command
          { _title = "Normalize selection"
          , _command = "dhall.server.normalize"
          , _arguments = Just [Aeson.toJSON uri_, Aeson.toJSON selection]
          }
    normalized <- applyCommand docId normalize
    liftIO $ normalized `shouldBe` expected

organizeSpec :: FilePath -> Spec
organizeSpec fixtureDir = describe "organize imports" $ do
  it "sorts import bindings by name" $
    expectOrganize fixtureDir "Reorder.dhall"
      "let a = ./a.dhall\nlet b = ./b.dhall\nin { a, b }\n"
  it "drops an unused import binding" $
    expectOrganize fixtureDir "Unused.dhall"
      "let a = ./a.dhall\nin a\n"
  it "refuses a repeated top-level name" $
    runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
      docId <- openDoc "Duplicate.dhall" "dhall"
      actions <- getCodeActions docId (Range (Position 0 0) (Position 3 0))
      liftIO $ do
        let found = [ codeAction | InR codeAction <- actions, actionTitle codeAction == "Organize imports" ]
        found `shouldSatisfy` (not . null)
        _disabled (head found) `shouldSatisfy` isJust
        _edit (head found) `shouldBe` Nothing
  it "is offered for a cursor outside the import block" $
    runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
      docId <- openDoc "Reorder.dhall" "dhall"
      -- The cursor is in the body, past the last import binding.
      actions <- getCodeActions docId (Range (Position 2 3) (Position 2 3))
      liftIO $ do
          let found = [ codeAction | InR codeAction <- actions, actionTitle codeAction == "Organize imports" ]
              kinds =
                [ k
                | CodeAction { _kind = Just k } <- found
                ]
          found `shouldSatisfy` (not . null)
          kinds `shouldContain` [CodeActionKind_QuickFix]
          kinds `shouldContain` [CodeActionKind_SourceOrganizeImports]
          _disabled (head found) `shouldBe` Nothing

expectOrganize :: FilePath -> FilePath -> T.Text -> IO ()
expectOrganize fixtureDir file expected =
  runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
    docId <- openDoc file "dhall"
    actions <- getCodeActions docId (Range (Position 0 0) (Position 5 0))
    liftIO $ do
      let found = [ codeAction | InR codeAction <- actions, actionTitle codeAction == "Organize imports" ]
          action = head found
          edits = maybe [] concat (fmap Map.elems (_edit action >>= _changes))
      map _newText edits `shouldBe` [expected]

inlineSpec :: FilePath -> Spec
inlineSpec fixtureDir = describe "inline let" $ do
  it "inlines a binding" $
    expectTitle fixtureDir "Simple.dhall" (Position 0 4) "Inline let" "1\n"
  it "inlines a use nested in another let" $
    expectTitle fixtureDir "Nested.dhall" (Position 0 4) "Inline let" "let y = 1 in y\n"
  it "refuses when a binder would capture a name" $
    expectDisabled fixtureDir "Capture.dhall" (Position 1 4)
      "Inline let: Inlining would capture a variable."
  it "refuses a use of name@n" $
    expectDisabled fixtureDir "Indexed.dhall" (Position 1 4)
      "Inline let: The body uses a variable of the form name@n."
  it "refuses a binding that contains assert" $
    expectDisabled fixtureDir "Assert.dhall" (Position 0 4)
      "Inline let: The binding contains an assert."
  it "is offered when the selection ends on the binder name" $
    runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
      docId <- openDoc "Simple.dhall" "dhall"
      -- The selection covers "let " and ends exactly where the name starts.
      actions <- getCodeActions docId (Range (Position 0 0) (Position 0 4))
      liftIO $ do
        let found = [ codeAction | InR codeAction <- actions, actionTitle codeAction == "Inline let" ]
            kinds =
              [ k
              | CodeAction { _kind = Just k } <- found
              ]
            edits = maybe [] concat (fmap Map.elems (_edit (head found) >>= _changes))
        found `shouldSatisfy` (not . null)
        kinds `shouldContain` [CodeActionKind_QuickFix]
        kinds `shouldContain` [CodeActionKind_RefactorInline]
        map _newText edits `shouldBe` ["1\n"]

expectTitle :: FilePath -> FilePath -> Position -> T.Text -> T.Text -> IO ()
expectTitle fixtureDir file pos title_ expected =
  runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
    docId <- openDoc file "dhall"
    actions <- getCodeActions docId (Range pos pos)
    liftIO $ do
      let found = [ codeAction | InR codeAction <- actions, actionTitle codeAction == title_ ]
          action = head found
          edits = maybe [] concat (fmap Map.elems (_edit action >>= _changes))
      map _newText edits `shouldBe` [expected]

expectDisabled :: FilePath -> FilePath -> Position -> T.Text -> IO ()
expectDisabled fixtureDir file pos title_ =
  runSession "dhall-lsp-server" fullLatestClientCaps fixtureDir $ do
    docId <- openDoc file "dhall"
    actions <- getCodeActions docId (Range pos pos)
    liftIO $ do
      let found = [ codeAction | InR codeAction <- actions, actionTitle codeAction == title_ ]
      found `shouldSatisfy` (not . null)
      _disabled (head found) `shouldSatisfy` isJust
      _edit (head found) `shouldBe` Nothing

-- | Record fields of an imported value, and import failures met on the way.
definitionSpec :: FilePath -> Spec
definitionSpec dir =
  describe "Dhall.Definition" $ do
    it "opens a field defined in an import" $
      runSession "dhall-lsp-server" fullLatestClientCaps dir $ do
        docId <- openDoc "use.dhall" "dhall"
        _ <- waitForDiagnostics
        defs <- getDefinitions docId (Position 2 8)
        liftIO $ case defs of
          InL (Definition (InL (Location uri (Range (Position line _) _)))) -> do
            T.unpack (getUri uri) `shouldContain` "lib.dhall"
            line `shouldBe` 0
          _ ->
            expectationFailure "expected a location in the imported source"
    it "opens a non-file import through dhall-import" $ do
      setEnv "DHALL_LSP_TEST_LIB" "{ customFunction = 1 }"
      runSession "dhall-lsp-server" fullLatestClientCaps dir $ do
        docId <- openDoc "env-use.dhall" "dhall"
        _ <- waitForDiagnostics
        defs <- getDefinitions docId (Position 2 8)
        liftIO $ case defs of
          InL (Definition (InL (Location uri _))) ->
            T.unpack (getUri uri) `shouldContain` "dhall-import:"
          _ ->
            expectationFailure "expected a dhall-import location"
    it "reports a missing import when a later file inlines that placeholder" $
      runSession "dhall-lsp-server" fullLatestClientCaps dir $ do
        _ <- openDoc "placeholder-use.dhall" "dhall"
        diags <- waitForDiagnostics
        liftIO $
          any
            (\diag -> "no-such-placeholder-file" `T.isInfixOf` _message diag)
            diags
            `shouldBe` True
    it "analyses a mirror from the import it came from" $ do
      setEnv "DHALL_LSP_TEST_LIB" "./lib.dhall"
      runSession "dhall-lsp-server" fullLatestClientCaps dir $ do
        docId <- openDoc "env-use.dhall" "dhall"
        _ <- waitForDiagnostics
        defs <- getDefinitions docId (Position 2 8)
        uri <- liftIO $ case defs of
          InL (Definition (InL (Location uri _))) -> do
            T.unpack (getUri uri) `shouldContain` "dhall-import:"
            return uri
          _ -> do
            expectationFailure "expected a dhall-import location"
            fail "no location"
        let _textDocument = TextDocumentItem
              { _uri = uri
              , _languageId = "dhall"
              , _version = 1 :: Int32
              , _text = "./lib.dhall\n"
              }
        sendNotification LSP.SMethod_TextDocumentDidOpen
            DidOpenTextDocumentParams { _textDocument = _textDocument }
        diags <- waitForDiagnostics
        liftIO $ diags `shouldBe` []
    it "keeps document links from the last successful parse" $
      runSession "dhall-lsp-server" fullLatestClientCaps dir $ do
        docId <- openDoc "GoodLinks.dhall" "dhall"
        _ <- waitForDiagnostics
        let replacement =
#if MIN_VERSION_lsp_types(2,2,0)
                TextDocumentContentChangeEvent
                    (InR (TextDocumentContentChangeWholeDocument
                        "let a = ./lib.dhall\n+1\nlet b = ./lib.dhall\nin a\n"))
#else
                TextDocumentContentChangeEvent (InR (#text .== "let a = ./lib.dhall\n+1\nlet b = ./lib.dhall\nin a\n"))
#endif
        changeDoc docId [replacement]
        rsp <- request LSP.SMethod_TextDocumentDocumentLink DocumentLinkParams
            { _workDoneToken = Nothing
            , _partialResultToken = Nothing
            , _textDocument = docId
            }
        let links = case rsp ^. result of
                Right (InL xs) -> xs
                _ -> []
            shown =
                [ T.pack (show target)
                | DocumentLink { _target = Just target } <- links
                ]
        liftIO $ do
            length links `shouldBe` 2
            shown `shouldSatisfy` all ("lib.dhall" `T.isInfixOf`)
