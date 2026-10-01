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
    , Command (..)
    , CompletionItem (..)
    , Diagnostic (..)
    , ExecuteCommandParams (..)
    , DiagnosticSeverity (..)
    , DiagnosticTag (..)
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
    , TraceValues (..)
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
import Dhall.LSP.State            (ServerConfig (..))
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
        [diag] <- waitForDiagnosticsSource "Dhall.TypeCheck"
        hover <- getHover docId (Position 0 10)
        liftIO $ do
          let diagText = T.unpack (_message diag)
          diagText `shouldContain` "[ 1, 2 ]"
          diagText `shouldContain` "[ 1, 1 ]"
          case toEither (_contents (fromJust hover)) of
            Left content -> do
              T.unpack (_value content) `shouldContain` "Explain error"
              T.unpack (_value content) `shouldNotContain` "dhall-explain:"
            Right _ ->
              expectationFailure "expected hover text"
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
        docId <- openDoc "UnboundVar.dhall" "dhall"
        sendNotification LSP.SMethod_SetTrace (SetTraceParams TraceValues_Off)
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

foldingSpec :: FilePath -> Spec
foldingSpec fixtureDir =
  describe "folding" $
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
        liftIO $
            map (\r -> (_startLine r, _endLine r)) ranges
                `shouldContain` [(1, 3), (5, 7), (9, 11), (13, 17)]

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
  stability <- testSpec "Stability" (stabilitySpec (baseDir "diagnostics"))
  config <- testSpec "Config" configSpec
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
          stability,
          config
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
        length found `shouldBe` 1
        _disabled (head found) `shouldSatisfy` isJust
        _edit (head found) `shouldBe` Nothing

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
      length found `shouldBe` 1
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
