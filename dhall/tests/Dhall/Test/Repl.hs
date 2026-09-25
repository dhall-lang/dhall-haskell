-- | Tests for @dhall repl@.
--
-- These drive 'Dhall.Repl.repl' with a closed stdin pipe, which is the same
-- non-terminal mode Haskeline uses for a redirected standard input. The
-- process standard handles are redirected for the duration of each test, so
-- the suite must run single-threaded (see @TASTY_NUM_THREADS@ in
-- "Main").

module Dhall.Test.Repl (tests) where

import Control.Concurrent
    ( forkFinally
    , newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Exception
    ( SomeException
    , bracket
    , displayException
    , evaluate
    , try
    )
import Data.List (findIndex, isInfixOf, isPrefixOf, tails)
import Dhall.Pretty (CharacterSet (..))
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.FilePath ((</>))
import System.IO
    ( Handle
    , hClose
    , hGetContents
    , hPutStr
    , hSetBuffering
    , hSetEncoding
    , stderr
    , stdin
    , stdout
    , utf8
    )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import qualified Dhall.Repl
import qualified System.Directory as Directory
import qualified System.IO as IO
import qualified System.IO.Temp as Temp
import qualified System.Process as Process

-- | Plain output of one REPL session, plus any files it wrote in its working
-- directory (including @.history@).
data Session = Session
    { sessionOutput :: String
    , sessionFiles :: [(FilePath, String)]
    }

tests :: TestTree
tests =
    testGroup "repl"
        [ sessionCase "normalizes an expression and keeps going after a type error" Unicode
            "1 + True\n100 + 23\n:quit\n" $ \session -> do
                assertHas session "type error" "only works on"
                assertHas session "recovery" "\n123\n"
                assertBefore session "type error stays on screen" "only works on" "\n123\n"
                assertGoodbyeOnce session
        , sessionCase "infers types and accepts a unique command prefix" Unicode
            ":type 1\n:t Natural\n:quit\n" $ \session -> do
                assertHas session "type of 1" "Natural"
                assertHas session "prefix :t" "Type"
        , sessionCase "binds, saves, clears, and loads" Unicode
            ":let x = 1 + 1\n:save ctx.dhall\n:clear\nx\n:load ctx.dhall\nx\n:quit\n" $ \session -> do
                assertHas session "binding" "x : Natural"
                assertHas session "saved message" "Context saved to `ctx.dhall`"
                assertHas session "cleared binding" "Unbound variable: x"
                assertHas session "loaded message" "Loaded `ctx.dhall`"
                assertBefore session "clear happens before load" "Unbound variable: x" "Loaded `ctx.dhall`"
                saved <- fileText session "ctx.dhall"
                assertBool ("saved file should contain the normalized binding\n" <> saved)
                    (":let x = 2" `isInfixOf` saved)
                assertHas session "loaded value" "\n2\n"
        , sessionCase "saves every binding when :save has no file name" Unicode
            ":let x = 1\n:save\n:quit\n" $ \session -> do
                saved <- fileText session ".dhall-repl-0"
                assertBool ("default save file:\n" <> saved)
                    (":let x = 1" `isInfixOf` saved)
                assertHas session "default save message" "Context saved to `.dhall-repl-0`"
        , sessionCase "saves one expression" Unicode
            ":save out.dhall = 1 + 1\n:quit\n" $ \session -> do
                saved <- fileText session "out.dhall"
                assertBool ("expression file:\n" <> saved) ("2" `isInfixOf` saved)
                assertHas session "expression save message" "Expression saved to `out.dhall`"
        , sessionCase "hashes a normalized expression" Unicode
            ":hash 1\n:ha 1\n:quit\n" $ \session -> do
                let hash = "sha256:d60d8415e36e86dae7f42933d3b0c4fe3ca238f057fba206c7e9fbf5d784fe15"
                countOcc hash (sessionOutput session) @?= 2
        , sessionCase "checks a :let annotation" Unicode
            ":let x : Integer = 1\nx\n:quit\n" $ \session -> do
                assertHas session "annotation" "Expression doesn't match annotation"
                assertHas session "unbound after failed let" "Unbound variable: x"
        , sessionCase "explains a type error only while --explain is set" Unicode
            ":set --explain\n1 + True\n:unset --explain\n1 + True\n:quit\n" $ \session -> do
                countOcc "Explanation:" (sessionOutput session) @?= 1
                assertHas session "unset form still errors" "only works on"
        , sessionCaseWith "starts in explain mode when requested" Unicode True
            "1 + True\n:quit\n" $ \session ->
                assertHas session "initial explain" "Explanation:"
        , sessionCase "rejects unknown commands and bad :set arguments" Unicode
            ":nope\n:set\n:unset\n:load missing-file\n50 + 7\n:quit\n" $ \session -> do
                assertHas session "unknown command" "No such command :nope"
                assertHas session "bad set" ":set should be of the form `:set <command line option>`"
                assertHas session "bad unset" ":unset should be of the form `:unset <command line option>`"
                assertHas session "missing load" "does not exist"
                assertHas session "continues after load failure" "\n57\n"
        , sessionCase "prints help for every command" Unicode
            ":help\n:quit\n" $ \session ->
                mapM_ (\command -> assertHas session command (":" <> command))
                    [ "help", "paste", "type", "hash", "let", "clear"
                    , "load", "save", "set", "unset", "quit"
                    ]
        , sessionCase "reads a multi-line expression from :paste" Unicode
            ":paste\nlet x = 40\nin x + x\n" $ \session -> do
                assertHas session "paste banner" "Entering multi-line mode. Press <Ctrl-D> to finish."
                assertHas session "pasted expression" "\n80\n"
                assertGoodbyeOnce session
        , sessionCase "reads a multi-line :let from :paste" Unicode
            ":paste\n:let z = 1 + 1\n" $ \session -> do
                assertHas session "pasted let" "z : Natural"
                assertGoodbyeOnce session
        , sessionCase "ignores blank lines and reports a bare colon" Unicode
            "\n\n:\n1 + 1\n:quit\n" $ \session -> do
                assertHas session "bare colon" "Invalid input"
                assertHas session "expression after colon" "2"
        , sessionCase ":q quits before later input is read" Unicode
            "100 + 23\n:q\n99\n" $ \session -> do
                assertHas session "value before quit" "\n123\n"
                assertGoodbyeOnce session
                assertBool
                    ("input after :q was evaluated:\n" <> sessionOutput session)
                    (not ("99" `isInfixOf` sessionOutput session))
        , sessionCase "uses an ASCII prompt" ASCII
            "1 + 1\n:quit\n" $ \session -> do
                assertHas session "ascii prompt" "|- "
                assertBool
                    ("unicode prompt leaked into ascii mode:\n" <> sessionOutput session)
                    (not ("⊢" `isInfixOf` sessionOutput session))
        , sessionCase "uses a Unicode prompt" Unicode
            "1 + 1\n:quit\n" $ \session ->
                assertHas session "unicode prompt" "⊢"
        , sessionCase "creates a history file" Unicode
            "1 + 1\n:quit\n" $ \session ->
                assertBool
                    ("history file missing from " <> show (map fst (sessionFiles session)))
                    (".history" `elem` map fst (sessionFiles session))
        ]

sessionCase :: String -> CharacterSet -> String -> (Session -> IO ()) -> TestTree
sessionCase name characterSet input assert =
    sessionCaseWith name characterSet False input assert

sessionCaseWith :: String -> CharacterSet -> Bool -> String -> (Session -> IO ()) -> TestTree
sessionCaseWith name characterSet explain input assert =
    testCase name $ do
        session <- runRepl characterSet explain input
        assert session

runRepl :: CharacterSet -> Bool -> String -> IO Session
runRepl characterSet explain input =
    Temp.withSystemTempDirectory "dhall-repl-test" $ \dir ->
        Directory.withCurrentDirectory dir $ do
            (inRead, inWrite) <- Process.createPipe
            (outRead, outWrite) <- Process.createPipe
            mapM_ (`hSetEncoding` utf8) [inRead, inWrite, outRead, outWrite]
            hPutStr inWrite input
            hClose inWrite

            outputVar <- newEmptyMVar
            _ <- forkFinally (forceContents outRead) $ \result ->
                putMVar outputVar $ case result of
                    Right text -> text
                    Left exc ->
                        "failed to read repl output: " <> displayException exc

            bracket
                (do
                    oldIn <- hDuplicate stdin
                    oldOut <- hDuplicate stdout
                    oldErr <- hDuplicate stderr
                    pure (oldIn, oldOut, oldErr))
                (\(oldIn, oldOut, oldErr) -> do
                    hDuplicateTo oldIn stdin
                    hDuplicateTo oldOut stdout
                    hDuplicateTo oldErr stderr
                    hClose oldIn
                    hClose oldOut
                    hClose oldErr)
                $ \_ -> do
                    hDuplicateTo inRead stdin
                    hClose inRead
                    hDuplicateTo outWrite stdout
                    hDuplicateTo outWrite stderr
                    hClose outWrite
                    hSetBuffering stdin IO.LineBuffering
                    hSetBuffering stdout IO.LineBuffering
                    hSetBuffering stderr IO.LineBuffering
                    hSetEncoding stdin utf8
                    hSetEncoding stdout utf8
                    hSetEncoding stderr utf8
                    outcome <- try
                        (timeout (20 * 1000000) (Dhall.Repl.repl characterSet explain))
                        :: IO (Either SomeException (Maybe ()))
                    case outcome of
                        Right (Just ()) -> pure ()
                        Right Nothing ->
                            fail "dhall repl timed out"
                        Left exc ->
                            fail ("dhall repl threw " <> displayException exc)

            output <- takeMVar outputVar
            names <- Directory.listDirectory dir
            files <- mapM (readSessionFile dir) names
            pure Session
                { sessionOutput = stripAnsi output
                , sessionFiles = files
                }

readSessionFile :: FilePath -> FilePath -> IO (FilePath, String)
readSessionFile dir name = do
    let path = dir </> name
    isFile <- Directory.doesFileExist path
    if not isFile
        then pure (name, "")
        else do
            contents <- IO.readFile path
            _ <- evaluate (length contents)
            pure (name, contents)

forceContents :: Handle -> IO String
forceContents handle = do
    contents <- hGetContents handle
    _ <- evaluate (length contents)
    pure contents

fileText :: Session -> FilePath -> IO String
fileText session name =
    case lookup name (sessionFiles session) of
        Just text -> pure text
        Nothing ->
            assertFailure
                ("missing " <> name <> " in " <> show (map fst (sessionFiles session)))

assertHas :: Session -> String -> String -> IO ()
assertHas session label needle =
    assertBool
        (label <> "\nmissing: " <> needle <> "\noutput:\n" <> sessionOutput session)
        (needle `isInfixOf` sessionOutput session)

assertBefore :: Session -> String -> String -> String -> IO ()
assertBefore session label earlier later = do
    assertHas session label earlier
    assertHas session label later
    let output = sessionOutput session
    case (substringIndex earlier output, substringIndex later output) of
        (Just i, Just j) ->
            assertBool
                (label <> "\nexpected " <> earlier <> " before " <> later <> "\n" <> output)
                (i < j)
        _ ->
            assertFailure label

assertGoodbyeOnce :: Session -> IO ()
assertGoodbyeOnce session =
    countOcc "Goodbye." (sessionOutput session) @?= 1

countOcc :: String -> String -> Int
countOcc needle haystack
    | null needle = 0
    | otherwise = length (filter (needle `isPrefixOf`) (tails haystack))

substringIndex :: String -> String -> Maybe Int
substringIndex needle haystack =
    findIndex (needle `isPrefixOf`) (tails haystack)

stripAnsi :: String -> String
stripAnsi [] = []
stripAnsi ('\ESC' : '[' : rest) =
    stripAnsi (drop 1 (dropWhile (/= 'm') rest))
stripAnsi (char : rest) = char : stripAnsi rest
