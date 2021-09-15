{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}

module Dhall.Test.Util
    ( code
    , codeWith
    , equivalent
    , load
    , loadRelativeTo
    , loadWith
    , normalize'
    , normalizeWith'
    , assertNormalizesTo
    , assertNormalizesToWith
    , assertNormalized
    , assertTypeChecks
    , assertDoesntTypeCheck
    , discover
    , Dhall.Test.Util.testCase
    , toDhallPath
    , managedTestEnvironment
    ) where

import Control.Applicative              (liftA2, (<|>))
import Control.Exception                (tryJust)
import Control.Monad                    (guard)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Bifunctor                   (first)
import Data.Text                        (Text)
import Data.Void                        (Void)
import Dhall.Context                    (Context)
import Dhall.Core
    ( Chunks (..)
    , Expr (..)
    , Import
    , Normalizer
    , ReifiedNormalizer (..)
    )
import Dhall.Import                     (SemanticCacheMode (..), Status (..))
import Dhall.Parser                     (Src)
import Prelude                          hiding (FilePath)
import System.IO.Error                  (isDoesNotExistError)
import Test.Tasty                       (TestTree)
import Test.Tasty.HUnit
import Turtle                           (FilePath, Pattern, Shell, fp)

import qualified Control.Exception
import qualified Control.Foldl                    as Foldl
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.Text                        as Text
import qualified Data.Text.IO                     as Text.IO
import qualified Dhall.Context
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Map
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified System.FilePath                  as FilePath
import qualified Test.Tasty                       as Tasty
import qualified Test.Tasty.ExpectedFailure       as Tasty.ExpectedFailure
import qualified Turtle

#ifndef WITH_HTTP
import Control.Monad.IO.Class   (MonadIO (..))
import Dhall.Core               (URL (..))
import Lens.Family.State.Strict (zoom)

import qualified Data.Foldable
import qualified Data.Text.Encoding
import qualified Data.Text.IO
#endif

normalize' :: Expr Src Void -> Text
normalize' = Dhall.Core.pretty . Dhall.Core.normalize

normalizeWith' :: Normalizer Void -> Expr Src Void -> Text
normalizeWith' ctx t =
  Dhall.Core.pretty (Dhall.Core.normalizeWith (Just (ReifiedNormalizer ctx)) t)

code :: Text -> IO (Expr Src Void)
code = codeWith Dhall.Context.empty

codeWith :: Context (Expr Src Void) -> Text -> IO (Expr Src Void)
codeWith ctx expr = do
    expr0 <- case Dhall.Parser.exprFromText mempty expr of
        Left parseError -> Control.Exception.throwIO parseError
        Right expr0     -> return expr0
    expr1 <- load expr0
    case Dhall.TypeCheck.typeWith ctx expr1 of
        Left typeError -> Control.Exception.throwIO typeError
        Right _        -> return ()
    return expr1

load :: Expr Src Import -> IO (Expr Src Void)
load = loadRelativeTo "." UseSemanticCache

loadRelativeTo :: FilePath.FilePath -> SemanticCacheMode -> Expr Src Import -> IO (Expr Src Void)
loadRelativeTo rootDirectory semanticCacheMode expression =
    State.evalStateT
        (loadWith expression)
        (Dhall.Import.emptyStatus rootDirectory) { _semanticCacheMode = semanticCacheMode }

#ifdef WITH_HTTP
loadWith :: Expr Src Import -> StateT Status IO (Expr Src Void)
loadWith = Dhall.Import.loadWith

#else
loadWith :: Expr Src Import -> StateT Status IO (Expr Src Void)
loadWith expr = do
    let mockRemote' url = do
            liftIO . putStrLn $ "\nTesting without real HTTP support --"
                ++ " using mock HTTP client to resolve remote import."
            mockRemote url
    zoom Dhall.Import.remote (State.put mockRemote')
    Dhall.Import.loadWith expr

mockRemote :: Dhall.Core.URL -> StateT Status IO Data.Text.Text
-- Matches anything pointing to
-- `https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/`
mockRemote (URL { authority = "raw.githubusercontent.com"
                , path = Dhall.Core.File (Dhall.Core.Directory components) file })
  | take 3 (reverse components) == ["dhall-lang", "dhall-lang", "master"] = do
    let dropEnd n ls = take (length ls - n) ls
    let localDir = dropEnd 3 components ++ ["dhall-lang"]

    localPath <- Dhall.Import.localToPath Dhall.Core.Here (Dhall.Core.File (Dhall.Core.Directory localDir) file)
    liftIO $ Data.Text.IO.readFile localPath

-- Matches anything pointing to
-- `https://test.dhall-lang.org/Bool/package.dhall`; checks that a `test` header
-- is present and redirects to the local copy of the prelude.
mockRemote (URL { authority = "test.dhall-lang.org"
                , path = Dhall.Core.File (Dhall.Core.Directory components) file
                , headers = Just headersExpr }) =
    case Data.Foldable.find ((== "test") . fst) hs of
        Nothing -> fail $ "(mock http) Tried to load an import from "
                          ++"\"test.dhall-lang.org\""
                          ++ "without setting the \"test\" header field."
        Just (_, _) -> do
            let localDir = components ++ ["Prelude", "dhall-lang"]
            localPath <- Dhall.Import.localToPath Dhall.Core.Here (Dhall.Core.File (Dhall.Core.Directory localDir) file)
            liftIO $ Data.Text.IO.readFile localPath
  where
    hs = Dhall.Import.toHeaders headersExpr

-- Emulates `https://httpbin.org/user-agent`
mockRemote (URL { authority = "httpbin.org"
                , path = Dhall.Core.File (Dhall.Core.Directory []) "user-agent"
                , headers = Just headersExpr }) =
    case Data.Foldable.find ((== "user-agent") . fst) hs of
        Nothing -> fail $ "(mock http) Tried to read the user agent via "
                          ++ "\"httpbin.com/user-agent\" without supplying one "
                          ++ "in the header!"
        Just (_, userAgent) -> do
            let agentText = Data.Text.Encoding.decodeUtf8 userAgent
            return ("{\n  \"user-agent\": \"" <> agentText <> "\"\n}\n")
  where
    hs = Dhall.Import.toHeaders headersExpr

mockRemote url = do
    let urlString = Text.unpack (Dhall.Core.pretty url)
    fail ("(mock http) Url does not match any of the hard-coded rules: "
        <> urlString)
#endif

{- Given a test prefix, returns a managed resource
   which sets / reverts relevant environment variables based
   on `prefix <> "ENV.dhall"` (if present)
 -}
managedTestEnvironment :: Text -> Turtle.Managed [(Text, Maybe Text)]
managedTestEnvironment prefix = Turtle.managed (Control.Exception.bracket setup cleanup)
  where
    envPath = Text.unpack (prefix <> "ENV.dhall")

    setup :: IO [(Text, Maybe Text)]
    setup = do
        envFileContents <-
            tryJust (guard . isDoesNotExistError) (Text.IO.readFile envPath)

        testEnv <- case envFileContents of
            Right contents -> do
                resolved <- code contents
                return (convertEnvExpr (Dhall.Core.normalize resolved))
            Left _ -> return []

        traverse setEnv testEnv

    cleanup :: [(Text, Maybe Text)] -> IO ()
    cleanup = Data.Foldable.traverse_ restoreEnv

    convertEnvExpr :: Expr Src Void -> [(Text, Text)]
    convertEnvExpr (ListLit _ hs) = Data.Foldable.toList (Data.Foldable.fold maybePairs)
      where
          maybePairs = mapM toPair hs

          toPair :: Expr s a -> Maybe (Text, Text)
          toPair (RecordLit m) = do
              (Dhall.Core.recordFieldValue -> TextLit (Chunks [] key), Dhall.Core.recordFieldValue -> TextLit (Chunks [] value))
                  <- lookupHeader <|> lookupMapKey
              return (key, value)
                where
                  lookupHeader = liftA2 (,) (Dhall.Map.lookup "header" m) (Dhall.Map.lookup "value" m)
                  lookupMapKey = liftA2 (,) (Dhall.Map.lookup "mapKey" m) (Dhall.Map.lookup "mapValue" m)
          toPair _ = Nothing
    convertEnvExpr _ = []

    setEnv :: (Text, Text) -> IO (Text, Maybe Text)
    setEnv (k, v) = do
        old <- Turtle.need k
        Turtle.export k v
        return (k, old)

    restoreEnv :: (Text, Maybe Text) -> IO ()
    restoreEnv (k, Just old) = Turtle.export k old
    restoreEnv (k, Nothing) = Turtle.unset k

equivalent :: Text -> Text -> IO ()
equivalent text0 text1 = do
    expr0 <- fmap Dhall.Core.normalize (code text0) :: IO (Expr Void Void)
    expr1 <- fmap Dhall.Core.normalize (code text1) :: IO (Expr Void Void)
    assertEqual "Expressions are not equivalent" expr0 expr1

assertNormalizesTo :: Expr Src Void -> Text -> IO ()
assertNormalizesTo e expected = do
  assertBool msg (not $ Dhall.Core.isNormalized e)
  normalize' e @?= expected
  where msg = "Given expression is already in normal form"

assertNormalizesToWith :: Normalizer Void -> Expr Src Void -> Text -> IO ()
assertNormalizesToWith ctx e expected = do
  assertBool msg (not $ Dhall.Core.isNormalizedWith ctx (first (const ()) e))
  normalizeWith' ctx e @?= expected
  where msg = "Given expression is already in normal form"

assertNormalized :: Expr Src Void -> IO ()
assertNormalized e = do
  assertBool msg1 (Dhall.Core.isNormalized e)
  assertEqual msg2 (normalize' e) (Dhall.Core.pretty e)
  where msg1 = "Expression was not in normal form"
        msg2 = "Normalization is not supposed to change the expression"

assertTypeChecks :: Text -> IO ()
assertTypeChecks text = Data.Functor.void (code text)

assertDoesntTypeCheck :: Text -> IO ()
assertDoesntTypeCheck text = do
    expr0 <- case Dhall.Parser.exprFromText mempty text of
        Left parseError -> Control.Exception.throwIO parseError
        Right e         -> return e
    expr1 <- load expr0
    case Dhall.TypeCheck.typeOf expr1 of
        Left _      -> return ()
        Right type_ -> fail ("Bad type for " <> Text.unpack text <> "\n  " <> show type_)

{-| Automatically run a test on all files in a directory tree that end in
    @A.dhall@
-}
discover :: Pattern Text -> (Text -> TestTree) -> Shell FilePath -> IO TestTree
discover pattern buildTest paths = do
    let shell = do
            path_ <- paths

            let pathText = Turtle.format fp path_

            prefix : _ <- return (Turtle.match pattern pathText)

            return (buildTest prefix)

    tests <- Turtle.fold shell Foldl.list

    return (Tasty.testGroup "discover" tests)

testCase :: Text -> [ FilePath ] -> Assertion -> TestTree
testCase prefix expectedFailures assertion =
    if prefix `elem` map (Turtle.format fp) expectedFailures
    then Tasty.ExpectedFailure.expectFail test
    else test
  where
    test = Test.Tasty.HUnit.testCase (Text.unpack prefix) assertion

{-| Path names on Windows are not valid Dhall paths due to using backslashes
    instead of forwardslashes to separate path components.  This utility fixes
    them if necessary
-}
toDhallPath :: Text -> Text
toDhallPath = Text.replace "\\" "/"
