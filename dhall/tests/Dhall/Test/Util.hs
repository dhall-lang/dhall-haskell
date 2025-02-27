{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
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
    , pathIn
    , pathNotIn
    , pathNotPrefixOf
    , pathNotSuffixOf
    , toDhallPath
    , managedTestEnvironment
    ) where

import Control.Applicative              (Alternative, liftA2, (<|>))
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
import Dhall.Import                     (SemanticCacheMode (..), Status)
import Dhall.Parser                     (Src)
import Lens.Micro                       (set)
import System.IO.Error                  (isDoesNotExistError)
import Test.Tasty                       (TestTree)
import Test.Tasty.HUnit
import Turtle                           (Pattern, Shell, fp)

import qualified Control.Exception
import qualified Control.Foldl                    as Foldl
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Functor
import qualified Data.List                        as List
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

#if defined(WITH_HTTP) && defined(NETWORK_TESTS)
import qualified Data.Foldable
#else
import Control.Monad.IO.Class (MonadIO (..))
import Dhall.Core             (Directory (..), File (..), URL (..))
import Lens.Micro.Mtl         (zoom)

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
        (set Dhall.Import.semanticCacheMode semanticCacheMode (Dhall.Import.emptyStatus rootDirectory))

#if defined(WITH_HTTP) && defined(NETWORK_TESTS)
loadWith :: Expr Src Import -> StateT Status IO (Expr Src Void)
loadWith = Dhall.Import.loadWith

#else
loadWith :: Expr Src Import -> StateT Status IO (Expr Src Void)
loadWith expr = do
    zoom Dhall.Import.remote (State.put mockRemote)
    Dhall.Import.loadWith expr

mockRemote :: Dhall.Core.URL -> StateT Status IO Data.Text.Text
mockRemote
    url@URL
        { authority = "raw.githubusercontent.com"
        , path = File (Directory components) file
        } = do
    let localDir = case reverse components of
            "dhall-lang" : "dhall-lang" : _ : rest ->
                reverse ("dhall-lang" : rest)
            "Nadrieril" : "dhall-rust" : _ : "dhall" : rest ->
                reverse ("dhall-lang" : rest)
            _ -> do
                fail ("Unable to mock URL: " <> Text.unpack (Dhall.Core.pretty url))

    localPath <- Dhall.Import.localToPath Dhall.Core.Here (File (Directory localDir) file)

    liftIO (Data.Text.IO.readFile localPath)

mockRemote
    URL { authority = "prelude.dhall-lang.org"
        , path = File (Directory components) file
        } = do
    let localDir = components ++ [ "Prelude", "dhall-lang" ]

    localPath <- Dhall.Import.localToPath Dhall.Core.Here (File (Directory localDir) file)

    liftIO (Data.Text.IO.readFile localPath)

mockRemote url@URL{ authority = "test.dhall-lang.org", path, headers } =
    case (path, fmap Dhall.Import.toHeaders headers) of
        (File (Directory []) "foo", Just [("test", _)]) ->
            return "./bar"
        (File (Directory []) "bar", Just [("test", _)]) ->
            return "True"
        (File (Directory ["cors"]) "AllowedAll.dhall", _) ->
            return "42"
        (File (Directory ["cors"]) "OnlyGithub.dhall", _) ->
            return "42"
        (File (Directory ["cors"]) "OnlySelf.dhall", _) ->
            return "42"
        (File (Directory ["cors"]) "OnlyOther.dhall", _) ->
            return "42"
        (File (Directory ["cors"]) "Empty.dhall", _) ->
            return "42"
        (File (Directory ["cors"]) "NoCORS.dhall", _) ->
            return "42"
        (File (Directory ["cors"]) "Null.dhall", _) ->
            return "42"
        (File (Directory ["cors"]) "SelfImportAbsolute.dhall", _) ->
            return "https://test.dhall-lang.org/cors/NoCORS.dhall"
        (File (Directory ["cors"]) "SelfImportRelative.dhall", _) ->
            return "./NoCORS.dhall"
        (File (Directory ["cors"]) "TwoHopsFail.dhall", _) ->
            return "https://raw.githubusercontent.com/dhall-lang/dhall-lang/5ff7ecd2411894dd9ce307dc23020987361d2d43/tests/import/data/cors/OnlySelf.dhall"
        (File (Directory ["cors"]) "TwoHopsSuccess.dhall", _) ->
            return "https://raw.githubusercontent.com/dhall-lang/dhall-lang/5ff7ecd2411894dd9ce307dc23020987361d2d43/tests/import/data/cors/OnlyGithub.dhall"
        _ -> do
            fail ("Unable to mock URL: " <> Text.unpack (Dhall.Core.pretty url))

mockRemote url@URL{ authority = "httpbin.org", path, headers } =
    case (path, fmap Dhall.Import.toHeaders headers) of
        (File (Directory []) "user-agent", Just [("user-agent", userAgent)]) -> do
            let agentText = Data.Text.Encoding.decodeUtf8 userAgent

            return ("{\n  \"user-agent\": \"" <> agentText <> "\"\n}\n")
        (File (Directory []) "user-agent", Nothing) -> do
            return ("{\n  \"user-agent\": \"Dhall\"\n}\n")
        _ -> do
            fail ("Unable to mock URL: " <> Text.unpack (Dhall.Core.pretty url))

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

            let pathText = Turtle.format fp (FilePath.normalise path_)

            prefix : _ <- return (Turtle.match pattern pathText)

            return (buildTest prefix)

    tests <- Turtle.fold shell Foldl.list

    return (Tasty.testGroup "discover" tests)

testCase :: Text -> [ FilePath ] -> Assertion -> TestTree
testCase prefix expectedFailures assertion =
    if prefix `elem` map (Turtle.format fp . FilePath.normalise) expectedFailures
    then Tasty.ExpectedFailure.expectFail test
    else test
  where
    test = Test.Tasty.HUnit.testCase (Text.unpack prefix) assertion

pathIn :: Alternative f => FilePath -> [FilePath] -> f ()
pathIn this = guard . any (FilePath.equalFilePath this)

pathNotIn :: Alternative f => FilePath -> [FilePath] -> f ()
pathNotIn this = guard . not . any (FilePath.equalFilePath this)

pathNotPrefixOf :: Alternative f => FilePath -> FilePath -> f ()
pathNotPrefixOf this =
    guard . not . List.isPrefixOf (FilePath.normalise this) . FilePath.normalise

pathNotSuffixOf :: Alternative f => FilePath -> FilePath -> f ()
pathNotSuffixOf this =
    guard . not . List.isSuffixOf (FilePath.normalise this) . FilePath.normalise

{-| Path names on Windows are not valid Dhall paths due to using backslashes
    instead of forwardslashes to separate path components.  This utility fixes
    them if necessary
-}
toDhallPath :: Text -> Text
toDhallPath = ("./" <>) . Text.replace "\\" "/"
