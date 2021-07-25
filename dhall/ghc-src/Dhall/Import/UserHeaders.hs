{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE NamedFieldPuns       #-}

module Dhall.Import.UserHeaders
    (
      UserHeaders
    , Headers
    , defaultNewUserHeaders
    , envOnlyNewUserHeaders
    , noopUserHeaders
    , withUserHeaders
    ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text.IO as IO
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (Header)
import System.Directory (getXdgDirectory, XdgDirectory(XdgConfig))
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Data.Either.Combinators (rightToMaybe)
import Control.Exception (tryJust)
import Control.Monad (guard)
import Control.Monad.Catch              (throwM)
import System.IO.Error (isDoesNotExistError)
import Dhall.Core (Expr, Import)
import Dhall.Parser (Src)
import Dhall.Import.Headers (SiteHeaders)
import qualified Dhall.Parser as Parser
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP

-- TODO StateT?
data UserHeaders = UserHeaders {
  -- This loading function needs to be injected, because
  -- it would be circular for this module to depend on Import
  loadRelativeTo :: FilePath -> Expr Src Import -> IO SiteHeaders,

  -- Injected in order for tests to use a no-op implementation
  resolveHeaderExpression :: IO (Maybe (FilePath, Text))
}

type Headers = [Header]

noopResolveHeaderExpression :: IO (Maybe (FilePath, Text))
noopResolveHeaderExpression = return Nothing

{-| Resolve the raw dhall text for user headers
    only from $DHALL_HEADERS, not the filesystem
 -}
envOnlyResolveHeaderExpression :: IO (Maybe (FilePath, Text))
envOnlyResolveHeaderExpression =
  fmap (fmap fromEnv) (lookupEnv "DHALL_HEADERS")
    where
      fromEnv expr = (".", Text.pack expr)

{-| Resolve the raw dhall text for user headers,
    along with the directory containing it
    (which is `.` if loaded from $DHALL_HEADERS)
 -}
defaultResolveHeaderExpression :: IO (Maybe (FilePath, Text))
defaultResolveHeaderExpression =
  envOnlyResolveHeaderExpression >>= \case
    Just pair -> return (Just pair)
    Nothing -> loadConfigFile

    where

      loadConfigFile = do
        directory <- getXdgDirectory XdgConfig "dhall"
        fileContents <- tryReadFile (directory </> "headers.dhall")
        let withDirectory text = (directory, text)
        return $ fmap withDirectory fileContents

      tryReadFile path = rightToMaybe <$>
        tryJust (guard . isDoesNotExistError) (IO.readFile path)

defaultNewUserHeaders :: (FilePath -> Expr Src Import -> IO SiteHeaders) -> UserHeaders
defaultNewUserHeaders loadRelativeTo = UserHeaders
  { loadRelativeTo
  , resolveHeaderExpression = defaultResolveHeaderExpression
  }

envOnlyNewUserHeaders :: (FilePath -> Expr Src Import -> IO SiteHeaders) -> UserHeaders
envOnlyNewUserHeaders loadRelativeTo = UserHeaders
  { loadRelativeTo
  , resolveHeaderExpression = envOnlyResolveHeaderExpression
  }

noopUserHeaders :: UserHeaders
noopUserHeaders = UserHeaders
  { resolveHeaderExpression = noopResolveHeaderExpression
  , loadRelativeTo = const $ fail "impossible"
  }

loadHeaderExpr :: UserHeaders -> FilePath -> Text -> IO SiteHeaders
loadHeaderExpr UserHeaders { loadRelativeTo } directory text = do
  -- TODO surely there's a helper for this
  expr <- case Parser.exprFromText mempty text of
    Left exn -> throwM exn
    Right expr -> pure expr
  loadRelativeTo directory expr

loadAllHeaders :: UserHeaders -> IO SiteHeaders
loadAllHeaders userHeaders = resolveHeaderExpression userHeaders >>= \case
  Nothing -> pure HashMap.empty
  Just (directory, text) -> loadHeaderExpr userHeaders directory text

addUserHeaders :: HTTP.Request -> HashMap Text Headers -> HTTP.Request
addUserHeaders request config = addHeaders $ HashMap.lookupDefault [] origin config where
  origin = decodeUtf8 (HTTP.host request) <> ":" <> Text.pack (show (HTTP.port request))

  -- TODO how should we combine user / explicit headers?
  -- I think for forwards compat we should override mheaders (if any)
  -- with userHeaders.
  -- TODO check how library deals with multiple conflicting headers
  -- in the list
  addHeaders newHeaders = request {
    HTTP.requestHeaders = (HTTP.requestHeaders request) <> newHeaders
  }

-- TODO make this lazy / load only once (see ./HTTP newManager)
withUserHeaders :: UserHeaders -> HTTP.Request -> IO HTTP.Request
withUserHeaders userHeaders request = addUserHeaders request <$> (loadAllHeaders userHeaders)