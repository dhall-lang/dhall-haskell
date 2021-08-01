{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE NamedFieldPuns       #-}

module Dhall.Import.UserHeaders
    ( defaultUserHeaders
    , envOnlyUserHeaders
    ) where

import Control.Exception        (tryJust)
import Control.Monad            (guard)
import Data.Either.Combinators  (rightToMaybe)
import Data.Text                (Text)
import Dhall.Core               (throws)
import Dhall.Import.Types       (SiteHeadersFile(..))
import System.Directory         (getXdgDirectory, XdgDirectory(XdgConfig))
import System.Environment       (lookupEnv)
import System.FilePath          ((</>))
import System.IO.Error          (isDoesNotExistError)

import qualified Data.Text      as Text
import qualified Data.Text.IO   as IO
import qualified Dhall.Parser   as Parser

parseFrom :: FilePath -> Text -> IO SiteHeadersFile
parseFrom parentDirectory text = do
    expr <- throws (Parser.exprFromText mempty text)
    return (SiteHeadersFile { parentDirectory , expr })

-- lift 'parseFrom' to work on IO (Maybe Text)
parseFrom' :: FilePath -> IO (Maybe Text) -> IO (Maybe SiteHeadersFile)
parseFrom' parentDirectory getText = do
    mtext <- getText
    mapM (parseFrom parentDirectory) mtext

{-| Resolve the raw dhall text for user headers
    only from $DHALL_HEADERS, not the filesystem
 -}
envOnlyUserHeaders :: IO (Maybe SiteHeadersFile)
envOnlyUserHeaders =
    parseFrom' "." (fmap (fmap Text.pack) (lookupEnv "DHALL_HEADERS"))

{-| Resolve the raw dhall text for user headers,
    along with the directory containing it
    (which is `.` if loaded from $DHALL_HEADERS)
 -}
defaultUserHeaders :: IO (Maybe SiteHeadersFile)
defaultUserHeaders =
    envOnlyUserHeaders >>= \case
      Just file -> return (Just file)
      Nothing -> loadConfigFile
        where
          loadConfigFile = do
              directory <- getXdgDirectory XdgConfig "dhall"
              parseFrom' directory (tryReadFile (directory </> "headers.dhall"))

          tryReadFile path = rightToMaybe <$>
              tryJust (guard . isDoesNotExistError) (IO.readFile path)
