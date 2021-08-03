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
import Dhall.Import.Headers     (SiteHeadersFile(..))
import Dhall.Core
    ( Directory(..)
    , File(..)
    , FilePrefix(..)
    , ImportType
    )

import System.Directory         (getXdgDirectory, XdgDirectory(XdgConfig))
import System.Environment       (lookupEnv)
import System.FilePath          ((</>), splitDirectories)
import System.IO.Error          (isDoesNotExistError)

import qualified Data.Text      as Text
import qualified Data.Text.IO   as IO
import qualified Dhall.Core     as Core

siteHeadersFile :: FilePath -> ImportType -> Text -> SiteHeadersFile
siteHeadersFile parentDirectory source fileContents =
    SiteHeadersFile { parentDirectory , source, fileContents }

-- lift 'siteHeadersFile' to work on IO (Maybe Text)
siteHeadersFile' :: FilePath -> ImportType -> IO (Maybe Text) -> IO (Maybe SiteHeadersFile)
siteHeadersFile' parentDirectory source getText = do
    mtext <- getText
    return (fmap (siteHeadersFile parentDirectory source) mtext)

{-| Resolve the raw dhall text for user headers
    only from $DHALL_HEADERS, not the filesystem
 -}
envOnlyUserHeaders :: IO (Maybe SiteHeadersFile)
envOnlyUserHeaders =
    siteHeadersFile' "." (Core.Env (Text.pack key)) (fmap (fmap Text.pack) (lookupEnv key))
      where
        key = "DHALL_HEADERS"

configFileOnlyUserHeaders :: IO (Maybe SiteHeadersFile)
configFileOnlyUserHeaders = do
    directory <- getXdgDirectory XdgConfig "dhall"
    siteHeadersFile'
      directory
      (makeSource directory)
      (tryReadFile (directory </> (Text.unpack filename)))

      where
        filename :: Text
        filename = "headers.dhall"

        makeSource directory =
            Core.Local Absolute File
                { directory = Directory
                    { components = reverse (components directory) }
                , file = filename
                }
 
        components directory = map Text.pack (splitDirectories directory)

        tryReadFile path = rightToMaybe <$>
            tryJust (guard . isDoesNotExistError) (IO.readFile path)


{-| Resolve the raw dhall text for user headers,
    along with the directory containing it
    (which is `.` if loaded from $DHALL_HEADERS)
 -}
defaultUserHeaders :: IO (Maybe SiteHeadersFile)
defaultUserHeaders = envOnlyUserHeaders >>= \case
    Just file -> return (Just file)
    Nothing -> configFileOnlyUserHeaders