{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE NamedFieldPuns       #-}

module Dhall.Import.UserHeaders
    ( defaultUserHeaders
    , envOnlyUserHeaders
    ) where

import Control.Applicative       ((<|>))
import Control.Exception         (tryJust)
import Control.Monad             (guard)
import Control.Monad.IO.Class    (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Either.Combinators   (rightToMaybe)
import Data.Text                 (Text)
import Dhall.Import.Headers      (SiteHeadersFile(..))
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

{-| Resolve the raw dhall text for user headers
    only from $DHALL_HEADERS, not the filesystem
 -}
envOnlyUserHeaders :: MaybeT IO SiteHeadersFile
envOnlyUserHeaders = do
    string <- MaybeT (lookupEnv key)

    return (siteHeadersFile "." (Core.Env (Text.pack key)) (Text.pack string))
  where
    key = "DHALL_HEADERS"

configFileOnlyUserHeaders :: MaybeT IO SiteHeadersFile
configFileOnlyUserHeaders = do
    directory <- liftIO (getXdgDirectory XdgConfig "dhall")

    text <- MaybeT (tryReadFile (directory </> (Text.unpack filename)))

    return (siteHeadersFile directory (makeSource directory) text)
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
defaultUserHeaders :: MaybeT IO SiteHeadersFile
defaultUserHeaders = envOnlyUserHeaders <|> configFileOnlyUserHeaders