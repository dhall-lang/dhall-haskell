-- | Utilities to interact with the dhall-docs home directory

{-# LANGUAGE QuasiQuotes #-}

module Dhall.Docs.Store (getDocsHomeDirectory, makeHashForDirectory) where

import Crypto.Hash  (Digest, SHA256)
import Dhall.Crypto (SHA256Digest (..))
import Path         (Abs, Dir, Path, (</>))
import Path.IO      (XdgDirectory (..))

import qualified Control.Monad           as Monad
import qualified Crypto.Hash             as Hash
import qualified Data.ByteArray          as ByteArray
import qualified Data.ByteString         as ByteString
import qualified Data.ByteString.Char8   as ByteString.Char8
import qualified Data.List               as List
import qualified Path
import qualified Path.IO

{-| Fetches the dhall-docs home directory. If @XDG_DATA_HOME@ env var is
    defined, then @${XDG_DATA_HOME}/dhall-docs@ will be returned. Otherwise,
    "${HOME}/.local/share/dhall-docs"
-}
getDocsHomeDirectory :: IO (Path Abs Dir)
getDocsHomeDirectory = do
    dir <- Path.IO.getXdgDir Path.IO.XdgData $ Just $ [Path.reldir|dhall-docs|]
    Path.IO.ensureDir dir
    return dir

{-| Compute the hash for a directory. It takes into account the hierarchy
    structure of it and the contents of its files, but not the name of the
    actual files.

    This is done by computing the hash of each file and sorting them by its
    absolute file name, and computing the hash of the concatenation of all
    hashes.
-}
makeHashForDirectory :: Path Abs Dir -> IO SHA256Digest
makeHashForDirectory dir = do
    (dirs, files) <- Path.IO.listDirRecurRel dir

    let context0 = Hash.hashInit

    let addDir context directory = do
            let nameBytes = ByteString.Char8.pack (Path.toFilePath directory)

            return $! Hash.hashUpdate context nameBytes

    context1 <- Monad.foldM addDir context0 (List.sort dirs)

    let addFile context file = do
            let nameBytes = ByteString.Char8.pack (Path.toFilePath file)

            contentBytes <- ByteString.readFile (Path.toFilePath (dir </> file))

            return $! Hash.hashUpdates context [ nameBytes, contentBytes ]

    context2 <- Monad.foldM addFile context1 (List.sort files)

    let digest :: Digest SHA256
        digest = Hash.hashFinalize context2

    return (SHA256Digest (ByteArray.convert digest))
