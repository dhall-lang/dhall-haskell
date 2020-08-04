{-| Utilities to interact with the dhall-docs home directory
-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes  #-}


module Dhall.Docs.Store (getDocsHomeDirectory, makeHashForDirectory) where

import Dhall.Crypto (SHA256Digest (..))
import Path         (Abs, Dir, Path, Rel, (</>))
import Path.IO      (XdgDirectory (..))

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar.Entry
import qualified Control.Monad
import qualified Data.ByteString.Char8   as ByteString.Char8
import qualified Data.ByteString.Lazy
import qualified Data.List
import qualified Dhall.Crypto            as Crypto
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
    let setTimeToZero entry = entry{Tar.Entry.entryTime = 0}

    let hashFileAndRename relFile =
            do
                let hashFileName = Crypto.sha256Hash . ByteString.Char8.pack . Path.fromRelFile
                let hash = hashFileName relFile
                let hashAsString = Crypto.toString hash

                hashedRelFile <- Path.parseRelFile hashAsString
                Path.IO.copyFile (dir </> relFile) (dir </> hashedRelFile)

                return hashedRelFile


    files <- Data.List.sort . snd <$> Path.IO.listDirRecurRel dir

    renamedFilePairs <- mapM hashFileAndRename files

    !inMemoryTarBytes <- Data.ByteString.Lazy.toStrict . Tar.write . map setTimeToZero
                <$> Tar.pack (Path.fromAbsDir dir) (map Path.fromRelFile renamedFilePairs)

    Control.Monad.forM_ renamedFilePairs $ \hashedRelFile ->
        Path.IO.removeFile (dir </> hashedRelFile)

    return $ Crypto.sha256Hash inMemoryTarBytes
