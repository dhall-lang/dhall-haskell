{-| Utilities to interact with the dhall-docs home directory
-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE QuasiQuotes #-}


module Dhall.Docs.Store (getDocsHomeDirectory, makeHashForDirectory) where

import Dhall.Crypto    (SHA256Digest (..))
import Dhall.Docs.Util
import Path            (Abs, Dir, Path, Rel, (</>))
import Path.IO         (XdgDirectory (..))

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar.Entry
import qualified Control.Applicative     as Applicative
import qualified Data.ByteString.Char8   as ByteString.Char8
import qualified Data.ByteString.Lazy    as ByteString.Lazy
import qualified Data.List
import qualified Data.Text               as Text
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

    let makeEntry isDir path_ = do
            let realFilePath = Path.toFilePath $ dir </> path_
            let hashFilepath = Crypto.toString
                    $ Crypto.sha256Hash
                    $ ByteString.Char8.pack
                    $ Path.toFilePath path_
            let entryTarPath =
                    case Tar.Entry.toTarPath isDir hashFilepath of
                        Left e ->
                            fileAnIssue $ Text.pack $
                                "An error has occurred when invoking Tar.Entry.toTarPath with " <>
                                realFilePath <> ": " <> e

                        Right tp -> tp
            let pack = if isDir then Tar.Entry.packDirectoryEntry else Tar.Entry.packFileEntry
            pack realFilePath entryTarPath

    (dirs, files) <- Path.IO.listDirRecurRel dir
    let sortedDirs = Data.List.sort dirs
    let sortedFiles = Data.List.sort files

    entries <- Applicative.liftA2 (++) (mapM (makeEntry True) sortedDirs) (mapM (makeEntry False) sortedFiles)

    let inMemoryTarBytes = ByteString.Lazy.toStrict $ Tar.write $ map setTimeToZero entries

    return $ Crypto.sha256Hash inMemoryTarBytes
