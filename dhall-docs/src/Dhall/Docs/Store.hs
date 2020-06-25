{-| Utilities to interact with the dhall-docs home directory
-}
module Dhall.Docs.Store (getDocsHomeDirectory, makeHashForDirectory) where

import Dhall.Crypto (SHA256Digest (..), sha256Hash)
import Path         (Abs, Dir, Path)
import Path.IO      (XdgDirectory (..))

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar.Entry
import qualified Data.ByteString.Lazy
import qualified Data.List
import qualified Path
import qualified Path.IO

{-| Fetches the dhall-docs home directory. If @XDG_DATA_HOME@ env var is
    defined, then @${XDG_DATA_HOME}/dhall-docs@ will be returned. Otherwise,
    "${HOME}/.local/share/dhall-docs"
-}
getDocsHomeDirectory :: IO (Path Abs Dir)
getDocsHomeDirectory = do
    dir <- Path.IO.getXdgDir Path.IO.XdgData $ Path.parseRelDir "dhall-docs"
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
    -- Builds a map so key order is preserved between several calls
    files <- Data.List.sort . map Path.fromRelFile . snd
            <$> Path.IO.listDirRecurRel dir

    let setTimeToZero entry = entry{Tar.Entry.entryTime = 0}
    inMemoryTarBytes <- Data.ByteString.Lazy.toStrict . Tar.write . map setTimeToZero
                <$> Tar.pack (Path.fromAbsDir dir) files

    return $ sha256Hash inMemoryTarBytes
