{-| Utilities to interact with the dhall-docs home directory
-}
module Dhall.Docs.Store (getDocsHomeDirectory, makeHashForDirectory) where

import Data.Map.Strict (Map)
import Dhall.Crypto    (SHA256Digest (..), sha256Hash)
import Path            (Abs, Dir, File, Path)
import Path.IO         (XdgDirectory (..))

import qualified Data.ByteString
import qualified Data.Map.Strict as Map
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
    hashes <- Map.elems <$> Path.IO.walkDirAccum Nothing go dir
    return $ sha256Hash $ Data.ByteString.concat $ Prelude.map unSHA256Digest hashes
  where
    go :: (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> IO (Map (Path Abs File) SHA256Digest))
    go _ _ files =
        (Map.fromList . zip files) <$>
            mapM (fmap sha256Hash . Data.ByteString.readFile . Path.fromAbsFile) files
