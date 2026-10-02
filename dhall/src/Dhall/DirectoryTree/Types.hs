{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE ViewPatterns       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Types used by the implementation of the @to-directory-tree@ subcommand
module Dhall.DirectoryTree.Types
    ( FilesystemEntry(DirectoryEntry, BinaryFileEntry, TextFileEntry, FileEntry)
    , DirectoryEntry
    , FileEntry
    , BinaryFileEntry
    , TextFileEntry
    , Entry(..)
    , User(..)
    , Group(..)
    , Mode(..)
    , Access(..)

    , FileMode
    , UserID
    , GroupID

    , setFileMode
    , prettyFileMode

    , isMetadataSupported
    ) where

import Data.ByteString          (ByteString)
import Data.Functor.Identity    (Identity (..))
import Data.Sequence            (Seq)
import Data.Text                (Text)
import Dhall.Marshal.Decode
    ( Decoder (..)
    , FromDhall (..)
    , Generic
    , InputNormalizer
    , InterpretOptions (..)
    )
import Dhall.Syntax             (Expr (..), FieldSelection (..), Var (..))

#if defined(javascript_HOST_ARCH)
import Data.Bits     ((.&.))
import Data.Word     (Word32)
#else
import System.PosixCompat.Types (GroupID, UserID)

import qualified System.PosixCompat.Files as Posix
#endif

import qualified Data.Text                as Text
import qualified Dhall.Marshal.Decode     as Decode

#if defined(javascript_HOST_ARCH)
type FileMode = Word32
type UserID = Word32
type GroupID = Word32
#elif defined(mingw32_HOST_OS)
import Control.Monad            (unless)
import Data.Word                (Word32)
import System.IO                (hPutStrLn, stderr)
import System.PosixCompat.Types (CMode)

import qualified Unsafe.Coerce

type FileMode = CMode
#else
import System.PosixCompat.Types (FileMode)

import qualified System.PosixCompat.Types as Posix
#endif

pattern Make :: Text -> Expr s a -> Expr s a
pattern Make label entry <- App (Field (Var (V "_" 0)) (fieldSelectionLabel -> label)) entry

-- | A directory in the filesystem.
type DirectoryEntry = Entry (Seq FilesystemEntry)

-- | A file in the filesystem.
{-# DEPRECATED FileEntry "`FileEntry` is deprecated and will be removed eventually. Please use `TextFileEntry` instead." #-}
type FileEntry = Entry Text

-- | A binary file in the filesystem.
type BinaryFileEntry = Entry ByteString

-- | A text file in the filesystem.
type TextFileEntry = Entry Text

-- | A filesystem entry.
data FilesystemEntry
    = DirectoryEntry (Entry (Seq FilesystemEntry))
    | BinaryFileEntry BinaryFileEntry
    | TextFileEntry TextFileEntry
    deriving (Eq, Generic, Ord, Show)

pattern FileEntry :: Entry Text -> FilesystemEntry
pattern FileEntry entry = TextFileEntry entry

instance FromDhall FilesystemEntry where
    autoWith normalizer = Decoder
        { expected = pure $ Var (V "tree" 0)
        , extract = \case
            Make "directory" entry ->
                DirectoryEntry <$> extract (autoWith normalizer) entry
            Make "file" entry ->
                TextFileEntry <$> extract (autoWith normalizer) entry
            Make "binary-file" entry ->
                BinaryFileEntry <$> extract (autoWith normalizer) entry
            expr -> Decode.typeError (expected (Decode.autoWith normalizer :: Decoder FilesystemEntry)) expr
        }

-- | A generic filesystem entry. This type holds the metadata that apply to all
-- entries. It is parametric over the content of such an entry.
data Entry a = Entry
    { entryName :: String
    , entryContent :: a
    , entryUser :: Maybe User
    , entryGroup :: Maybe Group
    , entryMode :: Maybe (Mode Maybe)
    }
    deriving (Eq, Generic, Ord, Show)

instance FromDhall a => FromDhall (Entry a) where
    autoWith = Decode.genericAutoWithInputNormalizer Decode.defaultInterpretOptions
        { fieldModifier = Text.toLower . Text.drop (Text.length "entry")
        }

-- | A user identified either by id or name.
data User
    = UserId UserID
    | UserName String
    deriving (Eq, Generic, Ord, Show)

instance FromDhall User

#if defined(javascript_HOST_ARCH)
-- UserID is Word32
#elif defined(mingw32_HOST_OS)
instance FromDhall UserID where
    autoWith normalizer = Unsafe.Coerce.unsafeCoerce <$> autoWith @Word32 normalizer
#else
instance FromDhall Posix.CUid where
    autoWith normalizer = Posix.CUid <$> autoWith normalizer
#endif

-- | A group identified either by id or name.
data Group
    = GroupId GroupID
    | GroupName String
    deriving (Eq, Generic, Ord, Show)

instance FromDhall Group

#if defined(javascript_HOST_ARCH)
-- GroupID is Word32
#elif defined(mingw32_HOST_OS)
instance FromDhall GroupID where
    autoWith normalizer = Unsafe.Coerce.unsafeCoerce <$> autoWith @Word32 normalizer
#else
instance FromDhall Posix.CGid where
    autoWith normalizer = Posix.CGid <$> autoWith normalizer
#endif

-- | A filesystem mode. See chmod(1).
-- The parameter is meant to be instantiated by either `Identity` or `Maybe`
-- depending on the completeness of the information:
--  * For data read from the filesystem it will be `Identity`.
--  * For user-supplied data it will be `Maybe` as we want to be able to set
--    only specific bits.
data Mode f = Mode
    { modeUser :: f (Access f)
    , modeGroup :: f (Access f)
    , modeOther :: f (Access f)
    }
    deriving Generic

deriving instance Eq (Mode Identity)
deriving instance Eq (Mode Maybe)
deriving instance Ord (Mode Identity)
deriving instance Ord (Mode Maybe)
deriving instance Show (Mode Identity)
deriving instance Show (Mode Maybe)

instance FromDhall (Mode Identity) where
    autoWith = modeDecoder

instance FromDhall (Mode Maybe) where
    autoWith = modeDecoder

modeDecoder :: FromDhall (f (Access f)) => InputNormalizer -> Decoder (Mode f)
modeDecoder = Decode.genericAutoWithInputNormalizer Decode.defaultInterpretOptions
    { fieldModifier = Text.toLower . Text.drop (Text.length "mode")
    }

-- | The permissions for a subject (user/group/other).
data Access f = Access
    { accessExecute :: f Bool
    , accessRead :: f Bool
    , accessWrite :: f Bool
    }
    deriving Generic

deriving instance Eq (Access Identity)
deriving instance Eq (Access Maybe)
deriving instance Ord (Access Identity)
deriving instance Ord (Access Maybe)
deriving instance Show (Access Identity)
deriving instance Show (Access Maybe)

instance FromDhall (Access Identity) where
    autoWith = accessDecoder

instance FromDhall (Access Maybe) where
    autoWith = accessDecoder

accessDecoder :: FromDhall (f Bool) => InputNormalizer -> Decoder (Access f)
accessDecoder = Decode.genericAutoWithInputNormalizer Decode.defaultInterpretOptions
    { fieldModifier = Text.toLower . Text.drop (Text.length "access")
    }



-- | A wrapper around `Posix.setFileMode`. On Windows, it does check the
-- resulting file mode of the file/directory and emits a warning if it doesn't
-- match the desired file mode. On all other OS it is identical to
-- `Posix.setFileMode` as it is assumed to work correctly.
setFileMode :: FilePath -> FileMode -> IO ()
#if defined(javascript_HOST_ARCH)
setFileMode _ _ = return ()
#elif defined(mingw32_HOST_OS)
setFileMode fp mode = do
    Posix.setFileMode fp mode
    mode' <- Posix.fileMode <$> Posix.getFileStatus fp
    unless (mode' == mode) $ hPutStrLn stderr $
        "Warning: Setting file mode did not succeed for " <> fp <> "\n" <>
        "    Expected: " <> prettyFileMode mode <> "\n" <>
        "    Actual:   " <> prettyFileMode mode'
#else
setFileMode fp mode = Posix.setFileMode fp mode
#endif

-- | Pretty-print a `FileMode`. The format is similar to the one ls(1):
-- It is display as three blocks of three characters. The first block are the
-- permissions of the user, the second one are the ones of the group and the
-- third one the ones of other subjects. A @r@ denotes that the file or
-- directory is readable by the subject, a @w@ denotes that it is writable and
-- an @x@ denotes that it is executable. Unset permissions are represented by
-- @-@.
prettyFileMode :: FileMode -> String
prettyFileMode mode = userPP <> groupPP <> otherPP
    where
        userPP =
            isBitSet 'r' ownerReadMode <>
            isBitSet 'w' ownerWriteMode <>
            isBitSet 'x' ownerExecuteMode

        groupPP =
            isBitSet 'r' groupReadMode <>
            isBitSet 'w' groupWriteMode <>
            isBitSet 'x' groupExecuteMode

        otherPP =
            isBitSet 'r' otherReadMode <>
            isBitSet 'w' otherWriteMode <>
            isBitSet 'x' otherExecuteMode

        isBitSet c mask = if mask `intersectFileModes` mode /= nullFileMode
            then [c]
            else "-"

#if defined(javascript_HOST_ARCH)
nullFileMode, ownerReadMode, ownerWriteMode, ownerExecuteMode,
  groupReadMode, groupWriteMode, groupExecuteMode,
  otherReadMode, otherWriteMode, otherExecuteMode :: FileMode
nullFileMode     = 0
ownerReadMode    = 0o400
ownerWriteMode   = 0o200
ownerExecuteMode = 0o100
groupReadMode    = 0o040
groupWriteMode   = 0o020
groupExecuteMode = 0o010
otherReadMode    = 0o004
otherWriteMode   = 0o002
otherExecuteMode = 0o001

intersectFileModes :: FileMode -> FileMode -> FileMode
intersectFileModes = (.&.)
#else
nullFileMode = Posix.nullFileMode
ownerReadMode = Posix.ownerReadMode
ownerWriteMode = Posix.ownerWriteMode
ownerExecuteMode = Posix.ownerExecuteMode
groupReadMode = Posix.groupReadMode
groupWriteMode = Posix.groupWriteMode
groupExecuteMode = Posix.groupExecuteMode
otherReadMode = Posix.otherReadMode
otherWriteMode = Posix.otherWriteMode
otherExecuteMode = Posix.otherExecuteMode
intersectFileModes = Posix.intersectFileModes
#endif

-- | Is setting metadata supported on this platform or not.
isMetadataSupported :: Bool
#if defined(mingw32_HOST_OS) || defined(javascript_HOST_ARCH)
isMetadataSupported = False
#else
isMetadataSupported = True
#endif
