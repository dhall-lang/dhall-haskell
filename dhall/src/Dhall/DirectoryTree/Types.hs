{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Types used by the implementation of the @to-directory-tree@ subcommand
module Dhall.DirectoryTree.Types
    ( DirectoryEntry
    , FileEntry
    , FilesystemEntry(..)
    , Entry(..)
    , User(..)
    , Group(..)
    , Mode(..)
    , Access(..)
    ) where

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
import Dhall.Syntax
    ( Expr (..)
    , FieldSelection (..)
    , Var (..)
    )
import System.PosixCompat.Types (GroupID, UserID)

import qualified Data.Text                   as Text
import qualified Dhall.Marshal.Decode        as Decode

#ifndef mingw32_HOST_OS
import qualified System.PosixCompat.Types    as Posix
#endif

pattern Make :: Text -> Expr s a -> Expr s a
pattern Make label entry <- App (Field (Var (V "_" 0)) (fieldSelectionLabel -> label)) entry

type DirectoryEntry = Entry (Seq FilesystemEntry)

type FileEntry = Entry Text

-- | A filesystem entry.
data FilesystemEntry
    = DirectoryEntry (Entry (Seq FilesystemEntry))
    | FileEntry (Entry Text)
    deriving Show

instance FromDhall FilesystemEntry where
    autoWith normalizer = Decoder
        { expected = pure $ Var (V "tree" 0)
        , extract = \case
            Make "directory" entry ->
                DirectoryEntry <$> extract (autoWith normalizer) entry
            Make "file" entry ->
                FileEntry <$> extract (autoWith normalizer) entry
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
    deriving (Generic, Show)

instance FromDhall a => FromDhall (Entry a) where
    autoWith = Decode.genericAutoWithInputNormalizer Decode.defaultInterpretOptions
        { fieldModifier = Text.toLower . Text.drop (Text.length "entry")
        }

-- | A user identified either by id or name.
data User
    = UserId UserID
    | UserName String
    deriving (Generic, Show)

instance FromDhall User

#ifndef mingw32_HOST_OS
instance FromDhall Posix.CUid where
    autoWith normalizer = Posix.CUid <$> autoWith normalizer
#endif

-- | A group identified either by id or name.
data Group
    = GroupId GroupID
    | GroupName String
    deriving (Generic, Show)

instance FromDhall Group

#ifndef mingw32_HOST_OS
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