{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Dhall.Syntax.Import
    ( Directory(..)
    , File(..)
    , FilePrefix(..)
    , Import(..)
    , ImportHashed(..)
    , ImportMode(..)
    , ImportType(..)
    , URL(..)
    , Scheme(..)
    ) where

import Data.Data                      (Data)
import Data.Text                      (Text)
import Dhall.Src                      (Src (..))
import Dhall.Syntax.Expr              (Expr (..))
import Dhall.Syntax.Instances.Data    ()
import Dhall.Syntax.Instances.Functor ()
import GHC.Generics                   (Generic)

import qualified Dhall.Crypto

{-| Internal representation of a directory that stores the path components in
    reverse order

    In other words, the directory @\/foo\/bar\/baz@ is encoded as
    @Directory { components = [ "baz", "bar", "foo" ] }@
-}
newtype Directory = Directory { components :: [Text] }
    deriving (Data, Generic)

{-| A `File` is a `directory` followed by one additional path component
    representing the `file` name
-}
data File = File
    { directory :: Directory
    , file      :: Text
    } deriving (Data, Generic)

-- | The beginning of a file path which anchors subsequent path components
data FilePrefix
    = Absolute
    -- ^ Absolute path
    | Here
    -- ^ Path relative to @.@
    | Parent
    -- ^ Path relative to @..@
    | Home
    -- ^ Path relative to @~@
    deriving (Data, Generic)

-- | The URI scheme
data Scheme = HTTP | HTTPS
    deriving (Data, Generic)

-- | This type stores all of the components of a remote import
data URL = URL
    { scheme    :: Scheme
    , authority :: Text
    , path      :: File
    , query     :: Maybe Text
    , headers   :: Maybe (Expr Src Import)
    } deriving (Data, Generic)

-- | The type of import (i.e. local vs. remote vs. environment)
data ImportType
    = Local FilePrefix File
    -- ^ Local path
    | Remote URL
    -- ^ URL of remote resource and optional headers stored in an import
    | Env  Text
    -- ^ Environment variable
    | Missing
    deriving (Data, Generic)

-- | How to interpret the import's contents (i.e. as Dhall code or raw text)
data ImportMode = Code | RawText | Location | RawBytes
  deriving (Data, Generic)

-- | A `ImportType` extended with an optional hash for semantic integrity checks
data ImportHashed = ImportHashed
    { hash       :: Maybe Dhall.Crypto.SHA256Digest
    , importType :: ImportType
    } deriving (Data, Generic)

-- | Reference to an external resource
data Import = Import
    { importHashed :: ImportHashed
    , importMode   :: ImportMode
    } deriving (Data, Generic)




instance Semigroup Directory where
    Directory components0 <> Directory components1 =
        Directory (components1 <> components0)

instance Semigroup File where
    File directory0 _ <> File directory1 file =
        File (directory0 <> directory1) file

instance Semigroup ImportType where
    Local prefix file0 <> Local Here file1 = Local prefix (file0 <> file1)

    Remote (URL { path = path0, ..}) <> Local Here path1 =
        Remote (URL { path = path0 <> path1, ..})

    Local prefix file0 <> Local Parent file1 =
        Local prefix (file0 <> parent <> file1)

    Remote (URL { path = path0, .. }) <> Local Parent path1 =
        Remote (URL { path = path0 <> parent <> path1, .. })

    import0 <> Remote (URL { headers = headers0, .. }) =
        Remote (URL { headers = headers1, .. })
      where
        importHashed0 = Import (ImportHashed Nothing import0) Code

        headers1 = fmap (fmap (importHashed0 <>)) headers0

    _ <> import1 =
        import1

instance Semigroup ImportHashed where
    ImportHashed _ importType0 <> ImportHashed hash importType1 =
        ImportHashed hash (importType0 <> importType1)

instance Semigroup Import where
    Import importHashed0 _ <> Import importHashed1 code =
        Import (importHashed0 <> importHashed1) code

parent :: File
parent = File { directory = Directory { components = [ ".." ] }, file = "" }
