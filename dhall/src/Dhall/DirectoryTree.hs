{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Implementation of the @dhall to-directory-tree@ subcommand
module Dhall.DirectoryTree
    ( -- * Filesystem
      toDirectoryTree
    , FilesystemError(..)

      -- * Low-level types and functions
    , module Dhall.DirectoryTree.Types
    , decodeDirectoryTree
    , directoryTreeType
    ) where

import Control.Applicative       (empty)
import Control.Exception         (Exception)
import Control.Monad             (unless, when)
import Data.Either.Validation    (Validation (..))
import Data.Functor.Identity     (Identity (..))
import Data.Maybe                (fromMaybe, isJust)
import Data.Sequence             (Seq)
import Data.Text                 (Text)
import Data.Void                 (Void)
import Dhall.DirectoryTree.Types
import Dhall.Marshal.Decode      (Decoder (..), Expector)
import Dhall.Src                 (Src)
import Dhall.Syntax
    ( Chunks (..)
    , Const (..)
    , Expr (..)
    , RecordField (..)
    , Var (..)
    )
import System.FilePath           ((</>))
import System.PosixCompat.Types  (FileMode, GroupID, UserID)

import qualified Control.Exception           as Exception
import qualified Data.ByteString             as ByteString
import qualified Data.Foldable               as Foldable
import qualified Data.Text                   as Text
import qualified Data.Text.IO                as Text.IO
import qualified Dhall.Core                  as Core
import qualified Dhall.Map                   as Map
import qualified Dhall.Marshal.Decode        as Decode
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck             as TypeCheck
import qualified Dhall.Util                  as Util
import qualified Prettyprinter               as Pretty
import qualified Prettyprinter.Render.String as Pretty
import qualified System.Directory            as Directory
import qualified System.FilePath             as FilePath
#ifdef mingw32_HOST_OS
import System.IO.Error           (illegalOperationErrorType, mkIOError)
#else
import qualified System.Posix.User           as Posix
#endif
import qualified System.PosixCompat.Files    as Posix

{-| Attempt to transform a Dhall record into a directory tree where:

    * Records are translated into directories

    * @Map@s are translated into directory trees, if allowSeparators option is enabled

    * @Text@ values or fields are translated into files

    * @Optional@ values are omitted if @None@

    * There is a more advanced way to construct directory trees using a fixpoint
      encoding. See the documentation below on that.

    For example, the following Dhall record:

    > { dir = { `hello.txt` = "Hello\n" }
    > , `goodbye.txt`= Some "Goodbye\n"
    > , `missing.txt` = None Text
    > }

    ... should translate to this directory tree:

    > $ tree result
    > result
    > ├── dir
    > │   └── hello.txt
    > └── goodbye.txt
    >
    > $ cat result/dir/hello.txt
    > Hello
    >
    > $ cat result/goodbye.txt
    > Goodbye

    Use this in conjunction with the Prelude's support for rendering JSON/YAML
    in "pure Dhall" so that you can generate files containing JSON. For example:

    > let JSON =
    >       https://prelude.dhall-lang.org/v12.0.0/JSON/package.dhall sha256:843783d29e60b558c2de431ce1206ce34bdfde375fcf06de8ec5bf77092fdef7
    >
    > in  { `example.json` =
    >         JSON.render (JSON.array [ JSON.number 1.0, JSON.bool True ])
    >     , `example.yaml` =
    >         JSON.renderYAML
    >           (JSON.object (toMap { foo = JSON.string "Hello", bar = JSON.null }))
    >     }

    ... which would generate:

    > $ cat result/example.json
    > [ 1.0, true ]
    >
    > $ cat result/example.yaml
    > ! "bar": null
    > ! "foo": "Hello"

    /Construction of directory trees from maps/

    In @Map@s, the keys specify paths relative to the work dir.
    Only forward slashes (@/@) must be used as directory separators.
    They will be automatically transformed on Windows.
    Absolute paths (starting with @/@) and parent directory segments (@..@)
    are prohibited for security concerns.

    /Advanced construction of directory trees/

    In addition to the ways described above using "simple" Dhall values to
    construct the directory tree there is one based on a fixpoint encoding. It
    works by passing a value of the following type to the interpreter:

    > let User = < UserId : Natural | UserName : Text >
    >
    > let Group = < GroupId : Natural | GroupName : Text >
    >
    > let Access =
    >       { execute : Optional Bool
    >       , read : Optional Bool
    >       , write : Optional Bool
    >       }
    >
    > let Mode =
    >       { user : Optional Access
    >       , group : Optional Access
    >       , other : Optional Access
    >       }
    >
    > let Entry =
    >       \(content : Type) ->
    >         { name : Text
    >         , content : content
    >         , user : Optional User
    >         , group : Optional Group
    >         , mode : Optional Mode
    >         }
    >
    > in  forall (tree : Type) ->
    >     forall  ( make
    >             : { directory : Entry (List tree) -> tree
    >               , file : Entry Text -> tree
    >               }
    >             ) ->
    >       List tree

    The fact that the metadata for filesystem entries is modeled after the POSIX
    permission model comes with the unfortunate downside that it might not apply
    to other systems: There, changes to the metadata (user, group, permissions)
    might be a no-op and __no warning will be issued__.
    This is a leaking abstraction of the
    [unix-compat](https://hackage.haskell.org/package/unix-compat) package used
    internally.

    __NOTE__: This utility does not take care of type-checking and normalizing
    the provided expression. This will raise a `FilesystemError` exception or a
    `Dhall.Marshal.Decode.DhallErrors` exception upon encountering an expression
    that cannot be converted as-is.
-}
toDirectoryTree
    :: Bool -- ^ Whether to allow path separators in file names or not
    -> FilePath
    -> Expr Void Void
    -> IO ()
toDirectoryTree allowSeparators path expression = case expression of
    RecordLit keyValues ->
        Map.unorderedTraverseWithKey_ process $ recordFieldValue <$> keyValues

    ListLit (Just (App List (Record [ ("mapKey", recordFieldValue -> Text), ("mapValue", _) ]))) [] ->
        Directory.createDirectoryIfMissing allowSeparators path

    ListLit _ records
        | not (null records)
        , Just keyValues <- extract (Foldable.toList records) ->
            Foldable.traverse_ (uncurry process) keyValues

    TextLit (Chunks [] text) ->
        Text.IO.writeFile path text

    Some value ->
        toDirectoryTree allowSeparators path value

    App (Field (Union _) _) value -> do
        toDirectoryTree allowSeparators path value

    App None _ ->
        return ()

    -- If this pattern matches we assume the user wants to use the fixpoint
    -- approach, hence we typecheck it and output error messages like we would
    -- do for every other Dhall program.
    Lam _ _ (Lam _ _ _) -> do
        entries <- decodeDirectoryTree expression

        processFilesystemEntryList allowSeparators path entries

    _ ->
        die
  where
    extract [] =
        return []

    extract (RecordLit [ ("mapKey", recordFieldValue -> TextLit (Chunks [] key))
                       , ("mapValue", recordFieldValue -> value)] : records) =
        fmap ((key, value) :) (extract records)

    extract _ =
        empty

    process key value = do
        -- Fail if path is absolute, which is a security risk.
        when (FilePath.isAbsolute (Text.unpack key)) die
        
        let keyPathSegments =
                fmap Text.unpack $ Text.splitOn "/" key

        case keyPathSegments of
            -- Fail if separators are not allowed by the option.
            _ : _ | not allowSeparators ->
                die
            _ ->
                return ()

        -- Fail if path contains attempts to go to container directory,
        -- which is a security risk.
        if elem ".." keyPathSegments
            then die
            else return ()

        (dirPath, fileName) <- case reverse keyPathSegments of
            h : t ->
                return
                    ( Foldable.foldl' (</>) path (reverse t)
                    , h )
            _ ->
                die

        Directory.createDirectoryIfMissing True dirPath

        toDirectoryTree allowSeparators (dirPath </> fileName) value

    die = Exception.throwIO FilesystemError{..}
      where
        unexpectedExpression = expression

-- | Decode a fixpoint directory tree from a Dhall expression.
decodeDirectoryTree :: Expr s Void -> IO (Seq FilesystemEntry)
decodeDirectoryTree (Core.alphaNormalize . Core.denote -> expression@(Lam _ _ (Lam _ _ body))) = do
    expected' <- case directoryTreeType of
        Success x -> return x
        Failure e -> Exception.throwIO e

    _ <- Core.throws $ TypeCheck.typeOf $ Annot expression expected'

    case Decode.extract decoder body of
        Success x -> return x
        Failure e -> Exception.throwIO e
    where
        decoder :: Decoder (Seq FilesystemEntry)
        decoder = Decode.auto
decodeDirectoryTree expr = Exception.throwIO $ FilesystemError $ Core.denote expr

-- | The type of a fixpoint directory tree expression.
directoryTreeType :: Expector (Expr Src Void)
directoryTreeType = Pi Nothing "tree" (Const Type)
    <$> (Pi Nothing "make" <$> makeType <*> pure (App List (Var (V "tree" 0))))

-- | The type of make part of a fixpoint directory tree expression.
makeType :: Expector (Expr Src Void)
makeType = Record . Map.fromList <$> sequenceA
    [ makeConstructor "directory" (Decode.auto :: Decoder DirectoryEntry)
    , makeConstructor "file" (Decode.auto :: Decoder TextFileEntry)
    , makeConstructor "binary-file" (Decode.auto :: Decoder BinaryFileEntry)
    ]
    where
        makeConstructor :: Text -> Decoder b -> Expector (Text, RecordField Src Void)
        makeConstructor name dec = (name,) . Core.makeRecordField
            <$> (Pi Nothing "_" <$> expected dec <*> pure (Var (V "tree" 0)))

-- | Resolve a `User` to a numerical id.
getUser :: User -> IO UserID
getUser (UserId uid) = return uid
getUser (UserName name) =
#ifdef mingw32_HOST_OS
    ioError $ mkIOError illegalOperationErrorType x Nothing Nothing
    where x = "System.Posix.User.getUserEntryForName: not supported"
#else
    Posix.userID <$> Posix.getUserEntryForName name
#endif

-- | Resolve a `Group` to a numerical id.
getGroup :: Group -> IO GroupID
getGroup (GroupId gid) = return gid
getGroup (GroupName name) =
#ifdef mingw32_HOST_OS
    ioError $ mkIOError illegalOperationErrorType x Nothing Nothing
    where x = "System.Posix.User.getGroupEntryForName: not supported"
#else
    Posix.groupID <$> Posix.getGroupEntryForName name
#endif

-- | Process a `FilesystemEntry`. Writes the content to disk and apply the
-- metadata to the newly created item.
processFilesystemEntry :: Bool -> FilePath -> FilesystemEntry -> IO ()
processFilesystemEntry allowSeparators path (DirectoryEntry entry) =
    processEntryWith path entry $ \path' content -> do
        Directory.createDirectoryIfMissing allowSeparators path'
        processFilesystemEntryList allowSeparators path' content
processFilesystemEntry allowSeparators path (BinaryFileEntry entry) =
    processEntryWith path entry $ \path' content -> do
        when allowSeparators $ do
            Directory.createDirectoryIfMissing True (FilePath.takeDirectory path')
        ByteString.writeFile path' content
processFilesystemEntry allowSeparators path (TextFileEntry entry) =
    processEntryWith path entry $ \path' content -> do
        when allowSeparators $ do
            Directory.createDirectoryIfMissing True (FilePath.takeDirectory path')
        Text.IO.writeFile path' content

-- | A helper function used by 'processFilesystemEntry'.
processEntryWith
    :: FilePath
    -> Entry a
    -> (FilePath -> a -> IO ())
    -> IO ()
processEntryWith path entry f = do
    let path' = path </> entryName entry
    when (hasMetadata entry && not isMetadataSupported) $
        Exception.throwIO (MetadataUnsupportedError path')
    f path' (entryContent entry)
    -- It is important that we write the metadata after we wrote the content of
    -- the file as we might lock ourself out by changing ownership or
    -- permissions.
    applyMetadata entry path'

-- | Process a list of `FilesystemEntry`s.
processFilesystemEntryList :: Bool -> FilePath -> Seq FilesystemEntry -> IO ()
processFilesystemEntryList allowSeparators path = Foldable.traverse_
    (processFilesystemEntry allowSeparators path)

-- | Does this entry have some metadata set?
hasMetadata :: Entry a -> Bool
hasMetadata entry
    =  isJust (entryUser entry)
    || isJust (entryGroup entry)
    || maybe False hasMode (entryMode entry)
    where
        hasMode :: Mode Maybe -> Bool
        hasMode mode
            =  maybe False hasAccess (modeUser mode)
            || maybe False hasAccess (modeGroup mode)
            || maybe False hasAccess (modeOther mode)

        hasAccess :: Access Maybe -> Bool
        hasAccess access
            =  isJust (accessExecute access)
            || isJust (accessRead access)
            || isJust (accessWrite access)

-- | Set the metadata of an object referenced by a path.
applyMetadata :: Entry a -> FilePath -> IO ()
applyMetadata entry fp = do
    s <- Posix.getFileStatus fp
    let user = Posix.fileOwner s
        group = Posix.fileGroup s
        mode = fileModeToMode $ Posix.fileMode s

    user' <- getUser $ fromMaybe (UserId user) (entryUser entry)
    group' <- getGroup $ fromMaybe (GroupId group) (entryGroup entry)
    unless ((user', group') == (user, group)) $
        Posix.setOwnerAndGroup fp user' group'

    let mode' = maybe mode (updateModeWith mode) (entryMode entry)
    unless (mode' == mode) $
        setFileMode fp $ modeToFileMode mode'

-- | Calculate the new `Mode` from the current mode and the changes specified by
-- the user.
updateModeWith :: Mode Identity -> Mode Maybe -> Mode Identity
updateModeWith x y = Mode
    { modeUser = combine modeUser modeUser
    , modeGroup = combine modeGroup modeGroup
    , modeOther = combine modeOther modeOther
    }
    where
        combine f g = maybe (f x) (Identity . updateAccessWith (runIdentity $ f x)) (g y)

-- | Calculate the new `Access` from the current permissions and the changes
-- specified by the user.
updateAccessWith :: Access Identity -> Access Maybe -> Access Identity
updateAccessWith x y = Access
    { accessExecute = combine accessExecute accessExecute
    , accessRead = combine accessRead accessRead
    , accessWrite = combine accessWrite accessWrite
    }
    where
        combine f g = maybe (f x) Identity (g y)

-- | Convert a filesystem mode given as a bitmask (`FileMode`) to an ADT
-- (`Mode`).
fileModeToMode :: FileMode -> Mode Identity
fileModeToMode mode = Mode
    { modeUser = Identity $ Access
        { accessExecute = Identity $ mode `hasFileMode` Posix.ownerExecuteMode
        , accessRead = Identity $ mode `hasFileMode` Posix.ownerReadMode
        , accessWrite = Identity $ mode `hasFileMode` Posix.ownerReadMode
        }
    , modeGroup = Identity $ Access
        { accessExecute = Identity $ mode `hasFileMode` Posix.groupExecuteMode
        , accessRead = Identity $ mode `hasFileMode` Posix.groupReadMode
        , accessWrite = Identity $ mode `hasFileMode` Posix.groupReadMode
        }
    , modeOther = Identity $ Access
        { accessExecute = Identity $ mode `hasFileMode` Posix.otherExecuteMode
        , accessRead = Identity $ mode `hasFileMode` Posix.otherReadMode
        , accessWrite = Identity $ mode `hasFileMode` Posix.otherReadMode
        }
    }

-- | Convert a filesystem mode given as an ADT (`Mode`) to a bitmask
-- (`FileMode`).
modeToFileMode :: Mode Identity -> FileMode
modeToFileMode mode = foldr Posix.unionFileModes Posix.nullFileMode $
    [ Posix.ownerExecuteMode | runIdentity $ accessExecute (runIdentity $ modeUser  mode) ] <>
    [ Posix.ownerReadMode    | runIdentity $ accessRead    (runIdentity $ modeUser  mode) ] <>
    [ Posix.ownerWriteMode   | runIdentity $ accessWrite   (runIdentity $ modeUser  mode) ] <>
    [ Posix.groupExecuteMode | runIdentity $ accessExecute (runIdentity $ modeGroup mode) ] <>
    [ Posix.groupReadMode    | runIdentity $ accessRead    (runIdentity $ modeGroup mode) ] <>
    [ Posix.groupWriteMode   | runIdentity $ accessWrite   (runIdentity $ modeGroup mode) ] <>
    [ Posix.otherExecuteMode | runIdentity $ accessExecute (runIdentity $ modeOther mode) ] <>
    [ Posix.otherReadMode    | runIdentity $ accessRead    (runIdentity $ modeOther mode) ] <>
    [ Posix.otherWriteMode   | runIdentity $ accessWrite   (runIdentity $ modeOther mode) ]

-- | Check whether the second `FileMode` is contained in the first one.
hasFileMode :: FileMode -> FileMode -> Bool
hasFileMode mode x = (mode `Posix.intersectFileModes` x) == x

{- | This error indicates that you supplied an invalid Dhall expression to the
     `toDirectoryTree` function.  The Dhall expression could not be translated
     to a directory tree.
-}
newtype FilesystemError =
    FilesystemError { unexpectedExpression :: Expr Void Void }

instance Exception FilesystemError

instance Show FilesystemError where
    show FilesystemError{..} =
        Pretty.renderString (Dhall.Pretty.layout message)
      where
        message =
          Util._ERROR <> ": Not a valid directory tree expression                             \n\\
          \                                                                                   \n\\
          \Explanation: Only a subset of Dhall expressions can be converted to a directory    \n\\
          \tree.  Specifically, record literals or maps can be converted to directories,      \n\\
          \❰Text❱ literals can be converted to files, and ❰Optional❱ values are included if   \n\\
          \❰Some❱ and omitted if ❰None❱.  Values of union types can also be converted if      \n\\
          \they are an alternative which has a non-nullary constructor whose argument is of   \n\\
          \an otherwise convertible type.  Furthermore, there is a more advanced approach to  \n\\
          \constructing a directory tree utilizing a fixpoint encoding. Consult the upstream  \n\\
          \documentation of the `toDirectoryTree` function in the Dhall.Directory module for  \n\\
          \further information on that.                                                       \n\\
          \No other type of value can be translated to a directory tree.                      \n\\
          \                                                                                   \n\\
          \For example, this is a valid expression that can be translated to a directory      \n\\
          \tree:                                                                              \n\\
          \                                                                                   \n\\
          \                                                                                   \n\\
          \    ┌──────────────────────────────────┐                                           \n\\
          \    │ { `example.json` = \"[1, true]\" } │                                         \n\\
          \    └──────────────────────────────────┘                                           \n\\
          \                                                                                   \n\\
          \                                                                                   \n\\
          \In contrast, the following expression is not allowed due to containing a           \n\\
          \❰Natural❱ field, which cannot be translated in this way:                           \n\\
          \                                                                                   \n\\
          \                                                                                   \n\\
          \    ┌───────────────────────┐                                                      \n\\
          \    │ { `example.txt` = 1 } │                                                      \n\\
          \    └───────────────────────┘                                                      \n\\
          \                                                                                   \n\\
          \                                                                                   \n\\
          \Note that key names cannot contain path separators:                                \n\\
          \                                                                                   \n\\
          \                                                                                   \n\\
          \    ┌─────────────────────────────────────┐                                        \n\\
          \    │ { `directory/example.txt` = \"ABC\" } │ Invalid: Key contains a forward slash\n\\
          \    └─────────────────────────────────────┘                                        \n\\
          \                                                                                   \n\\
          \                                                                                   \n\\
          \Instead, you need to refactor the expression to use nested records instead:        \n\\
          \                                                                                   \n\\
          \                                                                                   \n\\
          \    ┌───────────────────────────────────────────┐                                  \n\\
          \    │ { directory = { `example.txt` = \"ABC\" } } │                                \n\\
          \    └───────────────────────────────────────────┘                                  \n\\
          \                                                                                   \n\\
          \                                                                                   \n\\
          \You tried to translate the following expression to a directory tree:               \n\\
          \                                                                                   \n\\
          \" <> Util.insert unexpectedExpression <> "\n\\
          \                                                                                   \n\\
          \... which is not an expression that can be translated to a directory tree.         \n"

{- | This error indicates that you want to set some metadata for a file or
     directory, but that operation is not supported  on your platform.
-}
newtype MetadataUnsupportedError =
    MetadataUnsupportedError { metadataForPath :: FilePath }

instance Exception MetadataUnsupportedError

instance Show MetadataUnsupportedError where
    show MetadataUnsupportedError{..} =
        Pretty.renderString (Dhall.Pretty.layout message)
      where
        message =
          Util._ERROR <> ": Setting metadata is not supported on this platform.               \n\\
          \                                                                                   \n\\
          \Explanation: Your Dhall expression indicates that you intend to set some metadata  \n\\
          \like ownership or permissions for the following file or directory:                 \n\\
          \                                                                                   \n\\
          \" <> Pretty.pretty metadataForPath <> "\n\\
          \                                                                                   \n\\
          \... which is not supported on your platform.                                       \n"
