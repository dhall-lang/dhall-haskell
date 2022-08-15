{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns       #-}

-- | Implementation of the @dhall to-directory-tree@ subcommand
module Dhall.DirectoryTree
    ( -- * Filesystem
      toDirectoryTree
    , FilesystemError(..)
    ) where

import Control.Applicative      (empty)
import Control.Exception        (Exception)
import Control.Monad            (unless, when)
import Data.Either              (isRight)
import Data.Functor.Identity    (Identity (..))
import Data.Maybe               (fromMaybe)
import Data.Sequence            (Seq)
import Data.Text                (Text)
import Data.Void                (Void)
import Dhall.Syntax
    ( Chunks (..)
    , Expr (..)
    , FieldSelection (..)
    , FunctionBinding (..)
    , RecordField (..)
    , Var (..)
    )
import System.FilePath          ((</>))
import System.PosixCompat.Types (FileMode, GroupID, UserID)

import qualified Control.Exception           as Exception
import qualified Data.Foldable               as Foldable
import qualified Data.Text                   as Text
import qualified Data.Text.IO                as Text.IO
import qualified Dhall.Map                   as Map
import qualified Dhall.Pretty
import qualified Dhall.TH                    as TH
import qualified Dhall.TypeCheck             as TypeCheck
import qualified Dhall.Util                  as Util
import qualified Prettyprinter.Render.String as Pretty
import qualified System.Directory            as Directory
import qualified System.FilePath             as FilePath
import qualified System.PosixCompat.Files    as Posix
import qualified System.PosixCompat.Types    as Posix
import qualified System.PosixCompat.User     as Posix

{-| Attempt to transform a Dhall record into a directory tree where:

    * Records are translated into directories

    * @Map@s are also translated into directories

    * @Text@ values or fields are translated into files

    * @Optional@ values are omitted if @None@

    * There is a more advanced way construction directory trees using a fixpoint
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

    /Advanced construction of directory trees/

    In addition to the ways described above using 'simple' Dhall values to
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
    > in  forall (r : Type) ->
    >     forall  ( make
    >             : { directory : Entry (List r) -> r
    >               , file : Entry Text -> r
    >               }
    >             ) ->
    >       List r

    The fact that the metadata for filesystem entries is modeled after the POSIX
    permission model comes with the unfortunate downside that it might not apply
    to other systems: There, changes to the metadata (user, group, permissions)
    might be a no-op and __no warning will be issued__.
    This is a leaking abstraction of the
    [unix-compat](https://hackage.haskell.org/package/unix-compat) package used
    internally.

    __NOTE__: This utility does not take care of type-checking and normalizing
    the provided expression. This will raise a `FilesystemError` exception upon
    encountering an expression that cannot be converted as-is.
-}
toDirectoryTree
    :: Bool -- ^ Whether to allow path separators in file names or not
    -> FilePath
    -> Expr Void Void
    -> IO ()
toDirectoryTree allowSeparators path expression = case expression of
    RecordLit keyValues ->
        Map.unorderedTraverseWithKey_ process $ recordFieldValue <$> keyValues

    ListLit (Just (Record [ ("mapKey", recordFieldValue -> Text), ("mapValue", _) ])) [] ->
        return ()

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

    Lam _ _ (Lam _ (functionBindingVariable -> make) body)
        | isFixpointedDirectoryTree expression
        -> processFilesystemEntryList allowSeparators path $
            extractFilesystemEntryList make body

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
        when (not allowSeparators && Text.isInfixOf (Text.pack [ FilePath.pathSeparator ]) key) $
            die

        Directory.createDirectoryIfMissing allowSeparators path

        toDirectoryTree allowSeparators (path </> Text.unpack key) value

    die = Exception.throwIO FilesystemError{..}
      where
        unexpectedExpression = expression

-- | Check if an expression is a valid fixpoint directory-tree.
isFixpointedDirectoryTree :: Expr Void Void -> Bool
isFixpointedDirectoryTree expr = isRight $ TypeCheck.typeOf $ Annot expr $
    [TH.dhall|
    let User = < UserId : Natural | UserName : Text >

    let Group = < GroupId : Natural | GroupName : Text >

    let Access =
          { execute : Optional Bool
          , read : Optional Bool
          , write : Optional Bool
          }

    let Mode =
          { user : Optional Access
          , group : Optional Access
          , other : Optional Access
          }

    let Entry =
          \(content : Type) ->
            { name : Text
            , content : content
            , user : Optional User
            , group : Optional Group
            , mode : Optional Mode
            }

    in  forall (r : Type) ->
        forall  ( make
                : { directory : Entry (List r) -> r
                  , file : Entry Text -> r
                  }
                ) ->
          List r
    |]

-- | A filesystem entry.
data FilesystemEntry
    = DirectoryEntry (Entry (Seq FilesystemEntry))
    | FileEntry (Entry Text)
    deriving Show

-- | Extract a `FilesystemEntry` from an expression.
extractFilesystemEntry :: Text -> Expr Void Void -> FilesystemEntry
extractFilesystemEntry make (App (Field (Var (V make' 0)) (fieldSelectionLabel -> label)) entry)
    | make' == make
    , label == "directory" = DirectoryEntry $ extractEntry (extractList (extractFilesystemEntry make)) entry
    | make' == make
    , label == "file" = FileEntry $ extractEntry extractText entry
extractFilesystemEntry _ expr = Exception.throw (FilesystemError expr)

-- | Extract a list of `FilesystemEntry`s from an expression.
extractFilesystemEntryList :: Text -> Expr Void Void -> Seq FilesystemEntry
extractFilesystemEntryList make = extractList (extractFilesystemEntry make)

-- | A generic filesystem entry parameterized over the content.
data Entry a = Entry
    { entryName :: String
    , entryContent :: a
    , entryUser :: Maybe User
    , entryGroup :: Maybe Group
    , entryMode :: Maybe (Mode Maybe)
    }
    deriving Show

-- | Extract an `Entry` from an expression.
extractEntry :: (Expr Void Void -> a) -> Expr Void Void -> Entry a
extractEntry extractContent (RecordLit (Map.toList ->
    [ ("content", recordFieldValue -> contentExpr)
    , ("group", recordFieldValue -> groupExpr)
    , ("mode", recordFieldValue -> modeExpr)
    , ("name", recordFieldValue -> nameExpr)
    , ("user", recordFieldValue -> userExpr)
    ])) = Entry
    { entryName = extractString nameExpr
    , entryContent = extractContent contentExpr
    , entryUser = extractMaybe extractUser userExpr
    , entryGroup = extractMaybe extractGroup groupExpr
    , entryMode = extractMaybe extractMode modeExpr
    }
extractEntry _ expr = Exception.throw (FilesystemError expr)

-- | A user identified either by id or name.
data User
    = UserId UserID
    | UserName String
    deriving Show

pattern UserP :: Text -> Expr Void Void -> Expr Void Void
pattern UserP label v <- App (Field (Union (Map.toList ->
    [ ("UserId", Just Natural)
    , ("UserName",Just Text)]))
    (fieldSelectionLabel -> label))
    v

-- | Extract a `User` from an expression.
extractUser :: Expr Void Void -> User
extractUser (UserP "UserId" (NaturalLit n)) = UserId $ Posix.CUid (fromIntegral n)
extractUser (UserP "UserName" (TextLit (Chunks [] text))) = UserName $ Text.unpack text
extractUser expr = Exception.throw (FilesystemError expr)

-- | Resolve a `User` to a numerical id.
getUser :: User -> IO UserID
getUser (UserId uid) = return uid
getUser (UserName name) = Posix.userID <$> Posix.getUserEntryForName name

-- | A group identified either by id or name.
data Group
    = GroupId GroupID
    | GroupName String
    deriving Show

pattern GroupP :: Text -> Expr Void Void -> Expr Void Void
pattern GroupP label v <- App (Field (Union (Map.toList ->
    [ ("GroupId", Just Natural)
    , ("GroupName", Just Text)]))
    (fieldSelectionLabel -> label))
    v

-- | Extract a `Group` from an expression.
extractGroup :: Expr Void Void -> Group
extractGroup (GroupP "GroupId" (NaturalLit n)) = GroupId $ Posix.CGid (fromIntegral n)
extractGroup (GroupP "GroupName" (TextLit (Chunks [] text))) = GroupName $ Text.unpack text
extractGroup expr = Exception.throw (FilesystemError expr)

-- | Resolve a `Group` to a numerical id.
getGroup :: Group -> IO GroupID
getGroup (GroupId gid) = return gid
getGroup (GroupName name) = Posix.groupID <$> Posix.getGroupEntryForName name

-- | A filesystem mode. See chmod(1).
-- The parameter is meant to be instantiated by either `Identity` or `Maybe`
-- depending on the completeness of the information.
data Mode f = Mode
    { modeUser :: f (Access f)
    , modeGroup :: f (Access f)
    , modeOther :: f (Access f)
    }

deriving instance Eq (Mode Identity)
deriving instance Eq (Mode Maybe)
deriving instance Show (Mode Identity)
deriving instance Show (Mode Maybe)

-- | Extract a `Mode` from an expression.
extractMode :: Expr Void Void -> Mode Maybe
extractMode (RecordLit (Map.toList ->
    [ ("group", recordFieldValue -> groupExpr)
    , ("other", recordFieldValue -> otherExpr)
    , ("user", recordFieldValue -> userExpr)
    ])) = Mode
    { modeUser = extractMaybe extractAccess userExpr
    , modeGroup = extractMaybe extractAccess groupExpr
    , modeOther = extractMaybe extractAccess otherExpr
    }
extractMode expr = Exception.throw (FilesystemError expr)

-- | The permissions for a subject (user/group/other).
data Access f = Access
    { accessExecute :: f Bool
    , accessRead :: f Bool
    , accessWrite :: f Bool
    }

deriving instance Eq (Access Identity)
deriving instance Eq (Access Maybe)
deriving instance Show (Access Identity)
deriving instance Show (Access Maybe)

-- | Extract a `Access` from an expression.
extractAccess :: Expr Void Void -> Access Maybe
extractAccess (RecordLit (Map.toList ->
    [ ("execute", recordFieldValue -> executeExpr)
    , ("read", recordFieldValue -> readExpr)
    , ("write", recordFieldValue -> writeExpr)
    ])) = Access
    { accessExecute = extractMaybe extractBool executeExpr
    , accessRead = extractMaybe extractBool readExpr
    , accessWrite = extractMaybe extractBool writeExpr
    }
extractAccess expr = Exception.throw (FilesystemError expr)

-- | Helper function to extract a `Bool` value.
extractBool :: Expr Void Void -> Bool
extractBool (BoolLit b) = b
extractBool expr = Exception.throw (FilesystemError expr)

-- | Helper function to extract a list of some values.
-- The first argument is used to extract the items.
extractList :: (Expr Void Void -> a) -> Expr Void Void -> Seq a
extractList _ (ListLit (Just _) _) = mempty
extractList f (ListLit _ xs) = fmap f xs
extractList _ expr = Exception.throw (FilesystemError expr)

-- | Helper function to extract optional values.
-- The first argument is used to extract the items.
extractMaybe :: (Expr Void Void -> a) -> Expr Void Void -> Maybe a
extractMaybe _ (App None _) = Nothing
extractMaybe f (Some expr) = Just (f expr)
extractMaybe _ expr = Exception.throw (FilesystemError expr)

-- | Helper function to extract a `String` value.
extractString :: Expr Void Void -> String
extractString = Text.unpack . extractText

-- | Helper function to extract a `Text` value.
extractText :: Expr Void Void -> Text
extractText (TextLit (Chunks [] text)) = text
extractText expr = Exception.throw (FilesystemError expr)

-- | Process a `FilesystemEntry`. Writes the content to disk and apply the
-- metadata to the newly created item.
processFilesystemEntry :: Bool -> FilePath -> FilesystemEntry -> IO ()
processFilesystemEntry allowSeparators path (DirectoryEntry entry) = do
    let path' = path </> entryName entry
    Directory.createDirectoryIfMissing allowSeparators path'
    processFilesystemEntryList allowSeparators path' $ entryContent entry
    -- It is important that we write the metadata after we wrote the content of
    -- the directories/files below this directory as we might lock ourself out
    -- by changing ownership or permissions.
    applyMetadata entry path'
processFilesystemEntry _ path (FileEntry entry) = do
    let path' = path </> entryName entry
    Text.IO.writeFile path' $ entryContent entry
    -- It is important that we write the metadata after we wrote the content of
    -- the file as we might lock ourself out by changing ownership or
    -- permissions.
    applyMetadata entry path'

-- | Process a list of `FilesystemEntry`s.
processFilesystemEntryList :: Bool -> FilePath -> Seq FilesystemEntry -> IO ()
processFilesystemEntryList allowSeparators path = Foldable.traverse_
    (processFilesystemEntry allowSeparators path)

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
        Posix.setFileMode fp $ modeToFileMode mode'

-- | Calculate the new `Mode` from the current mode and the changes specified by the user.
updateModeWith :: Mode Identity -> Mode Maybe -> Mode Identity
updateModeWith x y = Mode
    { modeUser = combine modeUser modeUser
    , modeGroup = combine modeGroup modeGroup
    , modeOther = combine modeOther modeOther
    }
    where
        combine f g = maybe (f x) (Identity . updateAccessWith (runIdentity $ f x)) (g y)

-- | Calculate the new `Access` from the current permissions and the changes specified by the user.
updateAccessWith :: Access Identity -> Access Maybe -> Access Identity
updateAccessWith x y = Access
    { accessExecute = combine accessExecute accessExecute
    , accessRead = combine accessRead accessRead
    , accessWrite = combine accessWrite accessWrite
    }
    where
        combine f g = maybe (f x) Identity (g y)

-- | Convert a filesystem mode given as a bitmask (`FileMode`) to an ADT (`Mode`).
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

-- | Convert a filesystem mode given as an ADT (`Mode`) to a bitmask (`FileMode`).
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

instance Show FilesystemError where
    show FilesystemError{..} =
        Pretty.renderString (Dhall.Pretty.layout message)
      where
        message =
          Util._ERROR <> ": Not a valid directory tree expression                             \n\
          \                                                                                   \n\
          \Explanation: Only a subset of Dhall expressions can be converted to a directory    \n\
          \tree.  Specifically, record literals or maps can be converted to directories,      \n\
          \❰Text❱ literals can be converted to files, and ❰Optional❱ values are included if   \n\
          \❰Some❱ and omitted if ❰None❱.  Values of union types can also be converted if      \n\
          \they are an alternative which has a non-nullary constructor whose argument is of   \n\
          \an otherwise convertible type.  Furthermore, there is a more advanced approach to  \n\
          \constructing a directory tree utilizing a fixpoint encoding. Consult the upstream  \n\
          \documentation of the `toDirectoryTree` function in the Dhall.Directory module for  \n\
          \further information on that.                                                       \n\
          \No other type of value can be translated to a directory tree.                      \n\
          \                                                                                   \n\
          \For example, this is a valid expression that can be translated to a directory      \n\
          \tree:                                                                              \n\
          \                                                                                   \n\
          \                                                                                   \n\
          \    ┌──────────────────────────────────┐                                           \n\
          \    │ { `example.json` = \"[1, true]\" } │                                         \n\
          \    └──────────────────────────────────┘                                           \n\
          \                                                                                   \n\
          \                                                                                   \n\
          \In contrast, the following expression is not allowed due to containing a           \n\
          \❰Natural❱ field, which cannot be translated in this way:                           \n\
          \                                                                                   \n\
          \                                                                                   \n\
          \    ┌───────────────────────┐                                                      \n\
          \    │ { `example.txt` = 1 } │                                                      \n\
          \    └───────────────────────┘                                                      \n\
          \                                                                                   \n\
          \                                                                                   \n\
          \Note that key names cannot contain path separators:                                \n\
          \                                                                                   \n\
          \                                                                                   \n\
          \    ┌─────────────────────────────────────┐                                        \n\
          \    │ { `directory/example.txt` = \"ABC\" } │ Invalid: Key contains a forward slash\n\
          \    └─────────────────────────────────────┘                                        \n\
          \                                                                                   \n\
          \                                                                                   \n\
          \Instead, you need to refactor the expression to use nested records instead:        \n\
          \                                                                                   \n\
          \                                                                                   \n\
          \    ┌───────────────────────────────────────────┐                                  \n\
          \    │ { directory = { `example.txt` = \"ABC\" } } │                                \n\
          \    └───────────────────────────────────────────┘                                  \n\
          \                                                                                   \n\
          \                                                                                   \n\
          \You tried to translate the following expression to a directory tree:               \n\
          \                                                                                   \n\
          \" <> Util.insert unexpectedExpression <> "\n\
          \                                                                                   \n\
          \... which is not an expression that can be translated to a directory tree.         \n"

instance Exception FilesystemError
