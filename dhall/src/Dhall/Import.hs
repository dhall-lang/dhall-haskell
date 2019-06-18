{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# OPTIONS_GHC -Wall #-}

{-| Dhall lets you import external expressions located either in local files or
    hosted on network endpoints.

    To import a local file as an expression, just insert the path to the file,
    prepending a @./@ if the path is relative to the current directory.  For
    example, if you create a file named @id@ with the following contents:

    > $ cat id
    > λ(a : Type) → λ(x : a) → x

    Then you can use the file directly within a @dhall@ program just by
    referencing the file's path:

    > $ dhall
    > ./id Bool True
    > <Ctrl-D>
    > Bool
    >
    > True

    Imported expressions may contain imports of their own, too, which will
    continue to be resolved.  However, Dhall will prevent cyclic imports.  For
    example, if you had these two files:

    > $ cat foo
    > ./bar

    > $ cat bar
    > ./foo

    ... Dhall would throw the following exception if you tried to import @foo@:

    > $ dhall
    > ./foo
    > ^D
    > ↳ ./foo
    >   ↳ ./bar
    >
    > Cyclic import: ./foo

    You can also import expressions hosted on network endpoints.  Just use the
    URL

    > http://host[:port]/path

    The compiler expects the downloaded expressions to be in the same format
    as local files, specifically UTF8-encoded source code text.

    For example, if our @id@ expression were hosted at @http://example.com/id@,
    then we would embed the expression within our code using:

    > http://example.com/id

    You can also import expressions stored within environment variables using
    @env:NAME@, where @NAME@ is the name of the environment variable.  For
    example:

    > $ export FOO=1
    > $ export BAR='"Hi"'
    > $ export BAZ='λ(x : Bool) → x == False'
    > $ dhall <<< "{ foo = env:FOO , bar = env:BAR , baz = env:BAZ }"
    > { bar : Text, baz : ∀(x : Bool) → Bool, foo : Integer }
    >
    > { bar = "Hi", baz = λ(x : Bool) → x == False, foo = 1 }

    If you wish to import the raw contents of an impoert as @Text@ then add
    @as Text@ to the end of the import:

    > $ dhall <<< "http://example.com as Text"
    > Text
    >
    > "<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n\n    <meta
    >  charset=\"utf-8\" />\n    <meta http-equiv=\"Content-type\" content=\"text/html
    > ; charset=utf-8\" />\n    <meta name=\"viewport\" content=\"width=device-width,
    > initial-scale=1\" />\n    <style type=\"text/css\">\n    body {\n        backgro
    > und-color: #f0f0f2;\n        margin: 0;\n        padding: 0;\n        font-famil
    > y: \"Open Sans\", \"Helvetica Neue\", Helvetica, Arial, sans-serif;\n        \n
    >    }\n    div {\n        width: 600px;\n        margin: 5em auto;\n        paddi
    > ng: 50px;\n        background-color: #fff;\n        border-radius: 1em;\n    }\n
    >     a:link, a:visited {\n        color: #38488f;\n        text-decoration: none;
    > \n    }\n    @media (max-width: 700px) {\n        body {\n            background
    > -color: #fff;\n        }\n        div {\n            width: auto;\n            m
    > argin: 0 auto;\n            border-radius: 0;\n            padding: 1em;\n
    >   }\n    }\n    </style>    \n</head>\n\n<body>\n<div>\n    <h1>Example Domain</
    > h1>\n    <p>This domain is established to be used for illustrative examples in d
    > ocuments. You may use this\n    domain in examples without prior coordination or
    >  asking for permission.</p>\n    <p><a href=\"http://www.iana.org/domains/exampl
    > e\">More information...</a></p>\n</div>\n</body>\n</html>\n"
-}

module Dhall.Import (
    -- * Import
      exprFromImport
    , exprToImport
    , load
    , loadWith
    , hashExpression
    , hashExpressionToCode
    , assertNoImports
    , Status
    , emptyStatus
    , stack
    , cache
    , manager
    , standardVersion
    , normalizer
    , startingContext
    , resolver
    , cacher
    , Cycle(..)
    , ReferentiallyOpaque(..)
    , Imported(..)
    , ImportResolutionDisabled(..)
    , PrettyHttpException(..)
    , MissingFile(..)
    , MissingEnvironmentVariable(..)
    , MissingImports(..)
    , HashMismatch(..)
    ) where

import Control.Applicative (Alternative(..))
import Codec.CBOR.Term (Term(..))
import Control.Exception (Exception, SomeException, throwIO, toException)
import Control.Monad (guard)
import Control.Monad.Catch (throwM, MonadCatch(catch), catches, Handler(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.State.Strict (StateT)
import Crypto.Hash (SHA256)
import Data.CaseInsensitive (CI)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
#if MIN_VERSION_base(4,8,0)
#else
import Data.Traversable (traverse)
#endif
import Data.Typeable (Typeable)
import System.FilePath ((</>))
import Dhall.Binary (StandardVersion(..))
import Dhall.Core
    ( Expr(..)
    , Binding(..)
    , Chunks(..)
    , Directory(..)
    , File(..)
    , FilePrefix(..)
    , ImportHashed(..)
    , ImportType(..)
    , ImportMode(..)
    , Import(..)
    , URL(..)
    )
#ifdef MIN_VERSION_http_client
import Dhall.Import.HTTP
#endif
import Dhall.Import.Types
import Text.Dot ((.->.), userNodeId)

import Dhall.Parser (Parser(..), ParseError(..), Src(..), SourcedException(..))
import Dhall.TypeCheck (X(..))
import Lens.Family.State.Strict (zoom)

import qualified Codec.Serialise
import qualified Control.Monad.Trans.Maybe        as Maybe
import qualified Control.Monad.Trans.State.Strict as State
import qualified Crypto.Hash
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.CaseInsensitive
import qualified Data.Foldable
import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Map.Strict                  as Map
import qualified Data.Text.Encoding
import qualified Data.Text                        as Text
import qualified Data.Text.IO
import qualified Dhall.Binary
import qualified Dhall.Core
import qualified Dhall.Map
import qualified Dhall.Parser
import qualified Dhall.Pretty.Internal
import qualified Dhall.TypeCheck
import qualified System.Environment
import qualified System.Directory                 as Directory
import qualified System.FilePath                  as FilePath
import qualified Text.Megaparsec
import qualified Text.Parser.Combinators
import qualified Text.Parser.Token

-- | An import failed because of a cycle in the import graph
newtype Cycle = Cycle
    { cyclicImport :: Import  -- ^ The offending cyclic import
    }
  deriving (Typeable)

instance Exception Cycle

instance Show Cycle where
    show (Cycle import_) =
        "\nCyclic import: " ++ Dhall.Pretty.Internal.prettyToString import_

{-| Dhall tries to ensure that all expressions hosted on network endpoints are
    weakly referentially transparent, meaning roughly that any two clients will
    compile the exact same result given the same URL.

    To be precise, a strong interpretaton of referential transparency means that
    if you compiled a URL you could replace the expression hosted at that URL
    with the compiled result.  Let's call this \"static linking\".  Dhall (very
    intentionally) does not satisfy this stronger interpretation of referential
    transparency since \"statically linking\" an expression (i.e. permanently
    resolving all imports) means that the expression will no longer update if
    its dependencies change.

    In general, either interpretation of referential transparency is not
    enforceable in a networked context since one can easily violate referential
    transparency with a custom DNS, but Dhall can still try to guard against
    common unintentional violations.  To do this, Dhall enforces that a
    non-local import may not reference a local import.

    Local imports are defined as:

    * A file

    * A URL with a host of @localhost@ or @127.0.0.1@

    All other imports are defined to be non-local
-}
newtype ReferentiallyOpaque = ReferentiallyOpaque
    { opaqueImport :: Import  -- ^ The offending opaque import
    } deriving (Typeable)

instance Exception ReferentiallyOpaque

instance Show ReferentiallyOpaque where
    show (ReferentiallyOpaque import_) =
        "\nReferentially opaque import: " ++ Dhall.Pretty.Internal.prettyToString import_

-- | Extend another exception with the current import stack
data Imported e = Imported
    { importStack :: NonEmpty Import -- ^ Imports resolved so far, in reverse order
    , nested      :: e               -- ^ The nested exception
    } deriving (Typeable)

instance Exception e => Exception (Imported e)

instance Show e => Show (Imported e) where
    show (Imported canonicalizedImports e) =
           concat (zipWith indent [0..] toDisplay)
        ++ "\n"
        ++ show e
      where
        indent n import_ =
            "\n" ++ replicate (2 * n) ' ' ++ "↳ " ++ Dhall.Pretty.Internal.prettyToString import_

        canonical = NonEmpty.toList canonicalizedImports

        -- Tthe final (outermost) import is fake to establish the base
        -- directory. Also, we need outermost-first.
        toDisplay = drop 1 (reverse canonical)

-- | Exception thrown when an imported file is missing
data MissingFile = MissingFile FilePath
    deriving (Typeable)

instance Exception MissingFile

instance Show MissingFile where
    show (MissingFile path) =
            "\n"
        <>  "\ESC[1;31mError\ESC[0m: Missing file "
        <>  path

-- | Exception thrown when an environment variable is missing
newtype MissingEnvironmentVariable = MissingEnvironmentVariable { name :: Text }
    deriving (Typeable)

instance Exception MissingEnvironmentVariable

instance Show MissingEnvironmentVariable where
    show (MissingEnvironmentVariable {..}) =
            "\n"
        <>  "\ESC[1;31mError\ESC[0m: Missing environment variable\n"
        <>  "\n"
        <>  "↳ " <> Text.unpack name

-- | List of Exceptions we encounter while resolving Import Alternatives
newtype MissingImports = MissingImports [SomeException]

instance Exception MissingImports

instance Show MissingImports where
    show (MissingImports []) =
            "\n"
        <>  "\ESC[1;31mError\ESC[0m: No valid imports"
    show (MissingImports [e]) = show e
    show (MissingImports es) =
            "\n"
        <>  "\ESC[1;31mError\ESC[0m: Failed to resolve imports. Error list:"
        <>  "\n"
        <>  concatMap (\e -> "\n" <> show e <> "\n") es

throwMissingImport :: (MonadCatch m, Exception e) => e -> m a
throwMissingImport e = throwM (MissingImports [toException e])

-- | Exception thrown when a HTTP url is imported but dhall was built without
-- the @with-http@ Cabal flag.
data CannotImportHTTPURL =
    CannotImportHTTPURL
        String
        (Maybe [(CI Data.ByteString.ByteString, Data.ByteString.ByteString)])
    deriving (Typeable)

instance Exception CannotImportHTTPURL

instance Show CannotImportHTTPURL where
    show (CannotImportHTTPURL url _mheaders) =
            "\n"
        <>  "\ESC[1;31mError\ESC[0m: Cannot import HTTP URL.\n"
        <>  "\n"
        <>  "Dhall was compiled without the 'with-http' flag.\n"
        <>  "\n"
        <>  "The requested URL was: "
        <>  url
        <>  "\n"

{-|
> canonicalize . canonicalize = canonicalize

> canonicalize (a <> b) = canonicalize a <> canonicalize b
-}
class Semigroup path => Canonicalize path where
    canonicalize :: path -> path

-- |
-- >>> canonicalize (Directory {components = ["..",".."]})
-- Directory {components = ["..",".."]}
instance Canonicalize Directory where
    canonicalize (Directory []) = Directory []

    canonicalize (Directory ("." : components₀)) =
        canonicalize (Directory components₀)

    canonicalize (Directory (".." : components₀)) =
        case canonicalize (Directory components₀) of
            Directory [] ->
                Directory [ ".." ]
            Directory (".." : components₁) ->
                Directory (".." : ".." : components₁)
            Directory (_    : components₁) ->
                Directory components₁

    canonicalize (Directory (component : components₀)) =
        Directory (component : components₁)
      where
        Directory components₁ = canonicalize (Directory components₀)

instance Canonicalize File where
    canonicalize (File { directory, .. }) =
        File { directory = canonicalize directory, .. }

instance Canonicalize ImportType where
    canonicalize (Local prefix file) =
        Local prefix (canonicalize file)

    canonicalize (Remote (URL {..})) =
        Remote (URL { path = canonicalize path, headers = fmap (fmap canonicalize) headers, ..})

    canonicalize (Env name) =
        Env name

    canonicalize Missing =
        Missing

instance Canonicalize ImportHashed where
    canonicalize (ImportHashed hash importType) =
        ImportHashed hash (canonicalize importType)

instance Canonicalize Import where
    canonicalize (Import importHashed importMode) =
        Import (canonicalize importHashed) importMode

toHeaders
  :: Expr s a
  -> Maybe [(CI Data.ByteString.ByteString, Data.ByteString.ByteString)]
toHeaders (ListLit _ hs) = do
    hs' <- mapM toHeader hs
    return (Data.Foldable.toList hs')
toHeaders  _             = do
    empty

toHeader
  :: Expr s a
  -> Maybe (CI Data.ByteString.ByteString, Data.ByteString.ByteString)
toHeader (RecordLit m) = do
    TextLit (Chunks [] keyText  ) <- Dhall.Map.lookup "header" m
    TextLit (Chunks [] valueText) <- Dhall.Map.lookup "value"  m
    let keyBytes   = Data.Text.Encoding.encodeUtf8 keyText
    let valueBytes = Data.Text.Encoding.encodeUtf8 valueText
    return (Data.CaseInsensitive.mk keyBytes, valueBytes)
toHeader _ = do
    empty


-- | Exception thrown when an integrity check fails
data HashMismatch = HashMismatch
    { expectedHash :: Crypto.Hash.Digest SHA256
    , actualHash   :: Crypto.Hash.Digest SHA256
    } deriving (Typeable)

instance Exception HashMismatch

instance Show HashMismatch where
    show (HashMismatch {..}) =
            "\n"
        <>  "\ESC[1;31mError\ESC[0m: Import integrity check failed\n"
        <>  "\n"
        <>  "Expected hash:\n"
        <>  "\n"
        <>  "↳ " <> show expectedHash <> "\n"
        <>  "\n"
        <>  "Actual hash:\n"
        <>  "\n"
        <>  "↳ " <> show actualHash <> "\n"

localToPath :: MonadIO io => FilePrefix -> File -> io FilePath
localToPath prefix file_ = liftIO $ do
    let File {..} = file_

    let Directory {..} = directory

    prefixPath <- case prefix of
        Home -> do
            Directory.getHomeDirectory

        Absolute -> do
            return "/"

        Parent -> do
            pwd <- Directory.getCurrentDirectory
            return (FilePath.takeDirectory pwd)

        Here -> do
            Directory.getCurrentDirectory

    let cs = map Text.unpack (file : components)

    let cons component dir = dir </> component

    return (foldr cons prefixPath cs)

-- | Parse an expression from a `Import` containing a Dhall program
exprFromImport :: Import -> StateT (Status IO) IO Resolved
exprFromImport here@(Import {..}) = do
    let ImportHashed {..} = importHashed

    Status {..} <- State.get

    result <- Maybe.runMaybeT $ do
        Just expectedHash <- return hash
        cacheFile         <- getCacheFile expectedHash
        True              <- liftIO (Directory.doesFileExist cacheFile)

        bytesStrict <- liftIO (Data.ByteString.readFile cacheFile)

        let actualHash = Crypto.Hash.hash bytesStrict

        if expectedHash == actualHash
            then return ()
            else throwMissingImport (Imported _stack (HashMismatch {..}))

        let bytesLazy = Data.ByteString.Lazy.fromStrict bytesStrict

        term <- Dhall.Core.throws (Codec.Serialise.deserialiseOrFail bytesLazy)

        Dhall.Core.throws (Dhall.Binary.decodeExpression term)

    case result of
        Just resolvedExpression -> do
            let newImport = here

            return (Resolved {..})
        Nothing -> do
            exprFromUncachedImport here

{-| Save an expression to the specified `Import`

    Currently this only works for cached imports and ignores other types of
    imports, but could conceivably work for uncached imports in the future

    The main reason for this more general type is for symmetry with
    `exprFromImport` and to support doing more clever things in the future,
    like doing \"the right thing\" for uncached imports (i.e. exporting
    environment variables or creating files)
-}
exprToImport :: Import -> Expr Src X -> StateT (Status IO) IO ()
exprToImport here expression = do
    Status {..} <- State.get

    let Import {..} = here

    let ImportHashed {..} = importHashed

    _ <- Maybe.runMaybeT $ do
        Just expectedHash  <- return hash
        cacheFile          <- getCacheFile expectedHash

        _ <- Dhall.Core.throws (Dhall.TypeCheck.typeWith _startingContext expression)

        let normalizedExpression =
                Dhall.Core.alphaNormalize
                    (Dhall.Core.normalizeWith
                        _normalizer
                        expression
                    )

        let check version = do
                let bytes = encodeExpression version normalizedExpression

                let actualHash = Crypto.Hash.hash bytes

                guard (expectedHash == actualHash)

                liftIO (Data.ByteString.writeFile cacheFile bytes)

        let fallback = do
                let actualHash = hashExpression NoVersion normalizedExpression

                throwMissingImport (Imported _stack (HashMismatch {..}))

        Data.Foldable.asum (map check [ minBound .. maxBound ]) <|> fallback

    return ()

getCacheFile
    :: (Alternative m, MonadIO m) => Crypto.Hash.Digest SHA256 -> m FilePath
getCacheFile hash = do
    let assertDirectory directory = do
            let private = transform Directory.emptyPermissions
                  where
                    transform =
                            Directory.setOwnerReadable   True
                        .   Directory.setOwnerWritable   True
                        .   Directory.setOwnerSearchable True

            let accessible path =
                       Directory.readable   path
                    && Directory.writable   path
                    && Directory.searchable path

            directoryExists <- liftIO (Directory.doesDirectoryExist directory)

            if directoryExists
                then do
                    permissions <- liftIO (Directory.getPermissions directory)

                    guard (accessible permissions)

                else do
                    assertDirectory (FilePath.takeDirectory directory)

                    liftIO (Directory.createDirectory directory)

                    liftIO (Directory.setPermissions directory private)

    cacheDirectory <- getCacheDirectory

    let dhallDirectory = cacheDirectory </> "dhall"

    assertDirectory dhallDirectory

    let cacheFile = dhallDirectory </> ("1220" <> show hash)

    return cacheFile

getCacheDirectory :: (Alternative m, MonadIO m) => m FilePath
getCacheDirectory = alternative₀ <|> alternative₁
  where
    alternative₀ = do
        maybeXDGCacheHome <- do
          liftIO (System.Environment.lookupEnv "XDG_CACHE_HOME")

        case maybeXDGCacheHome of
            Just xdgCacheHome -> return xdgCacheHome
            Nothing           -> empty

    alternative₁ = do
        maybeHomeDirectory <- liftIO (System.Environment.lookupEnv "HOME")

        case maybeHomeDirectory of
            Just homeDirectory -> return (homeDirectory </> ".cache")
            Nothing            -> empty

exprFromUncachedImport :: Import -> StateT (Status IO) IO Resolved
exprFromUncachedImport import_@(Import {..}) = do
    let ImportHashed {..} = importHashed

    (path, text, newImport) <- case importType of
        Local prefix file -> liftIO $ do
            path   <- localToPath prefix file
            exists <- Directory.doesFileExist path

            if exists
                then return ()
                else throwMissingImport (MissingFile path)

            text <- Data.Text.IO.readFile path

            return (path, text, import_)

        Remote url@URL { headers = maybeHeadersExpression } -> do
            maybeHeadersAndExpression <- case maybeHeadersExpression of
                Nothing -> do
                    return Nothing
                Just headersExpression -> do
                    expr <- loadWith headersExpression

                    let expected :: Expr Src X
                        expected =
                            App List
                                ( Record
                                    ( Dhall.Map.fromList
                                        [("header", Text), ("value", Text)]
                                    )
                                )
                    let suffix_ = Dhall.Pretty.Internal.prettyToStrictText expected
                    let annot = case expr of
                            Note (Src begin end bytes) _ ->
                                Note (Src begin end bytes') (Annot expr expected)
                              where
                                bytes' = bytes <> " : " <> suffix_
                            _ ->
                                Annot expr expected

                    case Dhall.TypeCheck.typeOf annot of
                        Left err -> liftIO (throwIO err)
                        Right _  -> return ()

                    let expr' = Dhall.Core.normalize expr

                    case toHeaders expr' of
                        Just headers -> do
                            return (Just (headers, expr'))
                        Nothing      -> do
                            liftIO (throwIO InternalError)

#ifdef MIN_VERSION_http_client
            let maybeHeaders = fmap fst maybeHeadersAndExpression

            let newHeaders =
                    fmap (fmap absurd . snd) maybeHeadersAndExpression

            (path, text) <- fetchFromHttpUrl url maybeHeaders

            let newImport = Import
                    { importHashed = ImportHashed
                        { importType =
                            Remote (url { headers = newHeaders })
                        , ..
                        }
                    , ..
                    }

            return (path, text, newImport)
#else
            let urlString = Text.unpack (Dhall.Core.pretty url)

            liftIO (throwIO (CannotImportHTTPURL urlString mheaders))
#endif

        Env env -> liftIO $ do
            x <- System.Environment.lookupEnv (Text.unpack env)
            case x of
                Just string -> do
                    return (Text.unpack env, Text.pack string, import_)
                Nothing -> do
                    throwMissingImport (MissingEnvironmentVariable env)

        Missing -> liftIO $ do
            throwM (MissingImports [])

    case importMode of
        Code -> do
            let parser = unParser $ do
                    Text.Parser.Token.whiteSpace
                    r <- Dhall.Parser.expr
                    Text.Parser.Combinators.eof
                    return r

            case Text.Megaparsec.parse parser path text of
                Left errInfo -> do
                    liftIO (throwIO (ParseError errInfo text))
                Right resolvedExpression -> do
                    return (Resolved {..})

        RawText -> do
            let resolvedExpression = TextLit (Chunks [] text)

            return (Resolved {..})

-- | Default starting `Status`, importing relative to the given directory.
emptyStatus :: FilePath -> Status IO
emptyStatus = emptyStatusWith exprFromImport exprToImport

{-| Generalized version of `load`

    You can configure the desired behavior through the initial `Status` that you
    supply
-}
loadWith :: MonadCatch m => Expr Src Import -> StateT (Status m) m (Expr Src X)
loadWith expr₀ = case expr₀ of
  Embed import₀ -> do
    Status {..} <- State.get

    let parent = NonEmpty.head _stack

    let import₁ = parent <> import₀

    let child = canonicalize import₁

    let local (Import (ImportHashed _ (Remote  {})) _) = False
        local (Import (ImportHashed _ (Local   {})) _) = True
        local (Import (ImportHashed _ (Env     {})) _) = True
        local (Import (ImportHashed _ (Missing {})) _) = True

    let referentiallySane = not (local child) || local parent

    if referentiallySane
        then return ()
        else throwMissingImport (Imported _stack (ReferentiallyOpaque import₀))

    let _stack' = NonEmpty.cons child _stack

    expr <- if child `elem` _stack
        then throwMissingImport (Imported _stack (Cycle import₀))
        else do
            case Map.lookup child _cache of
                Just (childNode, expr) -> do
                    zoom dot . State.modify $ \getDot -> do
                        parentNode <- getDot

                        -- Add edge between parent and child
                        parentNode .->. childNode

                        -- Return parent NodeId
                        pure parentNode

                    pure expr
                Nothing        -> do
                    -- Here we have to match and unwrap the @MissingImports@
                    -- in a separate handler, otherwise we'd have it wrapped
                    -- in another @Imported@ when parsing a @missing@, because
                    -- we are representing it with an empty exception list
                    -- (which would not be empty if this would happen).
                    -- TODO: restructure the Exception hierarchy to prevent
                    -- this nesting from happening in the first place.
                    let handler₀
                            :: (MonadCatch m)
                            => MissingImports
                            -> StateT (Status m) m Resolved
                        handler₀ (MissingImports es) =
                          throwM
                            (MissingImports
                               (map
                                 (\e -> toException (Imported _stack' e))
                                 es
                               )
                             )

                        handler₁
                            :: (MonadCatch m)
                            => SomeException
                            -> StateT (Status m) m Resolved
                        handler₁ e =
                          throwMissingImport (Imported _stack' e)

                    -- This loads a \"dynamic\" expression (i.e. an expression
                    -- that might still contain imports)
                    let loadDynamic = _resolver child

                    Resolved {..} <- loadDynamic `catches` [ Handler handler₀, Handler handler₁ ]

                    let stackWithNewImport = NonEmpty.cons newImport _stack

                    let childNodeId = userNodeId _nextNodeId

                    -- Increment the next node id
                    zoom nextNodeId $ State.modify succ

                    -- Make current node the dot graph
                    zoom dot . State.put $ importNode childNodeId child

                    zoom stack (State.put stackWithNewImport)
                    expr'' <- loadWith resolvedExpression
                    zoom stack (State.put _stack)

                    zoom dot . State.modify $ \getSubDot -> do
                        parentNode <- _dot

                        -- Get current node back from sub-graph
                        childNode <- getSubDot

                        -- Add edge between parent and child
                        parentNode .->. childNode

                        -- Return parent NodeId
                        pure parentNode

                    _cacher child expr''

                    -- Type-check expressions here for three separate reasons:
                    --
                    --  * to verify that they are closed
                    --  * to catch type errors as early in the import process
                    --    as possible
                    --  * to avoid normalizing ill-typed expressions that need
                    --    to be hashed
                    --
                    -- There is no need to check expressions that have been
                    -- cached, since they have already been checked
                    expr''' <- case Dhall.TypeCheck.typeWith _startingContext expr'' of
                        Left  err -> throwM (Imported _stack' err)
                        Right _   -> return (Dhall.Core.normalizeWith _normalizer expr'')
                    zoom cache (State.modify' (Map.insert child (childNodeId, expr''')))
                    return expr'''

    case hash (importHashed import₀) of
        Nothing -> do
            return ()
        Just expectedHash -> do
            let matches version =
                    let actualHash =
                            hashExpression version (Dhall.Core.alphaNormalize expr)

                    in  expectedHash == actualHash

            if any matches [ minBound .. maxBound ]
                then return ()
                else do
                    let actualHash =
                            hashExpression NoVersion (Dhall.Core.alphaNormalize expr)

                    throwMissingImport (Imported _stack' (HashMismatch {..}))

    return expr
  ImportAlt a b -> loadWith a `catch` handler₀
    where
      handler₀ (SourcedException (Src begin _ text₀) (MissingImports es₀)) =
          loadWith b `catch` handler₁
        where
          handler₁ (SourcedException (Src _ end text₁) (MissingImports es₁)) =
              throwM (SourcedException (Src begin end text₂) (MissingImports (es₀ ++ es₁)))
            where
              text₂ = text₀ <> " ? " <> text₁

  Const a              -> pure (Const a)
  Var a                -> pure (Var a)
  Lam a b c            -> Lam <$> pure a <*> loadWith b <*> loadWith c
  Pi a b c             -> Pi <$> pure a <*> loadWith b <*> loadWith c
  App a b              -> App <$> loadWith a <*> loadWith b
  Let as b             -> Let <$> traverse f as <*> loadWith b
    where
      f (Binding c d e) = Binding c <$> traverse loadWith d <*> loadWith e
  Annot a b            -> Annot <$> loadWith a <*> loadWith b
  Bool                 -> pure Bool
  BoolLit a            -> pure (BoolLit a)
  BoolAnd a b          -> BoolAnd <$> loadWith a <*> loadWith b
  BoolOr a b           -> BoolOr <$> loadWith a <*> loadWith b
  BoolEQ a b           -> BoolEQ <$> loadWith a <*> loadWith b
  BoolNE a b           -> BoolNE <$> loadWith a <*> loadWith b
  BoolIf a b c         -> BoolIf <$> loadWith a <*> loadWith b <*> loadWith c
  Natural              -> pure Natural
  NaturalLit a         -> pure (NaturalLit a)
  NaturalFold          -> pure NaturalFold
  NaturalBuild         -> pure NaturalBuild
  NaturalIsZero        -> pure NaturalIsZero
  NaturalEven          -> pure NaturalEven
  NaturalOdd           -> pure NaturalOdd
  NaturalToInteger     -> pure NaturalToInteger
  NaturalShow          -> pure NaturalShow
  NaturalPlus a b      -> NaturalPlus <$> loadWith a <*> loadWith b
  NaturalTimes a b     -> NaturalTimes <$> loadWith a <*> loadWith b
  Integer              -> pure Integer
  IntegerLit a         -> pure (IntegerLit a)
  IntegerShow          -> pure IntegerShow
  IntegerToDouble      -> pure IntegerToDouble
  Double               -> pure Double
  DoubleLit a          -> pure (DoubleLit a)
  DoubleShow           -> pure DoubleShow
  Text                 -> pure Text
  TextLit (Chunks a b) -> fmap TextLit (Chunks <$> mapM (mapM loadWith) a <*> pure b)
  TextAppend a b       -> TextAppend <$> loadWith a <*> loadWith b
  TextShow             -> pure TextShow
  List                 -> pure List
  ListLit a b          -> ListLit <$> mapM loadWith a <*> mapM loadWith b
  ListAppend a b       -> ListAppend <$> loadWith a <*> loadWith b
  ListBuild            -> pure ListBuild
  ListFold             -> pure ListFold
  ListLength           -> pure ListLength
  ListHead             -> pure ListHead
  ListLast             -> pure ListLast
  ListIndexed          -> pure ListIndexed
  ListReverse          -> pure ListReverse
  Optional             -> pure Optional
  None                 -> pure None
  Some a               -> Some <$> loadWith a
  OptionalFold         -> pure OptionalFold
  OptionalBuild        -> pure OptionalBuild
  Record a             -> Record <$> mapM loadWith a
  RecordLit a          -> RecordLit <$> mapM loadWith a
  Union a              -> Union <$> mapM (mapM loadWith) a
  UnionLit a b c       -> UnionLit <$> pure a <*> loadWith b <*> mapM (mapM loadWith) c
  Combine a b          -> Combine <$> loadWith a <*> loadWith b
  CombineTypes a b     -> CombineTypes <$> loadWith a <*> loadWith b
  Prefer a b           -> Prefer <$> loadWith a <*> loadWith b
  Merge a b c          -> Merge <$> loadWith a <*> loadWith b <*> mapM loadWith c
  Field a b            -> Field <$> loadWith a <*> pure b
  Project a b          -> Project <$> loadWith a <*> mapM loadWith b
  Note a b             -> do
      let handler e = throwM (SourcedException a (e :: MissingImports))

      (Note <$> pure a <*> loadWith b) `catch` handler

-- | Resolve all imports within an expression
load :: Expr Src Import -> IO (Expr Src X)
load expression = State.evalStateT (loadWith expression) (emptyStatus ".")

encodeExpression
    :: forall s
    .  StandardVersion
    -- ^ `Nothing` means to encode without the version tag
    -> Expr s X
    -> Data.ByteString.ByteString
encodeExpression _standardVersion expression = bytesStrict
  where
    intermediateExpression :: Expr s Import
    intermediateExpression = fmap absurd expression

    term :: Term
    term = Dhall.Binary.encode intermediateExpression

    taggedTerm :: Term
    taggedTerm =
        case _standardVersion of
            NoVersion -> term
            s         -> TList [ TString v, term ]
              where
                v = Dhall.Binary.renderStandardVersion s

    bytesLazy = Codec.Serialise.serialise taggedTerm

    bytesStrict = Data.ByteString.Lazy.toStrict bytesLazy

-- | Hash a fully resolved expression
hashExpression
    :: StandardVersion -> Expr s X -> (Crypto.Hash.Digest SHA256)
hashExpression _standardVersion expression =
    Crypto.Hash.hash (encodeExpression _standardVersion expression)

{-| Convenience utility to hash a fully resolved expression and return the
    base-16 encoded hash with the @sha256:@ prefix

    In other words, the output of this function can be pasted into Dhall
    source code to add an integrity check to an import
-}
hashExpressionToCode :: StandardVersion -> Expr s X -> Text
hashExpressionToCode _standardVersion expr =
    "sha256:" <> Text.pack (show (hashExpression _standardVersion expr))

-- | A call to `assertNoImports` failed because there was at least one import
data ImportResolutionDisabled = ImportResolutionDisabled deriving (Exception)

instance Show ImportResolutionDisabled where
    show _ = "\nImport resolution is disabled"

-- | Assert than an expression is import-free
assertNoImports :: MonadIO io => Expr Src Import -> io (Expr Src X)
assertNoImports expression =
    Dhall.Core.throws (traverse (\_ -> Left ImportResolutionDisabled) expression)
