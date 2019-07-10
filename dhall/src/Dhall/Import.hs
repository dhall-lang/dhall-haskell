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
    , Depends(..)
    , graph
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

import Dhall.Parser (Parser(..), ParseError(..), Src(..), SourcedException(..))
import Dhall.TypeCheck (X(..))
import Lens.Family.State.Strict (zoom)

import qualified Codec.Serialise
import qualified Control.Exception                as Exception
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
  :: Text
  -> Text
  -> Expr s a
  -> Maybe [(CI Data.ByteString.ByteString, Data.ByteString.ByteString)]
toHeaders key₀ key₁ (ListLit _ hs) = do
    hs' <- mapM (toHeader key₀ key₁) hs
    return (Data.Foldable.toList hs')
toHeaders _ _ _ = do
    empty

toHeader
  :: Text
  -> Text
  -> Expr s a
  -> Maybe (CI Data.ByteString.ByteString, Data.ByteString.ByteString)
toHeader key₀ key₁ (RecordLit m) = do
    TextLit (Chunks [] keyText  ) <- Dhall.Map.lookup key₀ m
    TextLit (Chunks [] valueText) <- Dhall.Map.lookup key₁ m
    let keyBytes   = Data.Text.Encoding.encodeUtf8 keyText
    let valueBytes = Data.Text.Encoding.encodeUtf8 valueText
    return (Data.CaseInsensitive.mk keyBytes, valueBytes)
toHeader _ _ _ = do
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

-- concatenates relative imports; checks for cyclic imports; normalises headers
resolveImport :: ImportStack -> Import -> StateT (Status IO) IO ResolvedImport
resolveImport (parent :| stack) import_
    | child `elem` stack =  -- ensure there aren't any cyclic imports
        throwMissingImport (Imported stack (Cycle import_))
    | not (local parent) && local child =  -- ensure all imports are "referentially sane"
        throwMissingImport (Imported _stack (ReferentiallyOpaque import₀))
  where
    child = canonicalize (parent <> import_)
    local (Import (ImportHashed _ (Remote  {})) _) = False
    local (Import (ImportHashed _ (Local   {})) _) = True
    local (Import (ImportHashed _ (Env     {})) _) = True
    local (Import (ImportHashed _ (Missing {})) _) = True

-- resolve headers in remote import
resolveImport (parent :| stack)
              (Import (ImportHashed maybeHash (Remote url (Just headers))) importMode) = do
    expr <- loadExpr stack headers
    let decodeHeaders key₀ key₁ = do
            let expected :: Expr Src X
                expected = App List (Record (Dhall.Map.fromList [ (key₀, Text), (key₁, Text) ]))
            let annot = case expr of
                    Note (Src begin end bytes) _ -> do
                        let bytes' = bytes <> " : "
                              <> Dhall.Pretty.Internal.prettyToStrictText expected
                        Note (Src begin end bytes') (Annot expr expected)
                    _ -> Annot expr expected
            case Dhall.TypeCheck.typeOf annot of
                Left err -> liftIO (throwIO err)
                Right _  -> return ()
            let expr' = Dhall.Core.normalize expr
            case toHeaders key₀ key₁ expr' of
                Just binaryHeaders -> do
                    -- HERE!
                    return (ResolvedImport maybeHash (ResolvedRemote url (Just binaryHeaders)) importMode)
                Nothing -> do
                    liftIO (throwIO InternalError)
    let handler₀ (e :: SomeException) = do
            let handler₁ (_ :: SomeException) =
                    Exception.throw e
            Exception.handle handler₁ (decodeHeaders "header" "value")
    liftIO (Exception.handle handler₀ (decodeHeaders "mapKey" "mapValue"))

-- in general we only need to resolve imports relative to the parent import, i.e. chain them!
resolveImport (parent :| stack) import_ = return (chainImport parent import_)

chainImport :: ResolvedImport -> Import -> ResolvedImport
chainImport = undefined  -- needs to canonicalize after concatenating. see `instance Semigroup Import`

-- check "hot" cache; add to cache if not already there
loadImport :: ImportStack -> ResolvedImport -> StateT (Status IO) IO LoadedImport
loadImport stack import_ = do
    cache_ <- use cache
    case Map.lookup import_ _cache of
        Just loadedImport -> pure loadedImport
        Nothing -> do
            loadedImport <- loadImportFromSemanticCache stack resolvedImport
            modifying cache (Map.insert import_ loadedImport)
            pure loadedImport

-- if frozen (hashed) try to load from sematnic disk cache. in general, defer to loadImportFromSemisemanticCache
loadImportFromSemanticCache :: ImportStack -> ResolvedImport -> StateT (Status IO) IO LoadedImport
loadImportFromSemanticCache stack import_@(Import (ImportHashed (Just semanticHash) _) _) = do
    -- try to load from disk cache
    mCached <- fetchFromSemanticCache semanticHash
    case mCached of
        Just bytes -> do
            let actualHash = Crypto.Hash.hash bytesStrict
            if semanticHash == actualHash
                then return ()
                else throwMissingImport (Imported _stack (HashMismatch {..}))

            term <- Dhall.Core.throws (Codec.Serialise.deserialiseOrFail bytesLazy)
            normalisedImport <- Dhall.Core.throws (Dhall.Binary.decodeExpression term)
            -- a frozen import does not have any dependencies if loaded from semantic cache.
            let graph = Node import_ []
            return (LoadedImport {..})

        Nothing -> do
            -- continue with semi-semantic cache.
            -- note that the hash might be different here (uses default version).
            result <- loadImportFromSemisemanticCache stack import_
            let alphaNormal = Dhall.Core.alphaNormalize (normalisedImport result)
            -- encode with each different Dhall version
            let variants = map (\version -> encodeExpression version alphaNormal)
                               [ minBound .. maxBound ]
            -- find the version that results in the expected hash
            case find ((== semanticHash). Crypto.Hash.hash) of
                Nothing -> throwMissingImport (Imported stack (HashMismatch {..}))
                Just bytes -> writeToSemanticCache semanticHash bytes
            return (result {graph = ... replace root node with import_})

-- defer to loadImportFromSemisemanticCache for non-frozen imports.
loadImportFromSemanticCache stack import_@(Import (ImportHashed Nothing _) _) =
    loadImportFromSemisemanticCache stack import_


-- TODO: Pretty instances for ResolvedImport
-- load from semi-semantic cache, otherwise load from scratch
loadImportFromSemisemanticCache :: ImportStack -> ResolvedImport -> StateT (Status IO) IO LoadedImport
-- it doesn't make sense to cache locations semi-semantically
loadImportFromSemisemanticCache stack import_@(ResolvedImport (Import (ImportHashed _ importType) Location)) = do
    let locationType = Union $ Dhall.Map.fromList
            [ ("Environment", Just Text)
            , ("Remote", Just Text)
            , ("Local", Just Text)
            , ("Missing", Nothing)
            ]
    let normalisedImport =
            case importType of
                ResolvedMissing -> Field locationType "Missing"
                local@(ResolvedLocal _ _) ->
                    App (Field locationType "Local")
                      (TextLit (Chunks [] (Dhall.Pretty.Internal.pretty local)))
                remote@(ResolvedRemote _) ->
                    App (Field locationType "Remote")
                      (TextLit (Chunks [] (Dhall.Pretty.Internal.pretty remote)))
                ResolvedEnv env ->
                    App (Field locationType "Environment")
                      (TextLit (Chunks [] (Dhall.Pretty.Internal.pretty env)))
    -- note that normalisedImport is already alpha-normal!
    let semanticHash = hashExpression (normalisedImport)
    -- `as Location` doesn't actually touch anything
    let graph = Node import_ []
    return (LoadedImport {..})

-- `as Text` imports are never cached semi-semantically
loadImportFromSemisemanticCache stack import_@(ResolvedImport (Import (ImportHashed _ importType) RawText)) = do
    text <- fetchFresh importType
    let normalisedImport = TextLit (Chunks [] text)
    let semanticHash = hashExpression (normalisedImport)
    let graph = Node import_ []
    return (LoadedImport {..})

loadImportFromSemisemanticCache stack import_@(ResolvedImport (Import (ImportHashed _ importType) Code)) = do
    text <- fetchFresh importType

    -- used in parser error messages
    path <- case importType of
        Local prefix file = localToPath prefix file
        Remote url -> Text.unpack (renderURL url)
        Env env -> Text.unpack env
        Missing -> liftIO $ throwM (MissingImports [])
    let parser = unParser $ do
            Text.Parser.Token.whiteSpace  -- isn't this redundant?!
            r <- Dhall.Parser.expr
            Text.Parser.Combinators.eof
            return r
    parsedImport <- case Text.Megaparsec.parse parser path text of
        Left errInfo -> do
            liftIO (throwIO (ParseError errInfo text))
        Right expr -> do
            return expr

    let stack' = NonEmpty.cons import_ stack
    loaded@LoadedExpr {..} <- loadExpr stack' parsedImport
    let semisemanticHash = hashSemisemantic loaded
    -- Check semi-semantic cache
    result <- fetchFromSemisemanticCache semisemanticHash
    (bytes, normalisedImport) <- case result of
        Just bytes -> do
            let bytesLazy = Data.ByteString.Lazy.fromStrict bytesStrict
            term <- Dhall.Core.throws (Codec.Serialise.deserialiseOrFail bytesLazy)
            normalisedImport <- Dhall.Core.throws (Dhall.Binary.decodeExpression term)
            return (bytes, normalisedImport)
        Nothing -> do  -- otherwise typecheck and normalise
            normalisedImport <- case Dhall.TypeCheck.typeWith _startingContext loadedExpr of
                Left err -> throwM (Imported stack' err)
                Right _ -> return (Dhall.Core.normalizeWith _normalizer loadedExpr)

            let alphaNormal = Dhall.Core.alphaNormalize (normalisedImport result)
            let bytes = encodeExpression defaultVersion alphaNormal
            writeToSemisemanticCache semisemanticHash bytes
            return (bytes, normalisedImport)

    let semanticHash = Crypto.Hash.hash bytes
    let graph = Node import_ imports
    return (LoadedImport {..})


-- directly from disk/network
fetchFresh :: ResolvedImportType -> StateT (Status m) IO Text
fetchFresh (ResolvedLocal prefix file) = liftIO $ do
    let path = localToPath prefix file
    exists <- Directory.doesFileExist path
    if exists
        then Data.Text.IO.readFile path
        then throwMissingImport (MissingFile path)

fetchFresh (ResolvedRemote url mHeaders) = do
    (_, text) <- fetchFromHttpUrl url mHeaders
    return text

fetchFresh (ResolvedEnv env) = liftIO $ do
    x <- System.Environment.lookupEnv (Text.unpack env)
    case x of
        Just string -> do
            return (Text.pack string)
        Nothing -> do
                throwMissingImport (MissingEnvironmentVariable env)

fetchFresh Missing = liftIO $ throwM (MissingImports [])


fetchFromSemanticCache :: SemanticHash -> IO (Maybe ByteString)
fetchFromSemanticCache expectedHash = Maybe.runMaybeT $ do
    cacheFile <- getCacheFile expectedHash
    True <- liftIO (Directory.doesFileExist cacheFile)
    liftIO (Data.ByteString.readFile cacheFile)

fetchFromSemisemanticCache :: SemisemanticHash -> IO (Maybe ByteString)
fetchFromSemisemanticCache semisemanticHash = Maybe.runMaybeT $ do
        cacheFile <- getCacheFile semisemanticHash  -- TODO: use different cache directory
        True <- liftIO (Directory.doesFileExist cacheFile)
        liftIO (Data.ByteString.readFile cacheFile)

writeToSemanticCache :: SemanticHash -> ByteString -> IO ()
writeToSemanticCache semanticHash bytes = do
    Maybe.runMaybeT $ do
        cacheFile <- getCacheFile semanticHash
        liftIO (Data.ByteString.writeFile cacheFile bytes)
    return ()

writeToSemisemanticCache :: SemisemanticHash -> ByteString -> IO ()
writeToSemisemanticCache semisemanticHash bytes = do
    Maybe.runMaybeT $ do
        cacheFile <- getCacheFile semisemanticHash  -- TODO: different cache location
        liftIO (Data.ByteString.writeFile cacheFile bytes)
    return ()


loadExpr' :: ImportStack -> Expr Src Import ->
                WriterT [ResolvedImport] (StateT (Status IO) IO) (Expr Src X)
loadExpr' stack (Embed import_) = do
    let handler₀ (MissingImports es) =
            throwM (MissingImports (map (\e -> toException (Imported _stack' e)) es))
        handler₁ e = throwMissingImport (Imported _stack' e :: Import SomeException)
    resolvedImport <- lift $ resolveImport stack import_
                `catches` [ Handler handler₀, Handler handler₁ ]
    tell resolvedImport
    return (normalisedImport resolvedImport)

loadExpr' stack (ImportAlt a b) = loadExpr' stack a `catch` handler₀
  where
    handler₀ (SourcedException (Src begin _ text₀) (MissingImports es₀)) =
        loadExpr' loadWith b `catch` handler₁
      where
        handler₁ (SourcedException (Src _ end text₁) (MissingImports es₁)) =
            throwM (SourcedException (Src begin end text₂) (MissingImports (es₀ ++ es₁)))
          where
            text₂ = text₀ <> " ? " <> text₁

loadExpr' stack (Note a b) = do
    let handler e = throwM (SourcedException a (e :: MissingImports))
    (Note <$> pure a <*> loadExpr' stack b) `catch` handler

-- map over subexpressions, not quite sure which lens combinator to use here
loadExpr' stack expr = undefined  -- over Dhall.Core.subExpressions (loadExpr' stack) expr

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

-- | Resolve all imports within an expression
load :: Expr Src Import -> IO (Expr Src X)
load expression = State.evalStateT (loadExpr (rootImport ".") expression) emptyStatus

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

hashSemisemantic :: LoadedExpr -> Crypto.Hash.Digest SHA256
hashSemisemantic = undefined

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
