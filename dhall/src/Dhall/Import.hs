{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
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
      load
    , loadRelativeTo
    , loadWith
    , localToPath
    , hashExpression
    , hashExpressionToCode
    , writeExpressionToSemanticCache
    , warnAboutMissingCaches
    , assertNoImports
    , Status(..)
    , SemanticCacheMode(..)
    , Chained
    , chainedImport
    , chainedFromLocalHere
    , chainedChangeMode
    , emptyStatus
    , stack
    , cache
    , Depends(..)
    , graph
    , remote
    , toHeaders
    , normalizer
    , startingContext
    , chainImport
    , ImportSemantics
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
import Control.Exception (Exception, SomeException, IOException, toException)
import Control.Monad (guard)
import Control.Monad.Catch (throwM, MonadCatch(catch), handle)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Data.Void (absurd)
#if MIN_VERSION_base(4,8,0)
#else
import Data.Traversable (traverse)
#endif
import Data.Typeable (Typeable)
import System.FilePath ((</>))
import Dhall.Binary (StandardVersion(..))
import Dhall.Core
    ( Expr(..)
    , Chunks(..)
    , Directory(..)
    , File(..)
    , FilePrefix(..)
    , ImportHashed(..)
    , ImportType(..)
    , ImportMode(..)
    , Import(..)
    , URL(..)
    , bindingExprs
    , chunkExprs
    )
#ifdef MIN_VERSION_http_client
import Network.HTTP.Client (Manager)
import Dhall.Import.HTTP hiding (HTTPHeader)
#endif
import Dhall.Import.Types

import Dhall.Parser (Parser(..), ParseError(..), Src(..), SourcedException(..))
import Dhall.TypeCheck (X)
import Lens.Family.State.Strict (zoom)

import qualified Codec.Serialise
import qualified Control.Monad.Trans.Maybe        as Maybe
import qualified Control.Monad.Trans.State.Strict as State
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
import qualified Dhall.Crypto
import qualified Dhall.Map
import qualified Dhall.Parser
import qualified Dhall.Pretty.Internal
import qualified Dhall.TypeCheck
import qualified System.Environment
import qualified System.Info
import qualified System.IO
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
    { importStack :: NonEmpty Chained  -- ^ Imports resolved so far, in reverse order
    , nested      :: e                 -- ^ The nested exception
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

type HTTPHeader = (CI ByteString, ByteString)

-- | Exception thrown when a HTTP url is imported but dhall was built without
-- the @with-http@ Cabal flag.
data CannotImportHTTPURL =
    CannotImportHTTPURL
        String
        (Maybe [HTTPHeader])
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

> canonicalize (a <> b) = canonicalize (canonicalize a <> canonicalize b)
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

-- | Exception thrown when an integrity check fails
data HashMismatch = HashMismatch
    { expectedHash :: Dhall.Crypto.SHA256Digest
    , actualHash   :: Dhall.Crypto.SHA256Digest
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

-- | Construct the file path corresponding to a local import. If the import is
--   _relative_ then the resulting path is also relative.
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
            return ".."

        Here -> do
            return "."

    let cs = map Text.unpack (file : components)

    let cons component dir = dir </> component

    return (foldr cons prefixPath cs)

-- | Given a `Local` import construct the corresponding unhashed `Chained`
--   import (interpreting relative path as relative to the current directory).
chainedFromLocalHere :: FilePrefix -> File -> ImportMode -> Chained
chainedFromLocalHere prefix file mode = Chained $
     Import (ImportHashed Nothing (Local prefix (canonicalize file))) mode

-- | Adjust the import mode of a chained import
chainedChangeMode :: ImportMode -> Chained -> Chained
chainedChangeMode mode (Chained (Import importHashed _)) =
    Chained (Import importHashed mode)

-- Chain imports, also typecheck and normalize headers if applicable.
chainImport :: Chained -> Import -> StateT Status IO Chained
chainImport (Chained parent) child@(Import importHashed@(ImportHashed _ (Remote url)) _) = do
    url' <- normalizeHeaders url
    let child' = child { importHashed = importHashed { importType = Remote url' } }
    return (Chained (canonicalize (parent <> child')))

chainImport (Chained parent) child =
    return (Chained (canonicalize (parent <> child)))

-- | Load an import, resulting in a fully resolved, type-checked and normalised
--   expression. @loadImport@ handles the 'hot' cache in @Status@ and defers to
--   `loadImportWithSemanticCache` for imports that aren't in the @Status@ cache
--   already.
loadImport :: Chained -> StateT Status IO ImportSemantics
loadImport import_ = do
    Status {..} <- State.get
    case Map.lookup import_ _cache of
        Just importSemantics -> return importSemantics
        Nothing -> do
            importSemantics <- loadImportWithSemanticCache import_
            zoom cache (State.modify (Map.insert import_ importSemantics))
            return importSemantics

-- | Load an import from the 'semantic cache'. Defers to
--   `loadImportWithSemisemanticCache` for imports that aren't frozen (and
--   therefore not cached semantically), as well as those that aren't cached yet.
loadImportWithSemanticCache :: Chained -> StateT Status IO ImportSemantics
loadImportWithSemanticCache
  import_@(Chained (Import (ImportHashed Nothing _) _)) = do
    loadImportWithSemisemanticCache import_

loadImportWithSemanticCache
  import_@(Chained (Import (ImportHashed (Just semanticHash) _) _)) = do
    Status { .. } <- State.get
    mCached <-
        case _semanticCacheMode of
            UseSemanticCache -> liftIO $ fetchFromSemanticCache semanticHash
            IgnoreSemanticCache -> pure Nothing

    case mCached of
        Just bytesStrict -> do
            let actualHash = Dhall.Crypto.sha256Hash bytesStrict
            if semanticHash == actualHash
                then return ()
                else do
                    Status { _stack } <- State.get
                    throwMissingImport (Imported _stack (HashMismatch {expectedHash = semanticHash, ..}))

            let bytesLazy = Data.ByteString.Lazy.fromStrict bytesStrict
            term <- case Codec.Serialise.deserialiseOrFail bytesLazy of
                Left err -> throwMissingImport (Imported _stack err)
                Right t -> return t
            importSemantics <- case Dhall.Binary.decodeExpression term of
                Left err -> throwMissingImport (Imported _stack err)
                Right sem -> return sem

            return (ImportSemantics {..})

        Nothing -> do
            ImportSemantics { importSemantics } <- loadImportWithSemisemanticCache import_

            let variants = map (\version -> encodeExpression version (Dhall.Core.alphaNormalize importSemantics))
                                [ minBound .. maxBound ]
            case Data.Foldable.find ((== semanticHash). Dhall.Crypto.sha256Hash) variants of
                Just bytes -> liftIO $ writeToSemanticCache semanticHash bytes
                Nothing -> do
                    let expectedHash = semanticHash
                    Status { _stack } <- State.get
                    let actualHash = hashExpression (Dhall.Core.alphaNormalize importSemantics)
                    throwMissingImport (Imported _stack (HashMismatch {..}))

            return (ImportSemantics {..})

-- Fetch encoded normal form from "semantic cache"
fetchFromSemanticCache :: Dhall.Crypto.SHA256Digest -> IO (Maybe Data.ByteString.ByteString)
fetchFromSemanticCache expectedHash = Maybe.runMaybeT $ do
    cacheFile <- getCacheFile "dhall" expectedHash
    True <- liftIO (Directory.doesFileExist cacheFile)
    liftIO (Data.ByteString.readFile cacheFile)

-- | Ensure that the given expression is present in the semantic cache. The
--   given expression should be alpha-beta-normal.
writeExpressionToSemanticCache :: Expr Src X -> IO ()
writeExpressionToSemanticCache expression = writeToSemanticCache hash bytes
  where
    bytes = encodeExpression NoVersion expression
    hash = Dhall.Crypto.sha256Hash bytes

writeToSemanticCache :: Dhall.Crypto.SHA256Digest -> Data.ByteString.ByteString -> IO ()
writeToSemanticCache hash bytes = do
    _ <- Maybe.runMaybeT $ do
        cacheFile <- getCacheFile "dhall" hash
        liftIO (Data.ByteString.writeFile cacheFile bytes)
    return ()

-- Check the "semi-semantic" disk cache, otherwise typecheck and normalise from
-- scratch.
loadImportWithSemisemanticCache
  :: Chained -> StateT Status IO ImportSemantics
loadImportWithSemisemanticCache (Chained (Import (ImportHashed _ importType) Code)) = do
    text <- fetchFresh importType
    Status {..} <- State.get

    path <- case importType of
        Local prefix file -> liftIO $ do
            path <- localToPath prefix file
            absolutePath <- Directory.makeAbsolute path
            return absolutePath
        Remote url -> do
            let urlText = Dhall.Pretty.Internal.pretty (url { headers = Nothing })
            return (Text.unpack urlText)
        Env env -> return $ Text.unpack env
        Missing -> throwM (MissingImports [])

    let parser = unParser $ do
            Text.Parser.Token.whiteSpace
            r <- Dhall.Parser.expr
            Text.Parser.Combinators.eof
            return r

    parsedImport <- case Text.Megaparsec.parse parser path text of
        Left  errInfo -> do
            throwMissingImport (Imported _stack (ParseError errInfo text))
        Right expr    -> return expr

    resolvedExpr <- loadWith parsedImport  -- we load imports recursively here

    -- Check the semi-semantic cache. See
    -- https://github.com/dhall-lang/dhall-haskell/issues/1098 for the reasoning
    -- behind semi-semantic caching.
    let semisemanticHash = computeSemisemanticHash resolvedExpr
    mCached <- lift $ fetchFromSemisemanticCache semisemanticHash

    importSemantics <- case mCached of
        Just bytesStrict -> do
            let bytesLazy = Data.ByteString.Lazy.fromStrict bytesStrict

            term <- case Codec.Serialise.deserialiseOrFail bytesLazy of
                Left err -> throwMissingImport (Imported _stack err)
                Right t -> return t

            importSemantics <- case Dhall.Binary.decodeExpression term of
                Left err -> throwMissingImport (Imported _stack err)
                Right sem -> return sem

            return importSemantics

        Nothing -> do
            betaNormal <- case Dhall.TypeCheck.typeWith _startingContext resolvedExpr of
                Left  err -> throwMissingImport (Imported _stack err)
                Right _ -> return (Dhall.Core.normalizeWith _normalizer resolvedExpr)

            let bytes = encodeExpression NoVersion betaNormal
            lift $ writeToSemisemanticCache semisemanticHash bytes

            return betaNormal

    return (ImportSemantics {..})

-- `as Text` imports aren't cached since they are well-typed and normal by
-- construction
loadImportWithSemisemanticCache (Chained (Import (ImportHashed _ importType) RawText)) = do
    text <- fetchFresh importType

    -- importSemantics is alpha-beta-normal by construction!
    let importSemantics = TextLit (Chunks [] text)
    return (ImportSemantics {..})

-- `as Location` imports aren't cached since they are well-typed and normal by
-- construction
loadImportWithSemisemanticCache (Chained (Import (ImportHashed _ importType) Location)) = do
    let locationType = Union $ Dhall.Map.fromList
            [ ("Environment", Just Text)
            , ("Remote", Just Text)
            , ("Local", Just Text)
            , ("Missing", Nothing)
            ]

    -- importSemantics is alpha-beta-normal by construction!
    let importSemantics = case importType of
            Missing -> Field locationType "Missing"
            local@(Local _ _) ->
                App (Field locationType "Local")
                  (TextLit (Chunks [] (Dhall.Pretty.Internal.pretty local)))
            remote_@(Remote _) ->
                App (Field locationType "Remote")
                  (TextLit (Chunks [] (Dhall.Pretty.Internal.pretty remote_)))
            Env env ->
                App (Field locationType "Environment")
                  (TextLit (Chunks [] (Dhall.Pretty.Internal.pretty env)))

    return (ImportSemantics {..})

-- The semi-semantic hash of an expression is computed from the fully resolved
-- AST (without normalising or type-checking it first). See
-- https://github.com/dhall-lang/dhall-haskell/issues/1098 for further
-- discussion.
computeSemisemanticHash :: Expr Src X -> Dhall.Crypto.SHA256Digest
computeSemisemanticHash resolvedExpr = hashExpression resolvedExpr

-- Fetch encoded normal form from "semi-semantic cache"
fetchFromSemisemanticCache :: Dhall.Crypto.SHA256Digest -> IO (Maybe Data.ByteString.ByteString)
fetchFromSemisemanticCache semisemanticHash = Maybe.runMaybeT $ do
    cacheFile <- getCacheFile "dhall-haskell" semisemanticHash
    True <- liftIO (Directory.doesFileExist cacheFile)
    liftIO (Data.ByteString.readFile cacheFile)

writeToSemisemanticCache :: Dhall.Crypto.SHA256Digest -> Data.ByteString.ByteString -> IO ()
writeToSemisemanticCache semisemanticHash bytes = do
    _ <- Maybe.runMaybeT $ do
        cacheFile <- getCacheFile "dhall-haskell" semisemanticHash
        liftIO (Data.ByteString.writeFile cacheFile bytes)
    return ()

-- Fetch source code directly from disk/network
fetchFresh :: ImportType -> StateT Status IO Text
fetchFresh (Local prefix file) = do
    Status { _stack } <- State.get
    path <- liftIO $ localToPath prefix file
    exists <- liftIO $ Directory.doesFileExist path
    if exists
        then liftIO $ Data.Text.IO.readFile path
        else throwMissingImport (Imported _stack (MissingFile path))

fetchFresh (Remote url) = do
    Status { _remote } <- State.get
    _remote url

fetchFresh (Env env) = do
    Status { _stack } <- State.get
    x <- liftIO $ System.Environment.lookupEnv (Text.unpack env)
    case x of
        Just string -> do
            return (Text.pack string)
        Nothing -> do
                throwMissingImport (Imported _stack (MissingEnvironmentVariable env))

fetchFresh Missing = throwM (MissingImports [])


fetchRemote :: URL -> StateT Status IO Data.Text.Text
#ifndef MIN_VERSION_http_client
fetchRemote (url@URL { headers = maybeHeadersExpression }) = do
    let maybeHeaders = fmap toHeaders maybeHeadersExpression
    let urlString = Text.unpack (Dhall.Core.pretty url)
    Status { _stack } <- State.get
    throwMissingImport (Imported _stack (CannotImportHTTPURL urlString maybeHeaders))
#else
fetchRemote url = do
    manager <- liftIO $ newManager
    zoom remote (State.put (fetchFromHTTP manager))
    fetchFromHTTP manager url
  where
    fetchFromHTTP :: Manager -> URL -> StateT Status IO Data.Text.Text
    fetchFromHTTP manager (url'@URL { headers = maybeHeadersExpression }) = do
        let maybeHeaders = fmap toHeaders maybeHeadersExpression
        fetchFromHttpUrl manager url' maybeHeaders
#endif

-- | Given a well-typed (of type `List { header : Text, value Text }` or
-- `List { mapKey : Text, mapValue Text }`) headers expressions in normal form
-- construct the corresponding binary http headers; otherwise return the empty
-- list.
toHeaders :: Expr s a -> [HTTPHeader]
toHeaders (ListLit _ hs) = Data.Foldable.toList (Data.Foldable.fold maybeHeaders)
  where
      maybeHeaders = mapM toHeader hs
toHeaders _ = []

toHeader :: Expr s a -> Maybe HTTPHeader
toHeader (RecordLit m) = do
    TextLit (Chunks [] keyText  ) <-
        Dhall.Map.lookup "header" m <|> Dhall.Map.lookup "mapKey" m
    TextLit (Chunks [] valueText) <-
        Dhall.Map.lookup "value" m <|> Dhall.Map.lookup "mapValue" m
    let keyBytes   = Data.Text.Encoding.encodeUtf8 keyText
    let valueBytes = Data.Text.Encoding.encodeUtf8 valueText
    return (Data.CaseInsensitive.mk keyBytes, valueBytes)
toHeader _ = do
    empty

getCacheFile
    :: (MonadCatch m, Alternative m, MonadIO m) => FilePath -> Dhall.Crypto.SHA256Digest -> m FilePath
getCacheFile cacheName hash = do
    cacheDirectory <- getOrCreateCacheDirectory cacheName

    let cacheFile = cacheDirectory </> ("1220" <> show hash)

    return cacheFile

warnAboutMissingCaches :: (MonadCatch m, Alternative m, MonadIO m) => m ()
warnAboutMissingCaches = warn <|> return ()
    where warn = Data.Foldable.traverse_ getOrCreateCacheDirectoryOrWarn ["dhall", "dhall-haskell"]

getOrCreateCacheDirectoryOrWarn :: (MonadCatch m, Alternative m, MonadIO m) => FilePath -> m FilePath
getOrCreateCacheDirectoryOrWarn cacheName = do
    cacheBaseDir <- getCacheBaseDirectoryOrWarn

    let message =
            "\n" 
         <> "\ESC[1;33mWarning\ESC[0m: " 
         <> "It hasn't been possible to get or create the default cache directory:\n" 
         <> "\n"
         <> "  " ++ cacheBaseDir </> cacheName ++ "\n"
         <> "\n"
         <> "Usually it is caused by permissions issues.\n"
         <> "You should make it readable and writable or provide another cache base directory " 
         <> "setting the $XDG_CACHE_HOME environment variable.\n"
        
    let warn = liftIO (System.IO.hPutStrLn System.IO.stderr message) >> empty
    
    getOrCreateCacheDirectory cacheName <|> warn

getOrCreateCacheDirectory :: (MonadCatch m, Alternative m, MonadIO m) => FilePath -> m FilePath
getOrCreateCacheDirectory cacheName = do
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

    cacheBaseDirectory <- getCacheBaseDirectory

    let directory = cacheBaseDirectory </> cacheName
    
    let handler (_ :: IOException) = empty

    catch (assertDirectory directory) handler

    return directory

getCacheBaseDirectoryOrWarn :: (Alternative m, MonadIO m) => m FilePath
getCacheBaseDirectoryOrWarn = do
    let message =
            "\n" 
         <> "\ESC[1;33mWarning\ESC[0m: " 
         <> "It hasn't been possible get a cache base directory from environment.\n"
         <> "\n"
         <> "You can provide a cache base directory by pointing the $XDG_CACHE_HOME "
         <> "environment variable to a directory with read and write permissions.\n"

    let warn = liftIO (System.IO.hPutStrLn System.IO.stderr message) >> empty
    
    getCacheBaseDirectory <|> warn
                
getCacheBaseDirectory :: (Alternative m, MonadIO m) => m FilePath
getCacheBaseDirectory = alternative₀ <|> alternative₁
  where
    alternative₀ = do
        maybeXDGCacheHome <- do
          liftIO (System.Environment.lookupEnv "XDG_CACHE_HOME")

        case maybeXDGCacheHome of
            Just xdgCacheHome -> return xdgCacheHome
            Nothing           -> empty

    alternative₁
        | isWindows = do
            maybeLocalAppDirectory <-
              liftIO (System.Environment.lookupEnv "LOCALAPPDATA")

            case maybeLocalAppDirectory of
                Just localAppDirectory -> return localAppDirectory
                Nothing                -> empty

        | otherwise = do
            maybeHomeDirectory <-
              liftIO (System.Environment.lookupEnv "HOME")

            case maybeHomeDirectory of
                Just homeDirectory -> return (homeDirectory </> ".cache")
                Nothing            -> empty

        where isWindows = System.Info.os == "mingw32"


-- If the URL contains headers typecheck them and replace them with their normal
-- forms.
normalizeHeaders :: URL -> StateT Status IO URL
normalizeHeaders url@URL { headers = Just headersExpression } = do
    Status { _stack } <- State.get
    loadedExpr <- loadWith headersExpression

    let go key₀ key₁ = do
            let expected :: Expr Src X
                expected =
                    App List
                        ( Record
                            ( Dhall.Map.fromList
                                [ (key₀, Text), (key₁, Text) ]
                            )
                        )

            let suffix_ = Dhall.Pretty.Internal.prettyToStrictText expected
            let annot = case loadedExpr of
                    Note (Src begin end bytes) _ ->
                        Note (Src begin end bytes') (Annot loadedExpr expected)
                      where
                        bytes' = bytes <> " : " <> suffix_
                    _ ->
                        Annot loadedExpr expected

            _ <- case (Dhall.TypeCheck.typeOf annot) of
                Left err -> throwMissingImport (Imported _stack err)
                Right _ -> return ()

            return (Dhall.Core.normalize loadedExpr)

    let handler₀ (e :: SomeException) = do
            {- Try to typecheck using the preferred @mapKey@/@mapValue@ fields
               and fall back to @header@/@value@ if that fails. However, if
               @header@/@value@ still fails then re-throw the original exception
               for @mapKey@ / @mapValue@. -}
            let handler₁ (_ :: SomeException) =
                    throwMissingImport (Imported _stack e)

            handle handler₁ (go "header" "value")

    headersExpression' <-
        handle handler₀ (go "mapKey" "mapValue")

    return url { headers = Just (fmap absurd headersExpression') }

normalizeHeaders url = return url

-- | Default starting `Status`, importing relative to the given directory.
emptyStatus :: FilePath -> Status
emptyStatus = emptyStatusWith fetchRemote

{-| Generalized version of `load`

    You can configure the desired behavior through the initial `Status` that you
    supply
-}
loadWith :: Expr Src Import -> StateT Status IO (Expr Src X)
loadWith expr₀ = case expr₀ of
  Embed import₀ -> do
    Status {..} <- State.get

    let parent = NonEmpty.head _stack

    child <- chainImport parent import₀

    let local (Chained (Import (ImportHashed _ (Remote  {})) _)) = False
        local (Chained (Import (ImportHashed _ (Local   {})) _)) = True
        local (Chained (Import (ImportHashed _ (Env     {})) _)) = True
        local (Chained (Import (ImportHashed _ (Missing {})) _)) = True

    let referentiallySane = not (local child) || local parent

    if referentiallySane
        then return ()
        else throwMissingImport (Imported _stack (ReferentiallyOpaque import₀))

    let _stack' = NonEmpty.cons child _stack

    if child `elem` _stack
        then throwMissingImport (Imported _stack (Cycle import₀))
        else return ()

    zoom graph . State.modify $
        -- Add the edge `parent -> child` to the import graph
        \edges -> Depends parent child : edges

    let stackWithChild = NonEmpty.cons child _stack

    zoom stack (State.put stackWithChild)
    ImportSemantics {..} <- loadImport child
    zoom stack (State.put _stack)

    return importSemantics

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
  Let a b              -> Let <$> bindingExprs loadWith a <*> loadWith b
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
  NaturalSubtract      -> pure NaturalSubtract
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
  TextLit chunks       -> TextLit <$> chunkExprs loadWith chunks
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
  Combine a b          -> Combine <$> loadWith a <*> loadWith b
  CombineTypes a b     -> CombineTypes <$> loadWith a <*> loadWith b
  Prefer a b           -> Prefer <$> loadWith a <*> loadWith b
  Merge a b c          -> Merge <$> loadWith a <*> loadWith b <*> mapM loadWith c
  ToMap a b            -> ToMap <$> loadWith a <*> mapM loadWith b
  Field a b            -> Field <$> loadWith a <*> pure b
  Project a b          -> Project <$> loadWith a <*> mapM loadWith b
  Assert a             -> Assert <$> loadWith a
  Equivalent a b       -> Equivalent <$> loadWith a <*> loadWith b
  Note a b             -> do
      let handler e = throwM (SourcedException a (e :: MissingImports))

      (Note <$> pure a <*> loadWith b) `catch` handler

-- | Resolve all imports within an expression
load :: Expr Src Import -> IO (Expr Src X)
load = loadRelativeTo "." UseSemanticCache

-- | Resolve all imports within an expression, importing relative to the given
-- directory.
loadRelativeTo :: FilePath -> SemanticCacheMode -> Expr Src Import -> IO (Expr Src X)
loadRelativeTo rootDirectory semanticCacheMode expression =
    State.evalStateT
        (loadWith expression)
        (emptyStatus rootDirectory) { _semanticCacheMode = semanticCacheMode }

encodeExpression
    :: forall s
    .  StandardVersion
    -- ^ `NoVersion` means to encode without the version tag
    -> Expr s X
    -> Data.ByteString.ByteString
encodeExpression _standardVersion expression = bytesStrict
  where
    intermediateExpression :: Expr s Import
    intermediateExpression = fmap absurd expression

    term :: Term
    term = Dhall.Binary.encodeExpression intermediateExpression

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
hashExpression :: Expr s X -> Dhall.Crypto.SHA256Digest
hashExpression expression =
    Dhall.Crypto.sha256Hash (encodeExpression NoVersion expression)

{-| Convenience utility to hash a fully resolved expression and return the
    base-16 encoded hash with the @sha256:@ prefix

    In other words, the output of this function can be pasted into Dhall
    source code to add an integrity check to an import
-}
hashExpressionToCode :: Expr s X -> Text
hashExpressionToCode expr =
    "sha256:" <> Text.pack (show (hashExpression expr))

-- | A call to `assertNoImports` failed because there was at least one import
data ImportResolutionDisabled = ImportResolutionDisabled deriving (Exception)

instance Show ImportResolutionDisabled where
    show _ = "\nImport resolution is disabled"

-- | Assert than an expression is import-free
assertNoImports :: MonadIO io => Expr Src Import -> io (Expr Src X)
assertNoImports expression =
    Dhall.Core.throws (traverse (\_ -> Left ImportResolutionDisabled) expression)
