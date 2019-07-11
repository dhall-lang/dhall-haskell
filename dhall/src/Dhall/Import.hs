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
import qualified Data.ByteArray
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

    canonicalize (Remote (URL {..}) headers) =
        Remote (URL { path = canonicalize path, ..}) (fmap (fmap canonicalize) headers)

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

-- | "Resolve" an import: "chain" the import together with the parent import
--   (on top of the import stack) and check for cyclic imports and
--   "referential sanity"; in the case of remote imports, evaluate headers.
resolveImport :: ImportStack -> Import -> StateT (Status IO) IO ResolvedImport
resolveImport (parent :| stack) import_
    | child `elem` stack =  -- ensure there aren't any import cycles
        throwMissingImport (Imported stack (Cycle import_))
    | not (local parent) && local child =  -- ensure all imports are "referentially sane"
        throwMissingImport (Imported stack (ReferentiallyOpaque import_))
  where
    child = chainImport parent import_
    local (ResolvedImport _ (ResolvedLocal   {}) _) = True
    local (ResolvedImport _ (ResolvedRemote  {}) _) = False
    local (ResolvedImport _ (ResolvedEnv     {}) _) = True
    local (ResolvedImport _ (ResolvedMissing {}) _) = True

-- Remote import without headers
resolveImport parent
            (Import (ImportHashed mHash (Remote (URL {..}) Nothing) mode) =
    return (ResolvedImport mHash resolvedImportType mode)
  where
    resolvedUrl = URL { path = canonicalize path, ..}
    resolvedImportType = ResolvedRemote resolvedUrl Nothing

-- Remote import with headers: evaluate headers
resolveImport stack
  (Import (ImportHashed mHash (Remote (URL {..}) (Just headers))) mode) = do
    let resolvedUrl = URL { path = canonicalize path, .. }

    expr <- loadExpr stack headers

    let decodeHeaders key₀ key₁ = do
            let expected :: Expr Src X
                expected = App List (Record (Dhall.Map.fromList [ (key₀, Text), (key₁, Text) ]))
            let annot = case expr of
                    Note (Src begin end bytes) _ ->
                        let bytes' = bytes <> " : "
                              <> Dhall.Pretty.Internal.prettyToStrictText expected
                        in Note (Src begin end bytes') (Annot expr expected)
                    _ -> Annot expr expected
            case Dhall.TypeCheck.typeOf annot of
                Left err -> liftIO (throwIO err)
                Right _  -> return ()
            let expr' = Dhall.Core.normalize expr
            case toHeaders key₀ key₁ expr' of
                Just httpHeaders -> return httpHeaders
                Nothing -> liftIO (throwIO InternalError)

    let handler₀ (e :: SomeException) = do
            let handler₁ (_ :: SomeException) =
                    Exception.throw e
            Exception.handle handler₁ (decodeHeaders "header" "value")

    httpHeaders <-
        liftIO (Exception.handle handler₀ (decodeHeaders "mapKey" "mapValue"))

    let resolvedImportType = ResolvedRemote resolvedUrl (Just httpHeaders)
    return (ResolvedImport mHash resolvedImportType mode)

{- Local prefix file₀ <> Local Here file₁ = Local prefix (file₀ <> file₁) -}
resolveImport (ResolvedImport _ (ResolvedLocal prefix file) _ :| _)
  (Import (ImportHashed mHash (Local Here file')) mode) =
    return (ResolvedImport mHash resolvedImportType mode)
  where
    resolvedFile = canonicalize (file <> file')
    resolvedImportType = ResolvedLocal prefix resolvedFile

{- Remote (URL { path = path₀, ..}) <> Local Here path₁ =
       Remote (URL { path = path₀ <> path₁, ..}) -}
resolveImport (ResolvedImport _ (ResolvedRemote (URL {..}) mHeaders) _ :| _)
  (Import (ImportHashed mHash (Local Here file)) mode) =
    return (ResolvedImport mHash resolvedImportType mode)
  where
    resolvedUrl = URL { path = canonicalize (path <> file), ..}
    resolvedImportType = ResolvedRemote resolvedUrl mHeaders

{- Local prefix file₀ <> Local Parent file₁ =
       Local prefix (file₀ <> parent <> file₁) -}
resolveImport (ResolvedImport _ (ResolvedLocal prefix file) _ :| _)
  (Import (ImportHashed mHash (Local Parent file')) mode) =
    return (ResolvedImport mHash resolvedImportType mode)
  where
    resolvedFile = canonicalize (file <> parent <> file')
    resolvedImportType = ResolvedLocal prefix resolvedFile

{- Remote (URL { path = path₀, .. }) <> Local Parent path₁ =
       Remote (URL { path = path₀ <> parent <> path₁, .. }) -}
chainImport (ResolvedImport _ (ResolvedRemote (URL {..}) mHeaders) _)
  (Import (ImportHashed mHash (Local Parent file)) mode) =
    return (ResolvedImport mHash resolvedImportType mode)
  where
    resolvedUrl = URL { path = canonicalize (path <> parent <> file), ..}
    resolvedImportType = ResolvedRemote resolvedUrl mHeaders

{- _ <> import₁ =
       import₁ -}
-- case _ <> Env env = Env env
resolveImport _ (Import (ImportHashed mHash (Env env)) mode) =
    return (ResolvedImport mHash (ResolvedEnv env) mode)

resolveImport _ (Import (ImportHashed mHash Missing) mode) =
    return (ResolvedImport mHash ResolvedMissing mode)

resolveImport _ (Import (ImportHashed mHash (Local prefix file)) mode) =
    return (ResolvedImport mHash resolvedImportType mode)
  where
    resolvedFile = canonicalize file
    resolvedImportType = ResolvedLocal prefix resolvedFile


-- | Queries the "hot" cache (in `Status`); defers to
--   `processImportWithSemanticCache` in general.
processImport
  :: ImportStack -> ResolvedImport -> StateT (Status IO) IO SemanticImport
processImport stack import_ = do
    cache_ <- use cache
    case Map.lookup import_ cache_ of
        Just semanticImport -> return semanticImport
        Nothing -> do
            semanticImport <- processImportWithSemanticCache stack resolvedImport
            modifying cache (Map.insert import_ semanticImport)
            return semanticImport

-- Checks the "semantic" disk cache; defers to
-- `processImportWithSemisemanticCache` in general.
processImportWithSemanticCache
  :: ImportStack -> ResolvedImport -> StateT (Status IO) IO SemanticImport
-- Pass on non-frozen imports.
processImportWithSemanticCache stack import_@(Import (ImportHashed Nothing _) _) =
    processImportWithSemisemanticCache stack import_

processImportWithSemanticCache stack
  import_@(Import (ImportHashed (Just semanticHash) _) _) = do
    mCached <- fetchFromSemanticCache semanticHash
    case mCached of
        Just bytes -> do
            let actualHash = Crypto.Hash.hash bytesStrict
            if semanticHash == actualHash
                then return ()
                else throwMissingImport (Imported _stack (HashMismatch {..}))

            term <- Dhall.Core.throws (Codec.Serialise.deserialiseOrFail bytesLazy)
            normalisedImport <- Dhall.Core.throws (Dhall.Binary.decodeExpression term)
            -- a frozen import does not have any dependencies if loaded from the semantic cache.
            let graph = Node import_ []
            return (SemanticImport {..})

        Nothing -> do  -- continue with semi-semantic cache
            SemanticImport { normalisedImport = normalisedImport, graph = Node _ imports }
                <- processImportWithSemisemanticCache stack import_

            -- The semi-semantic cache always returns a semantic hash computed
            -- with the default `StandardVersion`. In general we will need to
            -- check all possible versions to find the one that matches the
            -- requested semantic hash, if it exists.
            let alphaNormal = Dhall.Core.alphaNormalize (normalisedImport result)
            let variants = map (\version -> encodeExpression version alphaNormal)
                               [ minBound .. maxBound ]
            case find ((== semanticHash). Crypto.Hash.hash) of
                Just bytes -> writeToSemanticCache semanticHash bytes
                Nothing -> throwMissingImport (Imported stack (HashMismatch {..}))

            return (SemanticImport { graph = Node import_ imports, .. })

-- Try to load from "semi-semantic" disk cache, otherwise typecheck and
-- normalise from scratch.
processImportWithSemisemanticCache
  :: ImportStack -> ResolvedImport -> StateT (Status IO) IO SemanticImport
-- `as Location` imports aren't cached "semi-semantically"
processImportWithSemisemanticCache stack
  import_@(ResolvedImport (Import (ImportHashed _ importType) Location)) = do
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
    -- note that normalisedImport is already alpha-normal since it doesn't contain any variables
    let semanticHash = hashExpression (normalisedImport)
    -- `as Location` doesn't actually touch anything
    let graph = Node import_ []
    return (SemanticImport {..})

-- `as Text` imports aren't cached "semi-semantically" either
processImportWithSemisemanticCache stack
  import_@(ResolvedImport (Import (ImportHashed _ importType) RawText)) = do
    text <- fetchFresh importType
    let normalisedImport = TextLit (Chunks [] text)
    let semanticHash = hashExpression (normalisedImport)
    let graph = Node import_ []
    return (SemanticImport {..})

processImportWithSemisemanticCache stack
  import_@(ResolvedImport (Import (ImportHashed _ importType) Code)) = do
    text <- fetchFresh importType

    path <- case importType of
        Local prefix file = localToPath prefix file
        Remote url -> Text.unpack (renderURL url)
        Env env -> Text.unpack env
        Missing -> liftIO $ throwM (MissingImports [])
    let parser = unParser $ do
            Text.Parser.Token.whiteSpace
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

    -- Check semi-semantic cache
    let semisemanticHash = hashSemisemantic loaded
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
    return (SemanticImport {..})


-- Fetch source code directly from disk/network
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

-- Fetch encoded normal form from "semantic cache"
fetchFromSemanticCache :: SemanticHash -> IO (Maybe ByteString)
fetchFromSemanticCache expectedHash = Maybe.runMaybeT $ do
    cacheFile <- getCacheFile expectedHash
    True <- liftIO (Directory.doesFileExist cacheFile)
    liftIO (Data.ByteString.readFile cacheFile)

-- Fetch encoded normal form from "semi-semantic cache"
fetchFromSemisemanticCache :: SemisemanticHash -> IO (Maybe ByteString)
fetchFromSemisemanticCache semisemanticHash = Maybe.runMaybeT $ do
        cacheFile <- getCacheFile semisemanticHash  -- TODO: use different cache directory
        True <- liftIO (Directory.doesFileExist cacheFile)
        liftIO (Data.ByteString.readFile cacheFile)

-- write to "semantic cache"
writeToSemanticCache :: SemanticHash -> ByteString -> IO ()
writeToSemanticCache semanticHash bytes = do
    Maybe.runMaybeT $ do
        cacheFile <- getCacheFile semanticHash
        liftIO (Data.ByteString.writeFile cacheFile bytes)
    return ()

-- write to "semi-semantic cache"
writeToSemisemanticCache :: SemisemanticHash -> ByteString -> IO ()
writeToSemisemanticCache semisemanticHash bytes = do
    Maybe.runMaybeT $ do
        cacheFile <- getCacheFile semisemanticHash  -- TODO: different cache location
        liftIO (Data.ByteString.writeFile cacheFile bytes)
    return ()

-- | Load an expression, i.e. replace all imports with their semantics (normal
--   forms). The resulting expression is neither typechecked nor normalised!
loadExpr :: ImportStack -> Expr Src Import -> StateT (Status IO) IO) LoadedExpr
loadExpr stack expr = do
    (loadedExpr, imports) <- runWriter (loadExpr' stack expr)
    return (LoadedExpr {..})

-- Use the writer monad to keep track of any imports
loadExpr' :: ImportStack -> Expr Src Import ->
                WriterT [ResolvedImport] (StateT (Status IO) IO) (Expr Src X)
loadExpr' stack (Embed import_) = do
    let handler₀ (MissingImports es) =
            throwM (MissingImports (map (\e -> toException (Imported _stack' e)) es))
        handler₁ e = throwMissingImport (Imported _stack' e :: Imported SomeException)

    resolvedImport <- lift $ resolveImport stack import_
        `catches` [ Handler handler₀, Handler handler₁ ]
    semanticImport@SemanticImport {..} <- lift $ processImport stack resolvedImport
        `catches` [ Handler handler₀, Handler handler₁ ]

    tell semanticImport
    return normalisedImport

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

-- map over subexpressions
loadExpr' stack expr = do
    let go = fmap exprXToImport . loadExpr' stack
        :: Expr Src Import -> WriterT [ResolvedImport] (StateT (Status IO) IO) (Expr Src Import)
    expr' <- Dhall.Core.subExpressions go expr
    let loadedExpr = fmap undefined expr'  -- this is safe since expr' doesn't contain imports!
    return loadedExpr


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

-- "semi-semantic" hash, calculated by concatenating the code for the
-- _unnormalised_ (and non-typechecked) syntax tree with the semantic hashes of all imports
hashSemisemantic :: StandardVersion -> LoadedExpr -> SemisemanticHash
hashSemisemantic standardVersion_ (LoadedExpr {..}) =
    SemisemanticHash (hash semisemanticContent)
  where
    syntacticHash = Crypto.Hash.hash (encodeExpression standardVersion_ loadedExpr)
    semanticHashes = map (fromSemanticHash . semanticHash . rootLabel) imports
    semisemanticContent = Data.ByteArray.concat (syntacticHash : semanticHashes)
        :: ByteString

-- | Hash a fully resolved expression. To compute the semantic hash the
--   expression has to be alpha-beta-normal.
hashExpression
    :: StandardVersion -> Expr s X -> Crypto.Hash.Digest SHA256
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
