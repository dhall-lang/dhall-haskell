{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# OPTIONS_GHC
  -Wall
  -fno-warn-unused-matches
  -fno-warn-unused-top-binds
  -fno-warn-name-shadowing
  #-}

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
      Cycle(..)
    , ReferentiallyOpaque(..)
    , Imported(..)
    , ImportResolutionDisabled(..)
    , PrettyHttpException(..)
    , MissingFile(..)
    , MissingEnvironmentVariable(..)
    , MissingImports(..)
    , rawFromImport
    , resolve
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Applicative (Alternative(..))
import Control.Exception (Exception, SomeException, throwIO, toException)
import Control.Monad.Catch (throwM, MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Data.CaseInsensitive (CI)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
#if MIN_VERSION_base(4,8,0)
#else
import Data.Traversable (traverse)
#endif
import Data.Typeable (Typeable)
import Data.IORef
import System.FilePath ((</>))
import Dhall.Context
import Dhall.Eval
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
    )
#ifdef MIN_VERSION_http_client
import Dhall.Import.HTTP
#endif

import {-# SOURCE #-} Dhall.Elaboration

import Dhall.Parser (Parser(..), ParseError(..))

import qualified Data.ByteString
import qualified Data.CaseInsensitive
import qualified Data.Foldable
import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Text.Encoding
import qualified Data.Text                        as Text
import qualified Data.Text.IO
import qualified Data.Map.Strict                  as Map
import qualified Dhall.Map
import qualified Dhall.Parser
import qualified Dhall.Pretty.Internal
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
        Remote (URL { path = canonicalize path, headers = fmap canonicalize headers, ..})

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

rawFromImport :: Import -> ElabM Raw
rawFromImport (Import {..}) = do
    let ImportHashed {..} = importHashed

    (path, text) <- case importType of
        Local prefix file -> liftIO $ do
            path   <- localToPath prefix file
            exists <- Directory.doesFileExist path
            unless exists $ throwMissingImport (MissingFile path)
            text <- Data.Text.IO.readFile path
            pure (path, text)

        Remote url@URL { headers = maybeHeaders } -> do
            mheaders <- forM maybeHeaders $ \_ -> error "headers not yet supported"

#ifdef MIN_VERSION_http_client
            fetchFromHttpUrl url mheaders
#else
            let urlString = Text.unpack (Dhall.Core.pretty url)
            liftIO (throwIO (CannotImportHTTPURL urlString mheaders))
#endif

        Env env -> liftIO $ do
            x <- System.Environment.lookupEnv (Text.unpack env)
            case x of
                Just string -> pure (Text.unpack env, Text.pack string)
                Nothing     -> throwMissingImport (MissingEnvironmentVariable env)

        Missing -> liftIO $ do
            throwM (MissingImports [])

    case importMode of
        Code -> do
            let parser = unParser $ do
                    Text.Parser.Token.whiteSpace
                    r <- Dhall.Parser.expr
                    Text.Parser.Combinators.eof
                    pure r

            case Text.Megaparsec.parse parser path text of
                Left errInfo -> do
                    liftIO (throwIO (ParseError errInfo text))
                Right expr -> do
                    pure expr

        RawText -> do
            pure (TextLit (Chunks [] text))

resolve :: Import -> ElabM (Core, Val, VType)
resolve import0 = do
  ImportState {..} <- ask

  let parent  = NonEmpty.head _stack
      import1 = parent <> import0
      child   = canonicalize import1

  let localImport (Import (ImportHashed _ ty) _) = case ty of
        Remote{} -> False
        _        -> True

  when (localImport child && not (localImport parent)) $
    throwMissingImport (Imported _stack (ReferentiallyOpaque import0))

  when (child `elem` _stack) $
    throwMissingImport (Imported _stack (Cycle import0))

  (Map.lookup child <$> liftIO (readIORef _cache)) >>= \case
    Just (t, tv, a) -> pure (t, tv, a)
    Nothing     -> do
      t      <- rawFromImport child
      (t, a) <- local (\s -> s {_stack = NonEmpty.cons child _stack})
                      (infer emptyCxt t)
      let ~tv = eval Empty t
      liftIO $ modifyIORef' _cache $ Map.insert child (t, tv, a)
      pure (t, tv, a)

-- | A call to `assertNoImports` failed because there was at least one import
data ImportResolutionDisabled = ImportResolutionDisabled deriving (Exception)

instance Show ImportResolutionDisabled where
    show _ = "\nImport resolution is disabled"
