{-# LANGUAGE CPP                 #-}
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
    , load
    , loadWith
    , loadWithContext
    , hashExpression
    , hashExpressionToCode
    , Status(..)
    , emptyStatus
    , Cycle(..)
    , ReferentiallyOpaque(..)
    , Imported(..)
    , PrettyHttpException(..)
    , MissingFile(..)
    , MissingEnvironmentVariable(..)
    ) where

import Control.Applicative (empty)
import Control.Exception (Exception, SomeException, throwIO)
import Control.Monad (join)
import Control.Monad.Catch (throwM, MonadCatch(catch))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.State.Strict (StateT)
import Crypto.Hash (SHA256)
import Data.CaseInsensitive (CI)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (sconcat, (<>))
import Data.Text (Text)
#if MIN_VERSION_base(4,8,0)
#else
import Data.Traversable (traverse)
#endif
import Data.Typeable (Typeable)
import System.FilePath ((</>))
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
    )
#ifdef MIN_VERSION_http_client
import Dhall.Import.HTTP
#endif
import Dhall.Import.Types

import Dhall.Parser (Parser(..), ParseError(..), Src(..))
import Dhall.TypeCheck (X(..))
import Lens.Family.State.Strict (zoom)

import qualified Control.Monad.Trans.State.Strict        as State
import qualified Crypto.Hash
import qualified Data.ByteString
import qualified Data.CaseInsensitive
import qualified Data.Foldable

import qualified Data.List                               as List
import qualified Data.HashMap.Strict.InsOrd
import qualified Data.Map.Strict                         as Map
import qualified Data.Text.Encoding
import qualified Data.Text                               as Text
import qualified Data.Text.IO
import qualified Dhall.Core
import qualified Dhall.Parser
import qualified Dhall.Context
import qualified Dhall.Pretty.Internal
import qualified Dhall.TypeCheck
import qualified System.Environment
import qualified System.Directory
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
    { importStack :: [Import] -- ^ Imports resolved so far, in reverse order
    , nested      :: e        -- ^ The nested exception
    } deriving (Typeable)

instance Exception e => Exception (Imported e)

instance Show e => Show (Imported e) where
    show (Imported imports e) =
            (case imports of [] -> ""; _ -> "\n")
        ++  unlines (map indent imports')
        ++  show e
      where
        indent (n, import_) =
            take (2 * n) (repeat ' ') ++ "↳ " ++ Dhall.Pretty.Internal.prettyToString import_
        -- Canonicalize all imports
        imports' = zip [0..] (drop 1 (reverse (canonicalizeAll imports)))


-- | Exception thrown when an imported file is missing
data MissingFile = MissingFile FilePath
    deriving (Typeable)

instance Exception MissingFile

instance Show MissingFile where
    show (MissingFile path) =
            "\n"
        <>  "\ESC[1;31mError\ESC[0m: Missing file "
        <>  path
        <>  "\n"

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

canonicalizeAll :: [Import] -> [Import]
canonicalizeAll = map canonicalizeImport . List.tails

{-|
> canonicalize (canonicalize x) = canonicalize x
-}
class Canonicalize path where
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

    canonicalize (URL prefix file suffix header) =
        URL prefix (canonicalize file) suffix header

    canonicalize (Env name) =
        Env name

instance Canonicalize ImportHashed where
    canonicalize (ImportHashed hash importType) =
        ImportHashed hash (canonicalize importType)

instance Canonicalize Import where
    canonicalize (Import importHashed importMode) =
        Import (canonicalize importHashed) importMode

canonicalizeImport :: [Import] -> Import
canonicalizeImport imports =
    canonicalize (sconcat (defaultImport :| reverse imports))
  where
    defaultImport =
        Import
            { importMode   = Code
            , importHashed = ImportHashed
                { hash       = Nothing
                , importType = Local Here (File (Directory []) ".")
                }
            }

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
    TextLit (Chunks [] keyText  ) <- Data.HashMap.Strict.InsOrd.lookup "header" m
    TextLit (Chunks [] valueText) <- Data.HashMap.Strict.InsOrd.lookup "value"  m
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

-- | Parse an expression from a `Import` containing a Dhall program
exprFromImport :: Import -> StateT Status IO (Expr Src Import)
exprFromImport (Import {..}) = do
    let ImportHashed {..} = importHashed

    (path, text) <- case importType of
        Local prefix (File {..}) -> liftIO $ do
            let Directory {..} = directory

            prefixPath <- case prefix of
                Home -> do
                    System.Directory.getHomeDirectory

                Absolute -> do
                    return "/"

                Here -> do
                    System.Directory.getCurrentDirectory

            let cs = map Text.unpack (file : components)

            let cons component dir = dir </> component

            let path = foldr cons prefixPath cs

            exists <- System.Directory.doesFileExist path

            if exists
                then return ()
                else throwIO (MissingFile path)

            text <- Data.Text.IO.readFile path

            return (path, text)

        URL prefix file suffix maybeHeaders -> do
            let fileText = Dhall.Pretty.Internal.prettyToStrictText file
            let url      = Text.unpack (prefix <> fileText <> suffix)

            mheaders <- case maybeHeaders of
                Nothing            -> return Nothing
                Just importHashed_ -> do
                    expr <- loadStaticWith
                        exprFromImport
                        Dhall.Context.empty
                        (const Nothing)
                        (Embed (Import importHashed_ Code))

                    let expected :: Expr Src X
                        expected =
                            App List
                                ( Record
                                    ( Data.HashMap.Strict.InsOrd.fromList
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
                            return (Just headers)
                        Nothing      -> do
                            liftIO (throwIO InternalError)

#ifdef MIN_VERSION_http_client
            fetchFromHttpUrl url mheaders
#else
            liftIO (throwIO (CannotImportHTTPURL url mheaders))
#endif

        Env env -> liftIO $ do
            x <- System.Environment.lookupEnv (Text.unpack env)
            case x of
                Just string -> return (Text.unpack env, Text.pack string)
                Nothing     -> throwIO (MissingEnvironmentVariable env)

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
                Right expr -> do
                    return expr

        RawText -> do
            return (TextLit (Chunks [] text))

-- | Resolve all imports within an expression using a custom typing context and
-- `Import`-resolving callback in arbitrary `MonadCatch` monad.
loadWith
    :: MonadCatch m
    => (Import -> StateT Status m (Expr Src Import))
    -> Dhall.Context.Context (Expr Src X)
    -> Dhall.Core.Normalizer X
    -> Expr Src Import
    -> m (Expr Src X)
loadWith from_import ctx n expr =
    State.evalStateT (loadStaticWith from_import ctx n expr) emptyStatus

-- | Resolve all imports within an expression using a custom typing context.
--
-- @load = loadWithContext Dhall.Context.empty@
loadWithContext
    :: Dhall.Context.Context (Expr Src X)
    -> Dhall.Core.Normalizer X
    -> Expr Src Import
    -> IO (Expr Src X)
loadWithContext ctx n expr =
    State.evalStateT (loadStaticWith exprFromImport ctx n expr) emptyStatus

-- | This loads a \"static\" expression (i.e. an expression free of imports)
loadStaticWith
    :: MonadCatch m
    => (Import -> StateT Status m (Expr Src Import))
    -> Dhall.Context.Context (Expr Src X)
    -> Dhall.Core.Normalizer X
    -> Expr Src Import
    -> StateT Status m (Expr Src X)
loadStaticWith from_import ctx n (Embed import_) = do
    imports <- zoom stack State.get

    let local (Import (ImportHashed _ (URL   {})) _) = False
        local (Import (ImportHashed _ (Local {})) _) = True
        local (Import (ImportHashed _ (Env   {})) _) = True

    let parent = canonicalizeImport imports
    let here   = canonicalizeImport (import_:imports)

    if local here && not (local parent)
        then throwM (Imported imports (ReferentiallyOpaque import_))
        else return ()

    expr <- if here `elem` canonicalizeAll imports
        then throwM (Imported imports (Cycle import_))
        else do
            m <- zoom cache State.get
            case Map.lookup here m of
                Just expr -> return expr
                Nothing   -> do
                    let handler
                            :: MonadCatch m
                            => SomeException
                            -> StateT Status m (Expr Src Import)
                        handler e = throwM (Imported (import_:imports) e)

                    -- This loads a \"dynamic\" expression (i.e. an expression
                    -- that might still contain imports)
                    let loadDynamic =
                            from_import (canonicalizeImport (import_:imports))

                    expr' <- loadDynamic `catch` handler

                    let imports' = import_:imports
                    zoom stack (State.put imports')
                    expr'' <- loadStaticWith from_import ctx n expr'
                    zoom stack (State.put imports)

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
                    expr''' <- case Dhall.TypeCheck.typeWith ctx expr'' of
                        Left  err -> throwM (Imported (import_:imports) err)
                        Right _   -> return (Dhall.Core.normalizeWith n expr'')
                    zoom cache (State.put $! Map.insert here expr''' m)
                    return expr'''

    case hash (importHashed import_) of
        Nothing -> do
            return ()
        Just expectedHash -> do
            let actualHash = hashExpression expr
            if expectedHash == actualHash
                then return ()
                else throwM (Imported (import_:imports) (HashMismatch {..}))

    return expr
loadStaticWith from_import ctx n expr = fmap join (traverse process expr)
  where
    process import_ = loadStaticWith from_import ctx n (Embed import_)

-- | Resolve all imports within an expression
load :: Expr Src Import -> IO (Expr Src X)
load = loadWithContext Dhall.Context.empty (const Nothing)

-- | Hash a fully resolved expression
hashExpression :: Expr s X -> (Crypto.Hash.Digest SHA256)
hashExpression expr = Crypto.Hash.hash actualBytes
  where
    text = Dhall.Core.pretty (Dhall.Core.normalize expr)
    actualBytes = Data.Text.Encoding.encodeUtf8 text

{-| Convenience utility to hash a fully resolved expression and return the
    base-16 encoded hash with the @sha256:@ prefix

    In other words, the output of this function can be pasted into Dhall
    source code to add an integrity check to an import
-}
hashExpressionToCode :: Expr s X -> Text
hashExpressionToCode expr = "sha256:" <> Text.pack (show (hashExpression expr))
