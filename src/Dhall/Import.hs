{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

    You can also reuse directory names as expressions.  If you provide a path
    to a local or remote directory then the compiler will look for a file named
    @\@@ within that directory and use that file to represent the directory.

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

    If you wish to import the raw contents of a path as @Text@ then add
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
      exprFromPath
    , load
    , loadWith
    , Cycle(..)
    , ReferentiallyOpaque(..)
    , Imported(..)
    , PrettyHttpException(..)
    , MissingFile(..)
    ) where

import Control.Applicative (empty)
import Control.Exception
    (Exception, IOException, SomeException, onException, throwIO)
import Control.Lens (Lens', zoom)
import Control.Monad (join)
import Control.Monad.Catch (throwM, MonadCatch(catch))
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.State.Strict (StateT)
import Data.ByteString.Lazy (ByteString)
import Data.CaseInsensitive (CI)
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.Text.Buildable (build)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder)
#if MIN_VERSION_base(4,8,0)
#else
import Data.Traversable (traverse)
#endif
import Data.Typeable (Typeable)
import Filesystem.Path ((</>), FilePath)
import Dhall.Core
    (Expr(..), HasHome(..), PathMode(..), PathType(..), Path(..))
import Dhall.Parser (Parser(..), ParseError(..), Src(..))
import Dhall.TypeCheck (X(..))
#if MIN_VERSION_http_client(0,5,0)
import Network.HTTP.Client
    (HttpException(..), HttpExceptionContent(..), Manager)
#else
import Network.HTTP.Client (HttpException(..), Manager)
#endif
import Prelude hiding (FilePath)
import Text.Trifecta (Result(..))
import Text.Trifecta.Delta (Delta(..))

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.CaseInsensitive
import qualified Data.List                        as List
import qualified Data.Map.Strict                  as Map
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy                   as Text
import qualified Data.Text.Lazy.Builder           as Builder
import qualified Data.Text.Lazy.Encoding
import qualified Data.Vector
import qualified Dhall.Core
import qualified Dhall.Parser
import qualified Dhall.Context
import qualified Dhall.TypeCheck
import qualified Filesystem
import qualified Filesystem.Path.CurrentOS
import qualified Network.HTTP.Client              as HTTP
import qualified Network.HTTP.Client.TLS          as HTTP
import qualified Filesystem.Path.CurrentOS        as Filesystem
import qualified System.Environment
import qualified Text.Parser.Combinators
import qualified Text.Parser.Token
import qualified Text.Trifecta

builderToString :: Builder -> String
builderToString = Text.unpack . Builder.toLazyText

-- | An import failed because of a cycle in the import graph
newtype Cycle = Cycle
    { cyclicImport :: Path  -- ^ The offending cyclic import
    }
  deriving (Typeable)

instance Exception Cycle

instance Show Cycle where
    show (Cycle path) = "\nCyclic import: " ++ builderToString (build path)

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
    { opaqueImport :: Path  -- ^ The offending opaque import
    } deriving (Typeable)

instance Exception ReferentiallyOpaque

instance Show ReferentiallyOpaque where
    show (ReferentiallyOpaque path) =
        "\nReferentially opaque import: " ++ builderToString (build path)

-- | Extend another exception with the current import stack
data Imported e = Imported
    { importStack :: [Path] -- ^ Imports resolved so far, in reverse order
    , nested      :: e      -- ^ The nested exception
    } deriving (Typeable)

instance Exception e => Exception (Imported e)

instance Show e => Show (Imported e) where
    show (Imported paths e) =
            (case paths of [] -> ""; _ -> "\n")
        ++  unlines (map indent paths')
        ++  show e
      where
        indent (n, path) =
            take (2 * n) (repeat ' ') ++ "↳ " ++ builderToString (build path)
        -- Canonicalize all paths
        paths' = zip [0..] (drop 1 (reverse (canonicalizeAll paths)))

-- | Newtype used to wrap `HttpException`s with a prettier `Show` instance
newtype PrettyHttpException = PrettyHttpException HttpException
    deriving (Typeable)

instance Exception PrettyHttpException

#if MIN_VERSION_http_client(0,5,0)
instance Show PrettyHttpException where
  show (PrettyHttpException (InvalidUrlException _ r)) =
    "\n"
    <>  "\ESC[1;31mError\ESC[0m: Invalid URL\n"
    <>  "\n"
    <>  "↳ " <> show r
  show (PrettyHttpException (HttpExceptionRequest _ e)) = case e of
    ConnectionFailure e' ->
      "\n"
      <>  "\ESC[1;31mError\ESC[0m: Wrong host\n"
      <>  "\n"
      <>  "↳ " <> show e'
    InvalidDestinationHost host ->
      "\n"
      <>  "\ESC[1;31mError\ESC[0m: Invalid host name\n"
      <>  "\n"
      <>  "↳ " <> show host
    ResponseTimeout ->
      "\ESC[1;31mError\ESC[0m: The host took too long to respond\n"
    e' -> "\n" <> show e'
#else
instance Show PrettyHttpException where
    show (PrettyHttpException e) = case e of
        FailedConnectionException2 _ _ _ e' ->
                "\n"
            <>  "\ESC[1;31mError\ESC[0m: Wrong host\n"
            <>  "\n"
            <>  "↳ " <> show e'
        InvalidDestinationHost host ->
                "\n"
            <>  "\ESC[1;31mError\ESC[0m: Invalid host name\n"
            <>  "\n"
            <>  "↳ " <> show host
        ResponseTimeout ->
                "\ESC[1;31mError\ESC[0m: The host took too long to respond\n"
        e' ->   "\n"
            <> show e'
#endif

-- | Exception thrown when an imported file is missing
data MissingFile = MissingFile
    deriving (Typeable)

instance Exception MissingFile

instance Show MissingFile where
    show MissingFile =
            "\n"
        <>  "\ESC[1;31mError\ESC[0m: Missing file\n"

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

data Status = Status
    { _stack   :: [Path]
    , _cache   :: Map Path (Expr Src X)
    , _manager :: Maybe Manager
    }

canonicalizeAll :: [Path] -> [Path]
canonicalizeAll = map canonicalizePath . List.tails

stack :: Lens' Status [Path]
stack k s = fmap (\x -> s { _stack = x }) (k (_stack s))

cache :: Lens' Status (Map Path (Expr Src X))
cache k s = fmap (\x -> s { _cache = x }) (k (_cache s))

manager :: Lens' Status (Maybe Manager)
manager k s = fmap (\x -> s { _manager = x }) (k (_manager s))

needManager :: StateT Status IO Manager
needManager = do
    x <- zoom manager State.get
    case x of
        Just m  -> return m
        Nothing -> do
            let settings = HTTP.tlsManagerSettings
#if MIN_VERSION_http_client(0,5,0)
                    { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro (30 * 1000 * 1000) }  -- 30 seconds
#else
                    { HTTP.managerResponseTimeout = Just (30 * 1000 * 1000) }  -- 30 seconds
#endif
            m <- liftIO (HTTP.newManager settings)
            zoom manager (State.put (Just m))
            return m

{-| This function computes the current path by taking the last absolute path
    (either an absolute `FilePath` or `URL`) and combining it with all following
    relative paths

    For example, if the file `./foo/bar` imports `./baz`, that will resolve to
    `./foo/baz`.  Relative imports are relative to a file's parent directory.
    This also works for URLs, too.

    This code is full of all sorts of edge cases so it wouldn't surprise me at
    all if you find something broken in here.  Most of the ugliness is due to:

    * Handling paths ending with @/\@@ by stripping the @/\@@ suffix if and only
      if you navigate to any downstream relative paths
    * Removing spurious @.@s and @..@s from the path

    Also, there are way too many `reverse`s in the URL-handling code For now I
    don't mind, but if were to really do this correctly we'd store the URLs as
    `Text` for O(1) access to the end of the string.  The only reason we use
    `String` at all is for consistency with the @http-client@ library.
-}
canonicalize :: [PathType] -> PathType
canonicalize  []                          = File Homeless "."
canonicalize (File hasHome0 file0:paths0) =
    if Filesystem.relative file0 && hasHome0 == Homeless
    then go file0 paths0
    else File hasHome0 (clean file0)
  where
    go currPath  []                       = File Homeless (clean currPath)
    go currPath (Env  _           :_    ) = File Homeless (clean currPath)
    go currPath (URL  url0 headers:rest ) = combine prefix suffix
      where
        headers' = fmap (\h -> canonicalize (h:rest)) headers

        prefix = parentURL (removeAtFromURL url0)

        suffix = clean currPath

        -- `clean` will resolve internal @.@/@..@'s in @currPath@, but we still
        -- need to manually handle @.@/@..@'s at the beginning of the path
        combine url path = case Filesystem.stripPrefix ".." path of
            Just path' -> combine url' path'
              where
                url' = parentURL (removeAtFromURL url)
            Nothing    -> case Filesystem.stripPrefix "." path of
                Just path' -> combine url path'
                Nothing    ->
                    -- This `last` is safe because the lexer constrains all
                    -- URLs to be non-empty.  I couldn't find a simple and safe
                    -- equivalent in the `text` API
                    case Text.last url of
                        '/' -> URL (url <>        path') headers'
                        _   -> URL (url <> "/" <> path') headers'
                  where
                    path' = Text.fromStrict (case Filesystem.toText path of
                        Left  txt -> txt
                        Right txt -> txt )
    go currPath (File hasHome file:paths) =
        if Filesystem.relative file && hasHome == Homeless
        then go file' paths
        else File hasHome (clean file')
      where
        file' = Filesystem.parent (removeAtFromFile file) </> currPath
canonicalize (URL path headers:rest) = URL path headers'
  where
    headers' = fmap (\h -> canonicalize (h:rest)) headers
canonicalize (Env env         :_   ) = Env env

canonicalizePath :: [Path] -> Path
canonicalizePath [] =
    Path
        { pathMode = Code
        , pathType = canonicalize []
        }
canonicalizePath (path:paths) =
    Path
        { pathMode = pathMode path
        , pathType = canonicalize (map pathType (path:paths))
        }

parentURL :: Text -> Text
parentURL = Text.dropWhileEnd (/= '/')

removeAtFromURL:: Text -> Text
removeAtFromURL url
    | Text.isSuffixOf "/@" url = Text.dropEnd 2 url
    | Text.isSuffixOf "/"  url = Text.dropEnd 1 url
    | otherwise                =                url

removeAtFromFile :: FilePath -> FilePath
removeAtFromFile file =
    if Filesystem.filename file == "@"
    then Filesystem.parent file
    else file

-- | Remove all @.@'s and @..@'s in the path
clean :: FilePath -> FilePath
clean = strip . Filesystem.collapse
  where
    strip p = case Filesystem.stripPrefix "." p of
        Nothing -> p
        Just p' -> p'

toHeaders
  :: Expr s a
  -> Maybe [(CI Data.ByteString.ByteString, Data.ByteString.ByteString)]
toHeaders (ListLit _ hs) = do
    hs' <- mapM toHeader hs
    return (Data.Vector.toList hs')
toHeaders  _             = do
    empty

toHeader
  :: Expr s a
  -> Maybe (CI Data.ByteString.ByteString, Data.ByteString.ByteString)
toHeader (RecordLit m) = do
    TextLit keyBuilder   <- Map.lookup "header" m
    TextLit valueBuilder <- Map.lookup "value"  m
    let keyText   = Text.toStrict (Builder.toLazyText keyBuilder  )
    let valueText = Text.toStrict (Builder.toLazyText valueBuilder)
    let keyBytes   = Data.Text.Encoding.encodeUtf8 keyText
    let valueBytes = Data.Text.Encoding.encodeUtf8 valueText
    return (Data.CaseInsensitive.mk keyBytes, valueBytes)
toHeader _ = do
    empty


{-| This exception indicates that there was an internal error in Dhall's
    import-related logic
    the `expected` type then the `extract` function must succeed.  If not, then
    this exception is thrown

    This exception indicates that an invalid `Type` was provided to the `input`
    function
-}
data InternalError = InternalError deriving (Typeable)

_ERROR :: String
_ERROR = "\ESC[1;31mError\ESC[0m"

instance Show InternalError where
    show InternalError = unlines
        [ _ERROR <> ": Compiler bug                                                        "
        , "                                                                                "
        , "Explanation: This error message means that there is a bug in the Dhall compiler."
        , "You didn't do anything wrong, but if you would like to see this problem fixed   "
        , "then you should report the bug at:                                              "
        , "                                                                                "
        , "https://github.com/Gabriel439/Haskell-Dhall-Library/issues                      "
        , "                                                                                "
        , "Please include the following text in your bug report:                           "
        , "                                                                                "
        , "```                                                                             "
        , "Header extraction failed even though the header type-checked                    "
        , "```                                                                             "
        ]

instance Exception InternalError

-- | Parse an expression from a `Path` containing a Dhall program
exprFromPath :: Manager -> Path -> IO (Expr Src Path)
exprFromPath m (Path {..}) = case pathType of
    File hasHome file -> do
        path <- case hasHome of
            Home -> do
                home <- liftIO Filesystem.getHomeDirectory
                return (home </> file)
            Homeless -> do
                return file

        case pathMode of
            Code -> do
                exists <- Filesystem.isFile path
                if exists
                    then return ()
                    else Control.Exception.throwIO MissingFile

                let string = Filesystem.Path.CurrentOS.encodeString path

                -- Unfortunately, GHC throws an `InappropriateType` exception
                -- when trying to read a directory, but does not export the
                -- exception, so I must resort to a more heavy-handed `catch`
                let handler :: IOException -> IO (Result (Expr Src Path))
                    handler e = do
                        let string' =
                                Filesystem.Path.CurrentOS.encodeString
                                    (path </> "@")

                        -- If the fallback fails, reuse the original exception
                        -- to avoid user confusion
                        Text.Trifecta.parseFromFileEx parser string'
                            `onException` throwIO e

                x <- Text.Trifecta.parseFromFileEx parser string `catch` handler
                case x of
                    Failure errInfo -> do
                        throwIO (ParseError (Text.Trifecta._errDoc errInfo))
                    Success expr -> do
                        return expr
            RawText -> do
                text <- Filesystem.readTextFile path
                return (TextLit (build text))
    URL url headerPath -> do
        request <- HTTP.parseUrlThrow (Text.unpack url)

        let handler :: HTTP.HttpException -> IO (HTTP.Response ByteString)
#if MIN_VERSION_http_client(0,5,0)
            handler err@(HttpExceptionRequest _ (StatusCodeException _ _)) = do
#else
            handler err@(StatusCodeException _ _ _) = do
#endif
                let request' = request { HTTP.path = HTTP.path request <> "/@" }
                -- If the fallback fails, reuse the original exception to avoid
                -- user confusion
                HTTP.httpLbs request' m `onException` throwIO (PrettyHttpException err)
            handler err = throwIO (PrettyHttpException err)

        requestWithHeaders <- case headerPath of
            Nothing   -> return request
            Just path -> do
                expr <- load (Embed (Path path Code))
                let expected :: Expr Src X
                    expected =
                        App List
                            ( Record
                                ( Map.fromList
                                    [("header", Text), ("value", Text)]
                                )
                            )
                let suffix =
                        ( Data.ByteString.Lazy.toStrict
                        . Data.Text.Lazy.Encoding.encodeUtf8
                        . Builder.toLazyText
                        . build
                        ) expected
                let annot = case expr of
                        Note (Src begin end bytes) _ ->
                            Note (Src begin end bytes') (Annot expr expected)
                          where
                            bytes' = bytes <> " : " <> suffix
                        _ ->
                            Annot expr expected
                case Dhall.TypeCheck.typeOf annot of
                    Left err -> Control.Exception.throwIO err
                    Right _  -> return ()
                let expr' = Dhall.Core.normalize expr
                headers <- case toHeaders expr' of
                    Just headers -> return headers
                    Nothing      -> Control.Exception.throwIO InternalError
                let requestWithHeaders = request
                        { HTTP.requestHeaders = headers
                        }
                return requestWithHeaders
        response <- HTTP.httpLbs requestWithHeaders m `catch` handler

        let bytes = HTTP.responseBody response

        text <- case Data.Text.Lazy.Encoding.decodeUtf8' bytes of
            Left  err  -> throwIO err
            Right text -> return text

        case pathMode of
            Code -> do
                let urlBytes = Data.Text.Lazy.Encoding.encodeUtf8 url
                let delta =
                        Directed (Data.ByteString.Lazy.toStrict urlBytes) 0 0 0 0
                case Text.Trifecta.parseString parser delta (Text.unpack text) of
                    Failure err -> do
                        -- Also try the fallback in case of a parse error, since
                        -- the parse error might signify that this URL points to
                        -- a directory list
                        let err' = ParseError (Text.Trifecta._errDoc err)

                        request' <- HTTP.parseUrlThrow (Text.unpack url)

                        let request'' =
                                request'
                                    { HTTP.path = HTTP.path request' <> "/@" }
                        response' <- HTTP.httpLbs request'' m
                            `onException` throwIO err'

                        let bytes' = HTTP.responseBody response'

                        text' <- case Data.Text.Lazy.Encoding.decodeUtf8' bytes' of
                            Left  _     -> throwIO err'
                            Right text' -> return text'

                        case Text.Trifecta.parseString parser delta (Text.unpack text') of
                            Failure _    -> throwIO err'
                            Success expr -> return expr
                    Success expr -> return expr
            RawText -> do
                return (TextLit (build text))
    Env env -> do
        x <- System.Environment.lookupEnv (Text.unpack env)
        case x of
            Just str -> do
                case pathMode of
                    Code -> do
                        let envBytes = Data.Text.Lazy.Encoding.encodeUtf8 env
                        let delta =
                                Directed (Data.ByteString.Lazy.toStrict envBytes) 0 0 0 0
                        case Text.Trifecta.parseString parser delta str of
                            Failure errInfo -> do
                                throwIO (ParseError (Text.Trifecta._errDoc errInfo))
                            Success expr    -> do
                                return expr
                    RawText -> return (TextLit (build str))
            Nothing  -> throwIO (MissingEnvironmentVariable env)
  where
    parser = unParser (do
        Text.Parser.Token.whiteSpace
        r <- Dhall.Parser.expr
        Text.Parser.Combinators.eof
        return r )

{-| Load a `Path` as a \"dynamic\" expression (without resolving any imports)

    This also returns the true final path (i.e. explicit "/@" at the end for
    directories)
-}
loadDynamic :: forall m . MonadCatch m => (Path -> m (Expr Src Path))
    -> Path -> StateT Status m (Expr Src Path)
loadDynamic from_path p = do
    paths <- zoom stack State.get

    let handler :: SomeException -> m (Expr Src Path)
        handler e = throwM (Imported (p:paths) e)

    lift (from_path (canonicalizePath (p:paths)) `catch` handler)

loadStaticIO :: Dhall.Context.Context (Expr Src X) -> Path -> StateT Status IO (Expr Src X)
loadStaticIO ctx path = do
    m <- needManager
    loadStaticWith (exprFromPath m) ctx path

-- | Resolve all imports within an expression using a custom typing context and Path
-- resolving callback in arbitrary `MonadCatch` monad.
loadWith :: MonadCatch m => (Path -> m (Expr Src Path))
    -> Dhall.Context.Context (Expr Src X) -> Expr Src Path -> m (Expr Src X)
loadWith from_path ctx = evalStatus (loadStaticWith from_path ctx)

loadStaticWith :: MonadCatch m => (Path -> m (Expr Src Path))
    -> Dhall.Context.Context (Expr Src X) -> Path -> StateT Status m (Expr Src X)
loadStaticWith from_path ctx path = do
    paths <- zoom stack State.get

    let local (Path (URL url _) _) =
            case HTTP.parseUrlThrow (Text.unpack url) of
                Nothing      -> False
                Just request -> case HTTP.host request of
                    "127.0.0.1" -> True
                    "localhost" -> True
                    _           -> False
        local (Path (File _ _ ) _) = True
        local (Path (Env  _   ) _) = True

    let parent = canonicalizePath paths
    let here   = canonicalizePath (path:paths)

    if local here && not (local parent)
        then throwM (Imported paths (ReferentiallyOpaque path))
        else return ()

    (expr, cached) <- if here `elem` canonicalizeAll paths
        then throwM (Imported paths (Cycle path))
        else do
            m <- zoom cache State.get
            case Map.lookup here m of
                Just expr -> return (expr, True)
                Nothing   -> do
                    expr'  <- loadDynamic from_path path
                    expr'' <- case traverse (\_ -> Nothing) expr' of
                        -- No imports left
                        Just expr -> do
                            zoom cache (State.put $! Map.insert here expr m)
                            return expr
                        -- Some imports left, so recurse
                        Nothing   -> do
                            let paths' = path:paths
                            zoom stack (State.put paths')
                            expr'' <- fmap join (traverse (loadStaticWith from_path ctx)
                                                           expr')
                            zoom stack (State.put paths)
                            return expr''
                    return (expr'', False)

    -- Type-check expressions here for two separate reasons:
    --
    --  * to verify that they are closed
    --  * to catch type errors as early in the import process as possible
    --
    -- There is no need to check expressions that have been cached, since they
    -- have already been checked
    if cached
        then return ()
        else case Dhall.TypeCheck.typeWith ctx expr of
            Left  err -> throwM (Imported (path:paths) err)
            Right _   -> return ()

    return expr

evalStatus :: (Traversable f, Monad m, Monad f) =>
                    (a -> StateT Status m (f b)) -> f a -> m (f b)
evalStatus cb expr = State.evalStateT (fmap join (traverse cb expr)) status
  where
    status = Status [] Map.empty Nothing

-- | Resolve all imports within an expression
load :: Expr Src Path -> IO (Expr Src X)
load = evalStatus (loadStaticIO Dhall.Context.empty)
