{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wall #-}

{-| Dhall lets you import external expressions located either in local files or
    hosted on network endpoints.

    To import a local file as an expression, just insert the path to the file,
    prepending a @./@ if the path is relative to the current directory.  For
    example, suppose we had the following three local files:

    > -- id
    > \(a : *) -> \(x : a) -> x

    > -- Bool
    > forall (Bool : *) -> forall (True : Bool) -> forall (False : Bool) -> Bool

    > -- True
    > \(Bool : *) -> \(True : Bool) -> \(False : Bool) -> True

    You could then reference them within a Dhall expression using this syntax:

    > ./id ./Bool ./True

    ... which would embed their expressions directly within the syntax tree:

    > -- ... expands out to:
    > (\(a : *) -> \(x : a) -> x)
    >     (forall (Bool : *) -> forall (True : Bool) -> forall (False : Bool) -> True)
    >     (\(Bool : *) -> \(True : Bool) -> \(False : Bool) -> True)

    ... and which normalizes to:

    > λ(Bool : *) → λ(True : Bool) → λ(False : Bool) → True

    Imported expressions may contain imports of their own, too, which will
    continue to be resolved.  However, Dhall will prevent cyclic imports.  For
    example, if you had these two files:

    > -- foo
    > ./bar

    > -- bar
    > ./foo

    ... Dhall would throw the following exception if you tried to import @foo@:

    > dhall: 
    > ⤷ ./foo
    > ⤷ ./bar
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
-}

module Dhall.Import (
    -- * Import
      load
    , exprFromFile
    , exprFromURL
    , Cycle(..)
    , ReferentiallyOpaque(..)
    , Imported(..)
    , PrettyHttpException(..)
    , MissingFile(..)
    ) where

import Control.Exception
    (Exception, IOException, SomeException, catch, onException, throwIO)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.State.Strict (StateT)
import Data.ByteString.Lazy (ByteString)
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
import Lens.Micro (Lens')
import Lens.Micro.Mtl (zoom)
import Dhall.Core (Expr, Path(..))
import Dhall.Parser (Parser(..), ParseError(..), Src)
import Dhall.TypeCheck (X(..))
import Network.HTTP.Client (HttpException(..), Manager)
import Prelude hiding (FilePath)
import Text.Trifecta (Result(..))
import Text.Trifecta.Delta (Delta(..))

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.ByteString.Lazy
import qualified Data.Foldable                    as Foldable
import qualified Data.List                        as List
import qualified Data.Map.Strict                  as Map
import qualified Data.Text.Lazy                   as Text
import qualified Data.Text.Lazy.Builder           as Builder
import qualified Data.Text.Lazy.Encoding
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Filesystem
import qualified Filesystem.Path.CurrentOS
import qualified Network.HTTP.Client              as HTTP
import qualified Network.HTTP.Client.TLS          as HTTP
import qualified Filesystem.Path.CurrentOS        as Filesystem
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
    with the compiled result.  Let's term this \"static linking\".  Dhall (very
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
        ++  unlines (map (\(n, path) -> take (2 * n) (repeat ' ') ++ "↳ " ++ builderToString (build path)) paths')
        ++  show e
      where
        -- Canonicalize all paths
        paths' = zip [0..] (drop 1 (reverse (canonicalizeAll paths)))

newtype PrettyHttpException = PrettyHttpException HttpException
    deriving (Typeable)

instance Exception PrettyHttpException

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
            <>  show e'

data MissingFile = MissingFile
    deriving (Typeable)

instance Exception MissingFile

instance Show MissingFile where
    show MissingFile =
            "\n"
        <>  "\ESC[1;31mError\ESC[0m: Missing file\n"

data Status = Status
    { _stack   :: [Path]
    , _cache   :: Map Path (Expr Src X)
    , _manager :: Maybe Manager
    }

canonicalizeAll :: [Path] -> [Path]
canonicalizeAll = map canonicalize . List.tails

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
                    { HTTP.managerResponseTimeout = Just 1000000 }  -- 1 second
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

    Also, there are way too many `reverse`s in the URL-handling cod  For now I
    don't mind, but if were to really do this correctly we'd store the URLs as
    `Text` for O(1) access to the end of the string.  The only reason we use
    `String` at all is for consistency with the @http-client@ library.
-}
canonicalize :: [Path] -> Path
canonicalize  []                 = File "."
canonicalize (File file0:paths0) =
    if Filesystem.relative file0
    then go          file0 paths0
    else File (clean file0)
  where
    go currPath  []               = File (clean currPath)
    go currPath (URL  url0:_    ) = combine prefix suffix
      where
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
                        '/' -> URL (url <>        path')
                        _   -> URL (url <> "/" <> path')
                  where
                    path' = Text.fromStrict (case Filesystem.toText path of
                        Left  txt -> txt
                        Right txt -> txt )
    go currPath (File file:paths) =
        if Filesystem.relative file
        then go          file'  paths
        else File (clean file')
      where
        file' = Filesystem.parent (removeAtFromFile file) </> currPath
canonicalize (URL path:_) = URL path

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

-- | Parse an expression from a `FilePath` containing a Dhall program
exprFromFile :: FilePath -> IO (Expr Src Path)
exprFromFile path = do
    let string = Filesystem.Path.CurrentOS.encodeString path

    -- Unfortunately, GHC throws an `InappropriateType`
    -- exception when trying to read a directory, but does not
    -- export the exception, so I must resort to a more
    -- heavy-handed `catch`
    let handler :: IOException -> IO (Result (Expr Src Path))
        handler e = do
            let string' = Filesystem.Path.CurrentOS.encodeString (path </> "@")

            -- If the fallback fails, reuse the original exception to avoid user
            -- confusion
            Text.Trifecta.parseFromFileEx parser string' `onException` throwIO e

    exists <- Filesystem.isFile path
    if exists
        then return ()
        else Control.Exception.throwIO MissingFile

    x <- Text.Trifecta.parseFromFileEx parser string `catch` handler
    case x of
        Failure errInfo -> throwIO (ParseError (Text.Trifecta._errDoc errInfo))
        Success expr    -> return expr
  where
    parser = unParser (do
        Text.Parser.Token.whiteSpace
        r <- Dhall.Parser.exprA
        Text.Parser.Combinators.eof
        return r )

exprFromURL :: Manager -> Text -> IO (Expr Src Path)
exprFromURL m url = do
    request <- HTTP.parseUrlThrow (Text.unpack url)

    let handler :: HTTP.HttpException -> IO (HTTP.Response ByteString)
        handler err@(HTTP.StatusCodeException _ _ _) = do
            let request' = request { HTTP.path = HTTP.path request <> "/@" }
            -- If the fallback fails, reuse the original exception to avoid user
            -- confusion
            HTTP.httpLbs request' m `onException` throwIO (PrettyHttpException err)
        handler err = throwIO (PrettyHttpException err)
    response <- HTTP.httpLbs request m `catch` handler

    let bytes = HTTP.responseBody response

    text <- case Data.Text.Lazy.Encoding.decodeUtf8' bytes of
        Left  err  -> throwIO err
        Right text -> return text

    let urlBytes = Data.Text.Lazy.Encoding.encodeUtf8 url
    let delta = Directed (Data.ByteString.Lazy.toStrict urlBytes) 0 0 0 0
    case Text.Trifecta.parseString parser delta (Text.unpack text) of
        Failure err -> do
            -- Also try the fallback in case of a parse error, since the parse
            -- error might signify that this URL points to a directory list
            let err' = ParseError (Text.Trifecta._errDoc err)

            request' <- HTTP.parseUrlThrow (Text.unpack url)

            let request'' = request' { HTTP.path = HTTP.path request' <> "/@" }
            response' <- HTTP.httpLbs request'' m `onException` throwIO err'

            let bytes' = HTTP.responseBody response'

            text' <- case Data.Text.Lazy.Encoding.decodeUtf8' bytes' of
                Left  _     -> throwIO err'
                Right text' -> return text'

            case Text.Trifecta.parseString parser delta (Text.unpack text') of
                Failure _    -> throwIO err'
                Success expr -> return expr
        Success expr -> return expr
  where
    parser = unParser (do
        Text.Parser.Token.whiteSpace
        r <- Dhall.Parser.exprA
        Text.Parser.Combinators.eof
        return r )

{-| Load a `Path` as a \"dynamic\" expression (without resolving any imports)

    This also returns the true final path (i.e. explicit "/@" at the end for
    directories)
-}
loadDynamic :: Path -> StateT Status IO (Expr Src Path)
loadDynamic p = do
    paths <- zoom stack State.get

    let handler :: SomeException -> IO (Expr Src Path)
        handler e = throwIO (Imported (p:paths) e)
    case canonicalize (p:paths) of
        File file -> do
            liftIO (exprFromFile file `catch` handler)
        URL  url  -> do
            m <- needManager
            liftIO (exprFromURL m url `catch` handler)

-- | Load a `Path` as a \"static\" expression (with all imports resolved)
loadStatic :: Path -> StateT Status IO (Expr Src X)
loadStatic path = do
    paths <- zoom stack State.get

    let local (URL url) = case HTTP.parseUrlThrow (Text.unpack url) of
            Nothing      -> False
            Just request -> case HTTP.host request of
                "127.0.0.1" -> True
                "localhost" -> True
                _           -> False
        local (File _)  = True

    let parent = canonicalize paths
    let here   = canonicalize (path:paths)

    if local here && not (local parent)
        then liftIO (throwIO (Imported paths (ReferentiallyOpaque path)))
        else return ()

    (expr, cached) <- if here `elem` canonicalizeAll paths
        then liftIO (throwIO (Imported paths (Cycle path)))
        else do
            m <- zoom cache State.get
            case Map.lookup here m of
                Just expr -> return (expr, True)
                Nothing   -> do
                    expr'  <- loadDynamic path
                    expr'' <- case traverse (\_ -> Nothing) expr' of
                        -- No imports left
                        Just expr -> do
                            zoom cache (State.put $! Map.insert here expr m)
                            return expr
                        -- Some imports left, so recurse
                        Nothing   -> do
                            let paths' = path:paths
                            zoom stack (State.put paths')
                            expr'' <- fmap join (traverse loadStatic expr')
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
        else case Dhall.TypeCheck.typeOf expr of
            Left  err -> liftIO (throwIO (Imported (path:paths) err))
            Right _   -> return ()

    return expr

{-| Resolve all imports within an expression

    By default the starting path is the current directory, but you can override
    the starting path with a file if you read in the expression from that file
-}
load
    :: Maybe Path
    -- ^ Starting path
    -> Expr Src Path
    -- ^ Expression to resolve
    -> IO (Expr Src X)
load here expr = State.evalStateT (fmap join (traverse loadStatic expr)) status
  where
    status = Status (Foldable.toList here) Map.empty Nothing
