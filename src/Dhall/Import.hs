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
    , loadDirWith
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
    , MissingImports(..)
    , expressionToTerm
    ) where

import Codec.CBOR.Term (Term(..))
import Control.Applicative (empty)
import Control.Exception (Exception, SomeException, throwIO, toException)
import Control.Monad.Catch (throwM, MonadCatch(catch), catches, Handler(..))
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
    ( Const(..)
    , Expr(..)
    , Chunks(..)
    , Directory(..)
    , File(..)
    , FilePrefix(..)
    , ImportHashed(..)
    , ImportType(..)
    , ImportMode(..)
    , Import(..)
    , Scheme(..)
    , Var(..)
    )
#ifdef MIN_VERSION_http_client
import Dhall.Import.HTTP
#endif
import Dhall.Import.Types

import Dhall.Parser (Parser(..), ParseError(..), Src(..))
import Dhall.TypeCheck (X(..))
import Lens.Family.State.Strict (zoom)
import Prelude hiding (exponent)

import qualified Control.Monad.Trans.State.Strict        as State
import qualified Crypto.Hash
import qualified Data.ByteString
import qualified Data.CaseInsensitive
import qualified Data.Foldable

import qualified Data.HashMap.Strict.InsOrd
import qualified Data.List.NonEmpty                      as NonEmpty
import qualified Data.Map.Strict                         as Map
import qualified Data.Scientific
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
    { importStack :: NonEmpty Import -- ^ Imports resolved so far, in reverse order
    , nested      :: e               -- ^ The nested exception
    } deriving (Typeable)

instance Exception e => Exception (Imported e)

instance Show e => Show (Imported e) where
    show (Imported imports e) =
           concat (zipWith indent [0..] toDisplay)
        ++ show e
      where
        indent n import_ =
            "\n" ++ replicate (2 * n) ' ' ++ "↳ " ++ Dhall.Pretty.Internal.prettyToString import_
        canonical = NonEmpty.toList (canonicalizeAll imports)
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

-- | List of Exceptions we encounter while resolving Import Alternatives
newtype MissingImports = MissingImports [SomeException]

instance Exception MissingImports

instance Show MissingImports where
    show (MissingImports []) =
            "\n"
        <>  "\ESC[1;31mError\ESC[0m: No valid imports"
        <>  "\n"
    show (MissingImports [e]) = show e
    show (MissingImports es) =
            "\n"
        <>  "\ESC[1;31mError\ESC[0m: Failed to resolve imports. Error list:"
        <>  "\n"
        <>  concatMap (\e -> "\n" <> show e <> "\n") es
        <>  "\n"

throwMissingImport :: (MonadCatch m, Exception e) => e -> m a
throwMissingImport e = throwM (MissingImports [(toException e)])


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

canonicalizeAll :: NonEmpty Import -> NonEmpty Import
canonicalizeAll = NonEmpty.scanr1 step
  where
    step a parent = canonicalizeImport (a :| [parent])

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

    canonicalize (URL scheme authority file query fragment header) =
        URL scheme authority (canonicalize file) query fragment header

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

canonicalizeImport :: NonEmpty Import -> Import
canonicalizeImport imports =
    canonicalize (sconcat (NonEmpty.reverse imports))

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
                else throwMissingImport (MissingFile path)

            text <- Data.Text.IO.readFile path

            return (path, text)

        URL scheme authority file query fragment maybeHeaders -> do
            let prefix =
                        (case scheme of HTTP -> "http"; HTTPS -> "https")
                    <>  "://"
                    <>  authority

            let fileText = Dhall.Pretty.Internal.prettyToStrictText file

            let suffix =
                        (case query    of Nothing -> ""; Just q -> "?" <> q)
                    <>  (case fragment of Nothing -> ""; Just f -> "#" <> f)
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
                Nothing     -> throwMissingImport (MissingEnvironmentVariable env)

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
                Right expr -> do
                    return expr

        RawText -> do
            return (TextLit (Chunks [] text))

-- | Resolve all imports within an expression using a custom typing
-- context and `Import`-resolving callback in arbitrary `MonadCatch`
-- monad.
--
-- This resolves imports relative to @.@ (the current working directory).
loadWith
    :: MonadCatch m
    => (Import -> StateT Status m (Expr Src Import))
    -> Dhall.Context.Context (Expr Src X)
    -> Dhall.Core.Normalizer X
    -> Expr Src Import
    -> m (Expr Src X)
loadWith from_import ctx n expr =
    loadDirWith "." from_import ctx n expr

-- | Resolve all imports within an expression using a custom typing
-- context and `Import`-resolving callback in arbitrary `MonadCatch`
-- monad, relative to a given directory.
--
-- @since 1.16
loadDirWith
    :: MonadCatch m
    => FilePath
    -> (Import -> StateT Status m (Expr Src Import))
    -> Dhall.Context.Context (Expr Src X)
    -> Dhall.Core.Normalizer X
    -> Expr Src Import
    -> m (Expr Src X)
loadDirWith dir from_import ctx n expr = do
    State.evalStateT (loadStaticWith from_import ctx n expr) (emptyStatus dir)

-- | Resolve all imports within an expression, relative to @.@ (the
-- current working directory), using a custom typing context.
--
-- @load = loadWithContext Dhall.Context.empty@
loadWithContext
    :: Dhall.Context.Context (Expr Src X)
    -> Dhall.Core.Normalizer X
    -> Expr Src Import
    -> IO (Expr Src X)
loadWithContext ctx n expr =
    loadDirWith "." exprFromImport ctx n expr


-- | This loads a \"static\" expression (i.e. an expression free of imports)
loadStaticWith
    :: MonadCatch m
    => (Import -> StateT Status m (Expr Src Import))
    -> Dhall.Context.Context (Expr Src X)
    -> Dhall.Core.Normalizer X
    -> Expr Src Import
    -> StateT Status m (Expr Src X)
loadStaticWith from_import ctx n expr₀ = case expr₀ of
  Embed import_ -> do
    imports <- zoom stack State.get

    let local (Import (ImportHashed _ (URL     {})) _) = False
        local (Import (ImportHashed _ (Local   {})) _) = True
        local (Import (ImportHashed _ (Env     {})) _) = True
        local (Import (ImportHashed _ (Missing {})) _) = True

    let parent   = canonicalizeImport imports
    let imports' = NonEmpty.cons import_ imports
    let here     = canonicalizeImport imports'

    if local here && not (local parent)
        then throwMissingImport (Imported imports (ReferentiallyOpaque import_))
        else return ()

    expr <- if here `elem` canonicalizeAll imports
        then throwMissingImport (Imported imports (Cycle import_))
        else do
            m <- zoom cache State.get
            case Map.lookup here m of
                Just expr -> return expr
                Nothing   -> do
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
                            -> StateT Status m (Expr Src Import)
                        handler₀ e@(MissingImports []) = throwM e
                        handler₀ (MissingImports [e]) =
                          throwMissingImport (Imported imports' e)
                        handler₀ (MissingImports es) = throwM
                          (MissingImports
                           (fmap
                             (\e -> (toException (Imported imports' e)))
                             es))
                        handler₁
                            :: (MonadCatch m)
                            => SomeException
                            -> StateT Status m (Expr Src Import)
                        handler₁ e =
                          throwMissingImport (Imported imports' e)

                    -- This loads a \"dynamic\" expression (i.e. an expression
                    -- that might still contain imports)
                    let loadDynamic =
                            from_import here

                    expr' <- loadDynamic `catches` [ Handler handler₀, Handler handler₁ ]

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
                        Left  err -> throwM (Imported imports' err)
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
                else throwMissingImport (Imported imports' (HashMismatch {..}))

    return expr
  ImportAlt a b -> loop a `catch` handler₀
    where
      handler₀ (MissingImports es₀) =
        loop b `catch` handler₁
        where
          handler₁ (MissingImports es₁) =
            throwM (MissingImports (es₀ ++ es₁))
  Const a              -> pure (Const a)
  Var a                -> pure (Var a)
  Lam a b c            -> Lam <$> pure a <*> loop b <*> loop c
  Pi a b c             -> Pi <$> pure a <*> loop b <*> loop c
  App a b              -> App <$> loop a <*> loop b
  Let a b c d          -> Let <$> pure a <*> mapM loop b <*> loop c <*> loop d
  Annot a b            -> Annot <$> loop a <*> loop b
  Bool                 -> pure Bool
  BoolLit a            -> pure (BoolLit a)
  BoolAnd a b          -> BoolAnd <$> loop a <*> loop b
  BoolOr a b           -> BoolOr <$> loop a <*> loop b
  BoolEQ a b           -> BoolEQ <$> loop a <*> loop b
  BoolNE a b           -> BoolNE <$> loop a <*> loop b
  BoolIf a b c         -> BoolIf <$> loop a <*> loop b <*> loop c
  Natural              -> pure Natural
  NaturalLit a         -> pure (NaturalLit a)
  NaturalFold          -> pure NaturalFold
  NaturalBuild         -> pure NaturalBuild
  NaturalIsZero        -> pure NaturalIsZero
  NaturalEven          -> pure NaturalEven
  NaturalOdd           -> pure NaturalOdd
  NaturalToInteger     -> pure NaturalToInteger
  NaturalShow          -> pure NaturalShow
  NaturalPlus a b      -> NaturalPlus <$> loop a <*> loop b
  NaturalTimes a b     -> NaturalTimes <$> loop a <*> loop b
  Integer              -> pure Integer
  IntegerLit a         -> pure (IntegerLit a)
  IntegerShow          -> pure IntegerShow
  IntegerToDouble      -> pure IntegerToDouble
  Double               -> pure Double
  DoubleLit a          -> pure (DoubleLit a)
  DoubleShow           -> pure DoubleShow
  Text                 -> pure Text
  TextLit (Chunks a b) -> fmap TextLit (Chunks <$> mapM (mapM loop) a <*> pure b)
  TextAppend a b       -> TextAppend <$> loop a <*> loop b
  List                 -> pure List
  ListLit a b          -> ListLit <$> mapM loop a <*> mapM loop b
  ListAppend a b       -> ListAppend <$> loop a <*> loop b
  ListBuild            -> pure ListBuild
  ListFold             -> pure ListFold
  ListLength           -> pure ListLength
  ListHead             -> pure ListHead
  ListLast             -> pure ListLast
  ListIndexed          -> pure ListIndexed
  ListReverse          -> pure ListReverse
  Optional             -> pure Optional
  OptionalLit a b      -> OptionalLit <$> loop a <*> mapM loop b
  OptionalFold         -> pure OptionalFold
  OptionalBuild        -> pure OptionalBuild
  Record a             -> Record <$> mapM loop a
  RecordLit a          -> RecordLit <$> mapM loop a
  Union a              -> Union <$> mapM loop a
  UnionLit a b c       -> UnionLit <$> pure a <*> loop b <*> mapM loop c
  Combine a b          -> Combine <$> loop a <*> loop b
  CombineTypes a b     -> CombineTypes <$> loop a <*> loop b
  Prefer a b           -> Prefer <$> loop a <*> loop b
  Merge a b c          -> Merge <$> loop a <*> loop b <*> mapM loop c
  Constructors a       -> Constructors <$> loop a
  Field a b            -> Field <$> loop a <*> pure b
  Project a b          -> Project <$> loop a <*> pure b
  Note a b             -> Note <$> pure a <*> loop b
  where
    loop = loadStaticWith from_import ctx n


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

{-| Convert a function applied to multiple arguments to the base function and
    the list of arguments
-}
unApply :: Expr s a -> (Expr s a, [Expr s a])
unApply e = (baseFunction₀, diffArguments₀ [])
  where
    ~(baseFunction₀, diffArguments₀) = go e

    go (App f a) = (baseFunction, diffArguments . (a :))
      where
        ~(baseFunction, diffArguments) = go f
    go baseFunction = (baseFunction, id)

expressionToTerm :: Expr s Import -> Term
expressionToTerm (Var (V "_" n)) =
    TInteger n
expressionToTerm (Var (V x 0)) =
    TString x
expressionToTerm (Var (V x n)) =
    TList [ TString x, TInteger n ]
expressionToTerm NaturalBuild =
    TString "Natural/build"
expressionToTerm NaturalFold =
    TString "Natural/fold"
expressionToTerm NaturalIsZero =
    TString "Natural/isZero"
expressionToTerm NaturalEven =
    TString "Natural/even"
expressionToTerm NaturalOdd =
    TString "Natural/odd"
expressionToTerm NaturalToInteger =
    TString "Natural/toInteger"
expressionToTerm NaturalShow =
    TString "Natural/show"
expressionToTerm IntegerToDouble =
    TString "Integer/toDouble"
expressionToTerm IntegerShow =
    TString "Integer/show"
expressionToTerm DoubleShow =
    TString "Double/show"
expressionToTerm ListBuild =
    TString "List/build"
expressionToTerm ListFold =
    TString "List/fold"
expressionToTerm ListLength =
    TString "List/length"
expressionToTerm ListHead =
    TString "List/head"
expressionToTerm ListLast =
    TString "List/last"
expressionToTerm ListIndexed =
    TString "List/indexed"
expressionToTerm ListReverse =
    TString "List/reverse"
expressionToTerm OptionalFold =
    TString "Optional/fold"
expressionToTerm OptionalBuild =
    TString "Optional/build"
expressionToTerm Bool =
    TString "Bool"
expressionToTerm Optional =
    TString "Optional"
expressionToTerm Natural =
    TString "Natural"
expressionToTerm Integer =
    TString "Integer"
expressionToTerm Double =
    TString "Double"
expressionToTerm Text =
    TString "Text"
expressionToTerm List =
    TString "List"
expressionToTerm (Const Type) =
    TString "Type"
expressionToTerm (Const Kind) =
    TString "Kind"
expressionToTerm e@(App _ _) =
    TList ([ TInt 0, f₁ ] ++ map expressionToTerm arguments)
  where
    (f₀, arguments) = unApply e

    f₁ = expressionToTerm f₀
expressionToTerm (Lam "_" _A₀ b₀) =
    TList [ TInt 1, _A₁, b₁ ]
  where
    _A₁ = expressionToTerm _A₀
    b₁  = expressionToTerm b₀
expressionToTerm (Lam x _A₀ b₀) =
    TList [ TInt 1, TString x, _A₁, b₁ ]
  where
    _A₁ = expressionToTerm _A₀
    b₁  = expressionToTerm b₀
expressionToTerm (Pi "_" _A₀ _B₀) =
    TList [ TInt 2, _A₁, _B₁ ]
  where
    _A₁ = expressionToTerm _A₀
    _B₁ = expressionToTerm _B₀
expressionToTerm (Pi x _A₀ _B₀) =
    TList [ TInt 2, TString x, _A₁, _B₁ ]
  where
    _A₁ = expressionToTerm _A₀
    _B₁ = expressionToTerm _B₀
expressionToTerm (BoolOr l₀ r₀) =
    TList [ TInt 3, TInt 0, l₁, r₁ ]
  where
    l₁ = expressionToTerm l₀
    r₁ = expressionToTerm r₀
expressionToTerm (BoolAnd l₀ r₀) =
    TList [ TInt 3, TInt 1, l₁, r₁ ]
  where
    l₁ = expressionToTerm l₀
    r₁ = expressionToTerm r₀
expressionToTerm (BoolEQ l₀ r₀) =
    TList [ TInt 3, TInt 2, l₁, r₁ ]
  where
    l₁ = expressionToTerm l₀
    r₁ = expressionToTerm r₀
expressionToTerm (BoolNE l₀ r₀) =
    TList [ TInt 3, TInt 3, l₁, r₁ ]
  where
    l₁ = expressionToTerm l₀
    r₁ = expressionToTerm r₀
expressionToTerm (NaturalPlus l₀ r₀) =
    TList [ TInt 3, TInt 4, l₁, r₁ ]
  where
    l₁ = expressionToTerm l₀
    r₁ = expressionToTerm r₀
expressionToTerm (NaturalTimes l₀ r₀) =
    TList [ TInt 3, TInt 5, l₁, r₁ ]
  where
    l₁ = expressionToTerm l₀
    r₁ = expressionToTerm r₀
expressionToTerm (TextAppend l₀ r₀) =
    TList [ TInt 3, TInt 6, l₁, r₁ ]
  where
    l₁ = expressionToTerm l₀
    r₁ = expressionToTerm r₀
expressionToTerm (ListAppend l₀ r₀) =
    TList [ TInt 3, TInt 7, l₁, r₁ ]
  where
    l₁ = expressionToTerm l₀
    r₁ = expressionToTerm r₀
expressionToTerm (Combine l₀ r₀) =
    TList [ TInt 3, TInt 8, l₁, r₁ ]
  where
    l₁ = expressionToTerm l₀
    r₁ = expressionToTerm r₀
expressionToTerm (Prefer l₀ r₀) =
    TList [ TInt 3, TInt 9, l₁, r₁ ]
  where
    l₁ = expressionToTerm l₀
    r₁ = expressionToTerm r₀
expressionToTerm (CombineTypes l₀ r₀) =
    TList [ TInt 3, TInt 10, l₁, r₁ ]
  where
    l₁ = expressionToTerm l₀
    r₁ = expressionToTerm r₀
expressionToTerm (ImportAlt l₀ r₀) =
    TList [ TInt 3, TInt 11, l₁, r₁ ]
  where
    l₁ = expressionToTerm l₀
    r₁ = expressionToTerm r₀
expressionToTerm (ListLit _T₀ xs₀)
    | null xs₀  = TList [ TInt 4, _T₁ ]
    | otherwise = TList ([ TInt 4, TNull ] ++ xs₁)
  where
    _T₁ = case _T₀ of
        Nothing -> TNull
        Just t  -> expressionToTerm t

    xs₁ = map expressionToTerm (Data.Foldable.toList xs₀)
expressionToTerm (OptionalLit _T₀ Nothing) =
    TList [ TInt 5, _T₁ ]
  where
    _T₁ = expressionToTerm _T₀
expressionToTerm (OptionalLit _T₀ (Just t₀)) =
    TList [ TInt 5, _T₁, t₁ ]
  where
    _T₁ = expressionToTerm _T₀
    t₁  = expressionToTerm t₀
expressionToTerm (Merge t₀ u₀ Nothing) =
    TList [ TInt 6, t₁, u₁ ]
  where
    t₁ = expressionToTerm t₀
    u₁ = expressionToTerm u₀
expressionToTerm (Merge t₀ u₀ (Just _T₀)) =
    TList [ TInt 6, t₁, u₁, _T₁ ]
  where
    t₁  = expressionToTerm t₀
    u₁  = expressionToTerm u₀
    _T₁ = expressionToTerm _T₀
expressionToTerm (Record xTs₀) =
    TList [ TInt 7, TMap xTs₁ ]
  where
    xTs₁ = do
        (x₀, _T₀) <- Data.HashMap.Strict.InsOrd.toList xTs₀
        let x₁  = TString x₀
        let _T₁ = expressionToTerm _T₀
        return (x₁, _T₁) 
expressionToTerm (RecordLit xts₀) =
    TList [ TInt 8, TMap xts₁ ]
  where
    xts₁ = do
        (x₀, t₀) <- Data.HashMap.Strict.InsOrd.toList xts₀
        let x₁ = TString x₀
        let t₁ = expressionToTerm t₀
        return (x₁, t₁)
expressionToTerm (Field t₀ x) =
    TList [ TInt 9, t₁, TString x ]
  where
    t₁ = expressionToTerm t₀
expressionToTerm (Project t₀ xs₀) =
    TList ([ TInt 10, t₁ ] ++ xs₁)
  where
    t₁  = expressionToTerm t₀
    xs₁ = map TString (Data.Foldable.toList xs₀)
expressionToTerm (Union xTs₀) =
    TList [ TInt 11, TMap xTs₁ ]
  where
    xTs₁ = do
        (x₀, _T₀) <- Data.HashMap.Strict.InsOrd.toList xTs₀
        let x₁  = TString x₀
        let _T₁ = expressionToTerm _T₀
        return (x₁, _T₁)
expressionToTerm (UnionLit x t₀ yTs₀) =
    TList [ TInt 12, TString x, t₁, TMap yTs₁ ]
  where
    t₁ = expressionToTerm t₀

    yTs₁ = do
        (y₀, _T₀) <- Data.HashMap.Strict.InsOrd.toList yTs₀
        let y₁  = TString y₀
        let _T₁ = expressionToTerm _T₀
        return (y₁, _T₁)
expressionToTerm (Constructors u₀) =
    TList [ TInt 13, u₁ ]
  where
    u₁ = expressionToTerm u₀
expressionToTerm (BoolLit b) =
    TBool b
expressionToTerm (BoolIf t₀ l₀ r₀) =
    TList [ TInt 14, t₁, l₁, r₁ ]
  where
    t₁ = expressionToTerm t₀
    l₁ = expressionToTerm l₀
    r₁ = expressionToTerm r₀
expressionToTerm (NaturalLit n) =
    TList [ TInt 15, TInteger (fromIntegral n) ]
expressionToTerm (IntegerLit n) =
    TList [ TInt 16, TInteger n ]
expressionToTerm (DoubleLit n) =
    TList [ TInt 17, TTagged 4 (TList [ TInt exponent, TInteger mantissa ]) ]
  where
    normalized = Data.Scientific.normalize n

    exponent = Data.Scientific.base10Exponent normalized

    mantissa = Data.Scientific.coefficient normalized
expressionToTerm (TextLit (Chunks xys₀ z₀)) =
    TList ([ TInt 18 ] ++ xys₁ ++ [ z₁ ])
  where
    xys₁ = do
        (x₀, y₀) <- xys₀
        let x₁ = TString x₀
        let y₁ = expressionToTerm y₀
        [ x₁, y₁ ]

    z₁ = TString z₀
expressionToTerm (Embed x) =
    importToTerm x
expressionToTerm (Let x Nothing a₀ b₀) =
    TList [ TInt 25, TString x, a₁, b₁ ]
  where
    a₁ = expressionToTerm a₀
    b₁ = expressionToTerm b₀
expressionToTerm (Let x (Just _A₀) a₀ b₀) =
    TList [ TInt 25, TString x, _A₁, a₁, b₁ ]
  where
    a₁  = expressionToTerm a₀
    _A₁ = expressionToTerm _A₀
    b₁  = expressionToTerm b₀
expressionToTerm (Annot t₀ _T₀) =
    TList [ TInt 26, t₁, _T₁ ]
  where
    t₁  = expressionToTerm t₀
    _T₁ = expressionToTerm _T₀
expressionToTerm (Note _ e) =
    expressionToTerm e

importToTerm :: Import -> Term
importToTerm import_ =
    case importType of
        URL scheme₀ authority path query fragment _ ->
            TList
                (   [ TInt 24, TInt scheme₁, TString authority ]
                ++  map TString (reverse components)
                ++  [ TString file ]
                ++  (case query    of Nothing -> [ TNull ]; Just q -> [ TString q ])
                ++  (case fragment of Nothing -> [ TNull ]; Just f -> [ TString f ])
                )
          where
            scheme₁ = case scheme₀ of
                HTTP  -> 0
                HTTPS -> 1
            File {..} = path

            Directory {..} = directory

        Local prefix₀ path ->
                TList
                    (   [ TInt 24, TInt prefix₁ ]
                    ++  map TString components₁
                    ++  [ TString file ]
                    )
          where
            File {..} = path

            Directory {..} = directory

            (prefix₁, components₁) = case (prefix₀, reverse components) of
                (Absolute, rest       ) -> (2, rest)
                (Here    , ".." : rest) -> (4, rest)
                (Here    , rest       ) -> (3, rest)
                (Home    , rest       ) -> (5, rest)

        Env x ->
            TList [ TInt 24, TInt 6, TString x ]

        Missing ->
            TString "missing"
  where
    Import {..} = import_

    ImportHashed {..} = importHashed
