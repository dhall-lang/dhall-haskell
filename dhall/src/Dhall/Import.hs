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
    -- * Hashing
      hashExpressionToCode

    -- * Resolution
    , rawFromImport
    , resolve
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Applicative (Alternative(..))
import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO(..))
import Data.CaseInsensitive (CI)
import Data.Semigroup (Semigroup(..))
import {-# SOURCE #-} Dhall.Elaboration
import Dhall.Parser (Parser(..), ParseError(..))
import Dhall.Errors
import Dhall.Binary (StandardVersion(..))
import Crypto.Hash (SHA256, Digest)
import Data.Text (Text)
import Codec.CBOR.Term (Term(..))

#if MIN_VERSION_base(4,8,0)
#else
import Data.Traversable (traverse)
#endif
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

import qualified Data.ByteString.Lazy
import qualified Codec.Serialise
import qualified Data.ByteString
import qualified Data.CaseInsensitive
import qualified Data.Foldable
import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Map.Strict                  as Map
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding
import qualified Data.Text.IO
import qualified Dhall.Map
import qualified Dhall.Parser
import qualified Dhall.Binary
import qualified System.Directory                 as Directory
import qualified System.Environment
import qualified System.FilePath                  as FilePath
import qualified Text.Megaparsec
import qualified Text.Parser.Combinators
import qualified Text.Parser.Token
import qualified Crypto.Hash


-- Canonicalization
--------------------------------------------------------------------------------

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

-- Hashing & encoding
--------------------------------------------------------------------------------

encodeExpression :: StandardVersion -> Nf -> Data.ByteString.ByteString
encodeExpression _standardVersion expression = bytesStrict
  where
    term :: Term
    term = Dhall.Binary.encode expression

    taggedTerm :: Term
    taggedTerm = case _standardVersion of
      NoVersion -> term
      s         -> TList [ TString (Dhall.Binary.renderStandardVersion s), term ]

    bytesLazy   = Codec.Serialise.serialise taggedTerm
    bytesStrict = Data.ByteString.Lazy.toStrict bytesLazy


-- | Hash an expression in normal form.
hashExpression :: StandardVersion -> Nf -> Crypto.Hash.Digest SHA256
hashExpression _standardVersion expression =
    Crypto.Hash.hash (encodeExpression _standardVersion expression)

{-| Convenience utility to hash a fully resolved expression and return the
    base-16 encoded hash with the @sha256:@ prefix
    In other words, the output of this function can be pasted into Dhall
    source code to add an integrity check to an import
-}
hashExpressionToCode :: StandardVersion -> Nf -> Text
hashExpressionToCode _standardVersion expr =
    "sha256:" <> Text.pack (show (hashExpression _standardVersion expr))


-- Resolution
--------------------------------------------------------------------------------

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
    let File {..}      = file_
    let Directory {..} = directory

    prefixPath <- case prefix of
        Home -> do
            Directory.getHomeDirectory
        Absolute -> do
            pure "/"
        Parent -> do
            pwd <- Directory.getCurrentDirectory
            return (FilePath.takeDirectory pwd)
        Here -> do
            Directory.getCurrentDirectory

    let cs = map Text.unpack (file : components)
    let cons component dir = dir </> component
    pure (foldr cons prefixPath cs)

rawFromImport :: Cxt -> Import -> ElabM Raw
rawFromImport cxt (Import {..}) = do
    let ImportHashed {..} = importHashed

    (path, text) <- case importType of
        Local prefix file -> do
            path   <- liftIO $ localToPath prefix file
            exists <- liftIO $ Directory.doesFileExist path
            unless exists $ importError cxt (MissingFile path)
            text <- liftIO $ Data.Text.IO.readFile path
            pure (path, text)

        Remote url@URL { headers = maybeHeaders } -> do
            mheaders <- forM maybeHeaders $ \_ -> do
              error "headers not yet supported"

#ifdef MIN_VERSION_http_client
            fetchFromHttpUrl url mheaders
#else
            let urlString = Text.unpack (Dhall.Core.pretty url)
            liftIO (throwIO (CannotImportHTTPURL urlString mheaders))
#endif

        Env env -> do
            x <- liftIO $ System.Environment.lookupEnv (Text.unpack env)
            case x of
                Just string -> pure (Text.unpack env, Text.pack string)
                Nothing     -> importError cxt (MissingEnvironmentVariable env)

        Missing -> do
          importError cxt MissingImport

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

-- | Resolve an import to an elaborated expression, its lazy value, its type and its
--   lazy hash.
resolve :: Cxt -> Import -> ElabM (Core, Val, VType, Digest SHA256)
resolve cxt import0 = do
  ImportState {..} <- ask

  let parent  = NonEmpty.head _stack
      import1 = parent <> import0
      child   = canonicalize import1

  let localImport (Import (ImportHashed _ ty) _) = case ty of
        Remote{} -> False
        _        -> True

  when (localImport child && not (localImport parent)) $
    importError cxt (ReferentiallyOpaque import0)

  when (child `elem` _stack) $
    importError cxt (Cycle import0)

  (Map.lookup child <$> liftIO (readIORef _cache)) >>= \case
    Just res -> pure res
    Nothing -> do
      t      <- rawFromImport cxt child
      (t, a) <- local (\s -> s {_stack = NonEmpty.cons child _stack})
                      (infer emptyCxt t)
      let ~tv  = eval Empty t
      let ~nf  = alphaNormalize $ quote NEmpty tv
      let ~hsh = hashExpression _standardVersion nf

      liftIO $ modifyIORef' _cache $ Map.insert child (t, tv, a, hsh)

      case hash (importHashed import0) of
        Nothing -> pure ()
        Just expectedHash -> do
          let matches version = expectedHash == hashExpression version nf
          unless (any matches [minBound .. maxBound]) $ do
            let actualHash = hashExpression NoVersion nf
            importError cxt HashMismatch{..}

      pure (t, tv, a, hsh)
