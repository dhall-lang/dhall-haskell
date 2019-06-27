module Dhall.LSP.Backend.Dhall {-(
  FileIdentifier,
  fileIdentifierFromURI,
  WellTyped,
  fromWellTyped,
  Normal,
  fromNormal,
  Cache,
  emptyCache,
  cacheExpr,
  invalidate,
  DhallError(..),
  parse,
  parseWithHeader,
  load,
  typecheck,
  normalize
 )-} where

import Dhall.Import (loadWith, emptyStatus)
import Dhall.Parser (Src, exprFromText)
import Dhall.TypeCheck (X)
import Dhall.Core (Expr)
import Dhall
  (rootDirectory, sourceName, defaultInputSettings, inputExprWithSettings)

import qualified Dhall.Binary as Dhall
import qualified Dhall.Core as Dhall
import qualified Dhall.Import as Dhall
import qualified Dhall.Parser.Token as Dhall
import qualified Dhall.Parser as Dhall
import qualified Dhall.TypeCheck as Dhall

import qualified Text.Dot as Dot
import qualified Data.Map.Strict as Map
import qualified Network.URI as URI
import qualified Language.Haskell.LSP.Types as LSP.Types
import qualified Data.Text as Text
import qualified Text.Megaparsec as Megaparsec
import Data.List.NonEmpty (NonEmpty((:|)))
import Crypto.Hash (Digest, SHA256)

import Data.Text (Text)
import System.FilePath (splitFileName, splitDirectories)
import Lens.Family (view, set)
import Control.Exception (handle, SomeException, catch)
import Control.Monad.Trans.State.Strict (evalStateT, runStateT)
import Network.URI (URI)
import Data.Bifunctor (first, second)

-- legacy, due to be removed
runDhall :: FilePath -> Text -> IO (Expr Src X)
runDhall path = inputExprWithSettings dhallparams
  where
    dhallparams = (set rootDirectory dir . set sourceName file)
                  defaultInputSettings
    (dir, file) = splitFileName path

-- legacy, due to be removed
runDhallSafe :: FilePath -> Text -> IO (Maybe (Expr Src X))
runDhallSafe path text = handle (\(_ :: SomeException) -> return Nothing)
                                (Just <$> runDhall path text)

-- legacy, due to be removed
loadDhallExprSafe :: FilePath -> Text -> IO (Maybe (Expr Src X))
loadDhallExprSafe filePath txt =
  case exprFromText filePath txt of
    Right expr ->
      let (dir, _) = splitFileName filePath
      in handle (\(_ :: SomeException) -> return Nothing)
                (Just <$> evalStateT (loadWith expr) (emptyStatus dir))
    Left _ -> return Nothing


-- new code
-- TODO: use record syntax for records!

-- | A @FileIdentifier@ represents either a local file or a remote url.
newtype FileIdentifier = FileIdentifier Dhall.ImportType

-- | Construct a FileIdentifier from a given URI. Supports "file:", "http:" and
--   "https:" URI schemes.
fileIdentifierFromURI :: URI -> Maybe FileIdentifier
fileIdentifierFromURI uri
  | URI.uriScheme uri == "file:" = do
    filePath <- LSP.Types.uriToFilePath . LSP.Types.Uri . Text.pack
                  $ URI.uriToString id uri ""
    (file : components) <- return . map Text.pack . reverse . splitDirectories
                             $ filePath
    return . FileIdentifier $ Dhall.Local Dhall.Absolute
               (Dhall.File (Dhall.Directory components) file)
fileIdentifierFromURI uri
  | otherwise = do
    url <- Megaparsec.parseMaybe (Dhall.unParser Dhall.httpRaw) . Text.pack
             $ URI.uriToString id uri ""
    return $ FileIdentifier (Dhall.Remote url)

-- | A well-typed expression.
newtype WellTyped = WellTyped {fromWellTyped :: Expr Src X}

-- | A fully normalised expression.
newtype Normal = Normal {fromNormal :: Expr Src X}

-- | A cache maps Dhall imports to fully normalised expressions. By reusing
--   caches we can speeds up diagnostics etc. significantly!
newtype Cache = Cache (Map.Map Dhall.Import (Dot.NodeId, Expr Src X))

-- | The initial cache.
emptyCache :: Cache
emptyCache = Cache Map.empty

-- | Cache a given normal expression.
cacheExpr :: FileIdentifier -> Normal -> Cache -> Cache
cacheExpr fileid (Normal expr) (Cache c) =
  let unhashedImport = importFromFileIdentifier fileid
      alpha = Dhall.alphaNormalize expr  -- we need to alpha-normalise before
      hash = Dhall.hashExpression maxBound alpha  -- calculating the hash
      hashedImport = hashedImportFromFileIdentifier fileid hash
  in Cache $ Map.insert unhashedImport (Dot.userNodeId 0, expr)
           $ Map.insert hashedImport (Dot.userNodeId 0, expr) c

-- Construct the unhashed import corresponding to the given file.
importFromFileIdentifier :: FileIdentifier -> Dhall.Import
importFromFileIdentifier (FileIdentifier importType) =
  Dhall.Import (Dhall.ImportHashed Nothing importType) Dhall.Code

-- Construct the hashed import corresponding to the given file.
hashedImportFromFileIdentifier :: FileIdentifier -> Digest SHA256 -> Dhall.Import
hashedImportFromFileIdentifier (FileIdentifier importType) hash =
  Dhall.Import (Dhall.ImportHashed (Just hash) importType) Dhall.Code

-- | Invalidate all imports of a file (unhashed, as well as any hashed
--   versions).
invalidate :: FileIdentifier -> Cache -> Cache
invalidate (FileIdentifier fileid) (Cache cache) =
  Cache $ Map.filterWithKey
    (\(Dhall.Import (Dhall.ImportHashed _ t) _) _ -> fileid /= t) cache

-- | A Dhall error. Covers parsing, resolving of imports, typechecking and
--   normalisation.
data DhallError = ErrorInternal SomeException
                | ErrorCBOR Dhall.DecodingFailure
                | ErrorImportSourced (Dhall.SourcedException Dhall.MissingImports)
                | ErrorTypecheck (Dhall.TypeError Src X)
                | ErrorParse Dhall.ParseError

-- | Parse a Dhall expression.
parse :: Text -> Either DhallError (Expr Src Dhall.Import)
parse = fmap snd . parseWithHeader

-- | Parse a Dhall expression along with its "header", i.e. whitespace and
--   comments prefixing the actual code.
parseWithHeader :: Text -> Either DhallError (Text, Expr Src Dhall.Import)
parseWithHeader = first ErrorParse . Dhall.exprAndHeaderFromText ""

-- | Resolve all imports in an expression.
load :: FileIdentifier -> Expr Src Dhall.Import -> Cache ->
  IO (Either DhallError (Cache, Expr Src X))
load fileid expr (Cache cache) = do
  let emptyStatus = Dhall.emptyStatus ""
      status = -- reuse cache
               set Dhall.cache cache .
               -- set "root import"
               set Dhall.stack (importFromFileIdentifier fileid :| [])
                 $ emptyStatus
  (do (expr', status') <- runStateT (Dhall.loadWith expr) status
      let cache' = Cache $ view Dhall.cache status'
      return . Right $ (cache', expr'))
    `catch` (\e -> return . Left $ ErrorImportSourced e)
    `catch` (\e -> return . Left $ ErrorInternal e)

-- | Typecheck a fully resolved expression.
typecheck :: Expr Src X -> Either DhallError WellTyped
typecheck = first ErrorTypecheck . second WellTyped . Dhall.typeOf

-- | Normalise a well-typed expression.
normalize :: WellTyped -> Normal
normalize (WellTyped expr) = Normal $ Dhall.normalize expr
