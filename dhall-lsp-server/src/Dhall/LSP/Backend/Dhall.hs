smodule Dhall.LSP.Backend.Dhall (
  FileIdentifier,
  fileIdentifierFromFilePath,
  fileIdentifierFromURI,
  WellTyped,
  fromWellTyped,
  Normal,
  fromNormal,
  Cache,
  emptyCache,
  invalidate,
  DhallError(..),
  parse,
  parseWithHeader,
  load,
  typecheck,
  normalize
 ) where

import Dhall.Parser (Src)
import Dhall.TypeCheck (X)
import Dhall.Core (Expr)

import qualified Dhall.Core as Dhall
import qualified Dhall.Import as Dhall
import qualified Dhall.Parser.Token as Dhall
import qualified Dhall.Parser as Dhall
import qualified Dhall.TypeCheck as Dhall

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Network.URI as URI
import qualified Language.Haskell.LSP.Types as LSP.Types
import qualified Data.Text as Text
import qualified Text.Megaparsec as Megaparsec

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import System.FilePath (splitDirectories, takeFileName, takeDirectory)
import Lens.Family (view, set)
import Control.Exception (SomeException, catch)
import Control.Monad.Trans.State.Strict (runStateT)
import Network.URI (URI)
import Data.Bifunctor (first)

-- | A @FileIdentifier@ represents either a local file or a remote url.
newtype FileIdentifier = FileIdentifier Dhall.ImportType

-- | Construct a FileIdentifier from a local file path.
fileIdentifierFromFilePath :: FilePath -> FileIdentifier
fileIdentifierFromFilePath path =
  let filename = Text.pack $ takeFileName path
      directory = takeDirectory path
      components = map Text.pack . reverse . splitDirectories $ directory
  in FileIdentifier $ Dhall.Local Dhall.Absolute
                        (Dhall.File (Dhall.Directory components) filename)

-- | Construct a FileIdentifier from a given URI. Supports "file:", "http:" and
--   "https:" URI schemes.
fileIdentifierFromURI :: URI -> Maybe FileIdentifier
fileIdentifierFromURI uri
  | URI.uriScheme uri == "file:" = do
    path <- LSP.Types.uriToFilePath . LSP.Types.Uri . Text.pack
                  $ URI.uriToString id uri ""
    return $ fileIdentifierFromFilePath path
fileIdentifierFromURI uri
  | otherwise = do
    url <- Megaparsec.parseMaybe (Dhall.unParser Dhall.httpRaw) . Text.pack
             $ URI.uriToString id uri ""
    return $ FileIdentifier (Dhall.Remote url)

-- | A well-typed expression.
newtype WellTyped = WellTyped {fromWellTyped :: Expr Src X}

-- | A fully normalised expression.
newtype Normal = Normal {fromNormal :: Expr Src X}

-- An import graph, represented by an adjacency list. An element `(a,b)` means
-- that `b` imports `a`.
type ImportGraph = [(Dhall.Import, Dhall.Import)]

-- | A cache maps Dhall imports to fully normalised expressions. By reusing
--   caches we can speeds up diagnostics etc. significantly!
data Cache = Cache ImportGraph (Map.Map Dhall.Import (Expr Src X))

-- | The initial cache.
emptyCache :: Cache
emptyCache = Cache [] Map.empty

-- | Cache a given normal expression.
cacheExpr :: FileIdentifier -> Normal -> Cache -> Cache
cacheExpr fileid (Normal expr) (Cache graph c) =
  let unhashedImport = importFromFileIdentifier fileid
      alpha = Dhall.alphaNormalize expr  -- we need to alpha-normalise before
      hash = Dhall.hashExpression maxBound alpha  -- calculating the hash
      hashedImport = hashedImportFromFileIdentifier fileid hash
  in Cache graph $ Map.insert unhashedImport expr
                 $ Map.insert hashedImport expr c

-- Construct the unhashed import corresponding to the given file.
importFromFileIdentifier :: FileIdentifier -> Dhall.Import
importFromFileIdentifier (FileIdentifier importType) =
  Dhall.Import { importHashed = Dhall.ImportHashed Nothing importType,
                 importMode = Dhall.Code }


-- | Invalidate any _unhashed_ imports of the given file. Hashed imports are
--   kept around as per
--   https://github.com/dhall-lang/dhall-lang/blob/master/standard/imports.md.
--   Transitively invalidates any imports depending on the changed file.
invalidate :: FileIdentifier -> Cache -> Cache
invalidate (FileIdentifier fileid) (Cache edgeList cache) =
  Cache (view Dhall.graph status') (view Dhall.cache status')
  where
    status' = Dhall.invalidate invalidImports status
    status = set Dhall.cache cache . set Dhall.graph edgeList $ Dhall.emptyStatus ""
    codeImport = Dhall.Import (Dhall.ImportHashed Nothing fileid) Dhall.Code
    textImport = Dhall.Import (Dhall.ImportHashed Nothing fileid) Dhall.RawText
    invalidImports = codeImport : Dhall.reverseDependencies codeImport status
                     ++ textImport : Dhall.reverseDependencies textImport status

-- | A Dhall error. Covers parsing, resolving of imports, typechecking and
--   normalisation.
data DhallError = ErrorInternal SomeException
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
load fileid expr (Cache graph cache) = do
  let emptyStatus = Dhall.emptyStatus ""
      status = -- reuse cache and import graph
               set Dhall.cache cache .
               set Dhall.graph graph .
               -- set "root import"
               set Dhall.stack (importFromFileIdentifier fileid :| [])
                 $ emptyStatus
  (do (expr', status') <- runStateT (Dhall.loadWith expr) status
      let cache' = view Dhall.cache status'
          graph' = view Dhall.graph status'
      return . Right $ (Cache graph' cache', expr'))
    `catch` (\e -> return . Left $ ErrorImportSourced e)
    `catch` (\e -> return . Left $ ErrorInternal e)

-- | Typecheck a fully resolved expression. Returns a certification that the
--   input was well-typed along with its (well-typed) type.
typecheck :: Expr Src X -> Either DhallError (WellTyped, WellTyped)
typecheck expr = case Dhall.typeOf expr of
  Left err -> Left $ ErrorTypecheck err
  Right typ -> Right (WellTyped expr, WellTyped typ)

-- | Normalise a well-typed expression.
normalize :: WellTyped -> Normal
normalize (WellTyped expr) = Normal $ Dhall.normalize expr
