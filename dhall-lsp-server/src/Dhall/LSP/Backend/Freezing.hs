module Dhall.LSP.Backend.Freezing (
  computeSemanticHash,
  getAllImportsWithHashPositions,
  getImportHashPosition,
  stripHash
) where

import Dhall.Parser (Src(..))
import Dhall.Core (Expr(..), Import(..), ImportHashed(..), subExpressions)

import Control.Lens (universeOf)
import Data.Text (Text)
import qualified Data.Text as Text

import Dhall.LSP.Backend.Dhall (FileIdentifier, Cache, DhallError, typecheck,
  normalize, hashNormalToCode, load)
import Dhall.LSP.Backend.Diagnostics (Range(..), rangeFromDhall, positionFromMegaparsec, positionToOffset, relativePosition)
import Dhall.LSP.Backend.Parsing (getImportHash)

-- | Given an expression (potentially still containing imports) compute its
-- 'semantic' hash in the textual representation used to freeze Dhall imports.
computeSemanticHash :: FileIdentifier -> Expr Src Import -> Cache ->
  IO (Either DhallError (Cache, Text))
computeSemanticHash fileid expr cache = do
  loaded <- load fileid expr cache
  case loaded of
    Left err -> return (Left err)
    Right (cache', expr') -> case typecheck expr' of
      Left err -> return (Left err)
      Right (wt,_) -> do
        return (Right (cache', hashNormalToCode (normalize wt)))

stripHash :: Import -> Import
stripHash (Import (ImportHashed _ importType) mode) =
  Import (ImportHashed Nothing importType) mode

getImportHashPosition :: Src -> Maybe Range
getImportHashPosition src@(Src left _ text)  = do
  Src left' right' _ <- getImportHash src
  let (x0, y0) = positionFromMegaparsec left

  -- sanitise the starting point
  let (x1, y1) = positionFromMegaparsec left'
      off1 = positionToOffset text (relativePosition (x0, y0) (x1, y1))
      Range _ left'' = rangeFromDhall (Src left left' (Text.take off1 text))

  -- sanitise the end point
  let Range _ right'' = rangeFromDhall (Src left right' text)

  return (Range left'' right'')

getAllImportsWithHashPositions :: Expr Src Import -> [(Import, Range)]
getAllImportsWithHashPositions expr =
  [ (i, range) |
    Note src (Embed i) <- universeOf subExpressions expr,
    Just range <- [getImportHashPosition src] ]
