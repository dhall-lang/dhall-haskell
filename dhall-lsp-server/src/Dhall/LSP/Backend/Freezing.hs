module Dhall.LSP.Backend.Freezing (
  computeSemanticHash,
  getAllImportsWithHashPositions,
  getImportHashPosition,
  stripHash
) where

import Control.Lens                  (universeOf)
import Data.Text                     (Text)
import Dhall                         (EvaluateSettings)
import Dhall.Core
    ( Expr (..)
    , Import (..)
    , ImportHashed (..)
    , subExpressions
    )
import Dhall.LSP.Backend.Dhall
    ( Cache
    , DhallError
    , FileIdentifier
    , hashNormalToCode
    , load
    , normalize
    , typecheck
    )
import Dhall.LSP.Backend.Diagnostics
    ( Range (..)
    , positionFromMegaparsec
    , positionToOffset
    , rangeFromDhall
    , subtractPosition
    )
import Dhall.LSP.Backend.Parsing     (getImportHash)
import Dhall.Parser                  (Src (..))


import qualified Data.Text as Text

-- | Given an expression (potentially still containing imports) compute its
-- 'semantic' hash in the textual representation used to freeze Dhall imports.
computeSemanticHash
    :: EvaluateSettings
    -> FileIdentifier
    -> Expr Src Import
    -> Cache
    -> IO (Either DhallError (Cache, Text))
computeSemanticHash settings fileid expr cache = do
  loaded <- load settings fileid expr cache
  case loaded of
    Left err -> return (Left err)
    Right (cache', expr') -> case typecheck settings expr' of
      Left err -> return (Left err)
      Right (wt,_) ->
        return (Right (cache', hashNormalToCode (normalize settings wt)))

stripHash :: Import -> Import
stripHash (Import (ImportHashed _ importType) mode) =
  Import (ImportHashed Nothing importType) mode

getImportHashPosition :: Src -> Maybe Range
getImportHashPosition src@(Src left _ text)  = do
  Src left' right' _ <- getImportHash src
  let p0 = positionFromMegaparsec left

  -- sanitise the starting point
  let p1 = positionFromMegaparsec left'
      off1 = positionToOffset text (subtractPosition p0 p1)
      Range _ left'' = rangeFromDhall (Src left left' (Text.take off1 text))

  -- sanitise the end point
  let Range _ right'' = rangeFromDhall (Src left right' text)

  return (Range left'' right'')

getAllImportsWithHashPositions :: Expr Src Import -> [(Import, Range)]
getAllImportsWithHashPositions expr =
  [ (i, range) |
    Note src (Embed i) <- universeOf subExpressions expr,
    Just range <- [getImportHashPosition src] ]
