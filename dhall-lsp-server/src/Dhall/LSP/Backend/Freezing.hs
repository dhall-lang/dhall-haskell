module Dhall.LSP.Backend.Freezing (
  computeSemanticHash,
  getAllImportsWithHashPositions,
  getImportHashPosition,
  stripHash
) where

import Control.Lens                  (universeOf, view)
import Data.Text                     (Text)
import Data.Void                     (Void)
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
    , asWellTyped
    , cachedSemantics
    , emptyCache
    , hashNormalToCode
    , identifierChained
    , load
    , lookupCachedImport
    , normalize
    , sharedStatus
    )

import qualified Dhall
import qualified Dhall.Core   as Core
import qualified Dhall.Import as Import
import qualified Dhall.Map    as Map
import Dhall.LSP.Backend.Diagnostics
    ( Range (..)
    , positionFromMegaparsec
    , positionToOffset
    , rangeFromDhall
    , subtractPosition
    )
import Dhall.LSP.Backend.Parsing     (getImportHash)
import Dhall.LSP.Backend.Prefix      (singleCodeImport)
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
    cachedHash <- hashCachedSingleImport settings fileid cache expr
    case cachedHash of
        Just hash ->
            return (Right (cache, hash))
        Nothing -> do
            loaded <- load settings fileid expr cache
            case loaded of
                Left err ->
                    return (Left err)
                Right (cache', expr') ->
                    return
                        (Right
                            ( cache'
                            , hashNormalToCode (normalize settings cache' (asWellTyped expr'))
                            ))

-- | When the file is only a code import, reuse cached import semantics.
hashCachedSingleImport
    :: EvaluateSettings
    -> FileIdentifier
    -> Cache
    -> Expr Src Import
    -> IO (Maybe Text)
hashCachedSingleImport settings fileid cache expr =
    case singleCodeImport expr of
        Nothing ->
            return Nothing
        Just imp ->
            case lookupCachedImport fileid cache imp of
                Nothing ->
                    return Nothing
                Just (_, sem) ->
                    case sharedStatus cache of
                        Nothing ->
                            return (Just (hashVoidSemantics settings sem))
                        Just status ->
                            hashShared status imp sem
  where
    hashShared status imp sem =
        let chained =
                Import.Chained
                    (Import.chainedImport (identifierChained fileid) <> imp)
        in case Map.lookup chained (cachedSemantics cache) of
            Nothing ->
                return (Just (hashVoidSemantics settings sem))
            Just semantics -> do
                normal <-
                    Import.alphaBetaNormalForIntegrityHash
                        status
                        (view Dhall.normalizer settings)
                        imp
                        semantics
                return (Just (Import.hashExpressionToCode normal))

hashVoidSemantics :: EvaluateSettings -> Expr Void Void -> Text
hashVoidSemantics settings sem =
    hashNormalToCode (normalize settings emptyCache (asWellTyped (Core.renote sem)))

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
