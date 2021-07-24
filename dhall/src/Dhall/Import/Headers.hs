{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Dhall.Import.Headers (
      HTTPHeader
    , SiteHeaders
    , toHeaders
    , toSiteHeaders
    , normalizeHeaders
    ) where

import Control.Applicative              (Alternative (..), liftA2)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.ByteString                  (ByteString)
import Data.CaseInsensitive             (CI)
import Data.Text                        (Text)
import Data.Void                        (Void, absurd)
import Control.Monad.Catch              (MonadCatch (catch), handle, throwM)

import Dhall.Syntax
    ( Chunks (..)
    , Expr (..)
    )

import Dhall.Parser
    ( ParseError (..)
    , Parser (..)
    , SourcedException (..)
    , Src (..)
    )

import Control.Exception
    ( Exception
    , IOException
    , SomeException
    , toException
    )

import qualified Data.CaseInsensitive
import qualified Data.Foldable
import qualified Data.Text.Encoding
import qualified Dhall.Core                                  as Core
import qualified Dhall.Map
import qualified Dhall.TypeCheck
import qualified Dhall.Pretty.Internal


-- | HTTP headers
type HTTPHeader = (CI ByteString, ByteString)

-- | A map of site origin -> HTTP headers
type SiteHeaders = HashMap Text [HTTPHeader]

-- | Given a well-typed (of type `List { header : Text, value Text }` or
-- `List { mapKey : Text, mapValue Text }`) headers expressions in normal form
-- construct the corresponding binary http headers; otherwise return the empty
-- list.
toHeaders :: Expr s a -> [HTTPHeader]
toHeaders (ListLit _ hs) = Data.Foldable.toList (Data.Foldable.fold maybeHeaders)
  where
      maybeHeaders = mapM toHeader hs
toHeaders _ = []

toHeader :: Expr s a -> Maybe HTTPHeader
toHeader (RecordLit m) = do
    (Core.recordFieldValue -> TextLit (Chunks [] keyText), Core.recordFieldValue -> TextLit (Chunks [] valueText))
        <- lookupHeader <|> lookupMapKey
    let keyBytes   = Data.Text.Encoding.encodeUtf8 keyText
    let valueBytes = Data.Text.Encoding.encodeUtf8 valueText
    return (Data.CaseInsensitive.mk keyBytes, valueBytes)
      where
        lookupHeader = liftA2 (,) (Dhall.Map.lookup "header" m) (Dhall.Map.lookup "value" m)
        lookupMapKey = liftA2 (,) (Dhall.Map.lookup "mapKey" m) (Dhall.Map.lookup "mapValue" m)
toHeader _ =
    empty

-- | Normalize, typecheck and return the SiteHeaders from a given expression.
toSiteHeaders :: Expr Src Void -> IO SiteHeaders
toSiteHeaders expr = fmap convert (normalizeSiteHeaders expr)
  where
    convert :: Expr s a -> SiteHeaders
    convert (ListLit _ hs) = HashMap.fromList (sitePairs hs)
    convert _ = mempty

    sitePairs hs = Data.Foldable.toList (Data.Foldable.fold (mapM toSitePair hs))

    toSitePair :: Expr s a -> Maybe (Text, [HTTPHeader])
    toSitePair (RecordLit m) = do
      (Core.recordFieldValue -> TextLit (Chunks [] keyText), Core.recordFieldValue -> value)
          <- lookupMapKey
      return (keyText, toHeaders value)
        where
          lookupMapKey = liftA2 (,) (Dhall.Map.lookup "mapKey" m) (Dhall.Map.lookup "mapValue" m)
    toSitePair _ = Nothing

makeHeadersTypeExpr :: Text -> Text -> Expr Src Void
makeHeadersTypeExpr keyKey valueKey =
  App List
      ( Record $ Core.makeRecordField <$>
          Dhall.Map.fromList
              [ (keyKey, Text)
              , (valueKey, Text)
              ]
      )

headersTypeExpr :: Expr Src Void
headersTypeExpr = makeHeadersTypeExpr "mapKey" "mapValue"

leagacyHeadersTypeExpr :: Expr Src Void
leagacyHeadersTypeExpr = makeHeadersTypeExpr "header" "value"

siteHeadersTypeExpr :: Expr Src Void
siteHeadersTypeExpr =
  App List
      ( Record $ Core.makeRecordField <$>
          Dhall.Map.fromList
              [ ("mapKey", Text)
              , ("mapValue", headersTypeExpr)
              ]
      )

typecheck :: Expr Src Void -> Expr Src Void -> IO (Expr Src Void)
typecheck expected expr = do
    let suffix_ = Dhall.Pretty.Internal.prettyToStrictText expected
    let annot = case expr of
            Note (Src begin end bytes) _ ->
                Note (Src begin end bytes') (Annot expr expected)
              where
                bytes' = bytes <> " : " <> suffix_
            _ ->
                Annot expr expected

    _ <- case (Dhall.TypeCheck.typeOf annot) of
        Left err -> throwM err
        Right _  -> return ()

    return (Core.normalize expr)

normalizeHeaders :: Expr Src Void -> IO (Expr Src Void)
normalizeHeaders headersExpr = do
    let handler₀ (e :: SomeException) = do
            {- Try to typecheck using the preferred @mapKey@/@mapValue@ fields
               and fall back to @header@/@value@ if that fails. However, if
               @header@/@value@ still fails then re-throw the original exception
               for @mapKey@ / @mapValue@. -}
            let handler₁ (_ :: SomeException) = throwM e
            handle handler₁ (typecheck leagacyHeadersTypeExpr headersExpr)

    handle handler₀ (typecheck headersTypeExpr headersExpr)

normalizeSiteHeaders :: Expr Src Void -> IO (Expr Src Void)
normalizeSiteHeaders = typecheck siteHeadersTypeExpr