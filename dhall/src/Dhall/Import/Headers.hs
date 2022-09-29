{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Dhall.Import.Headers
    ( normalizeHeaders
    , originHeadersTypeExpr
    , toHeaders
    , toOriginHeaders
    ) where

import Control.Applicative (Alternative (..), liftA2)
import Control.Exception   (SomeException)
import Control.Monad.Catch (handle, throwM)
import Data.Text           (Text)
import Data.Void           (Void)
import Dhall.Core          (Chunks (..), Expr (..))
import Dhall.Import.Types  (HTTPHeader, OriginHeaders)
import Dhall.Parser        (Src (..))

import qualified Data.CaseInsensitive
import qualified Data.Foldable
import qualified Data.HashMap.Strict   as HashMap
import qualified Data.Text.Encoding
import qualified Dhall.Core            as Core
import qualified Dhall.Map
import qualified Dhall.Pretty.Internal
import qualified Dhall.TypeCheck

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

-- | Normalize, typecheck and return OriginHeaders from a given expression.
toOriginHeaders :: Expr Src Void -> IO OriginHeaders
toOriginHeaders expr = fmap convert (normalizeOriginHeaders expr)
  where
    convert :: Expr s a -> OriginHeaders
    convert (ListLit _ hs) = HashMap.fromList (originPairs hs)
    convert _ = mempty

    originPairs hs = Data.Foldable.toList (Data.Foldable.fold (mapM toOriginPair hs))

    toOriginPair :: Expr s a -> Maybe (Text, [HTTPHeader])
    toOriginPair (RecordLit m) = do
      (Core.recordFieldValue -> TextLit (Chunks [] keyText), Core.recordFieldValue -> value)
          <- lookupMapKey
      return (keyText, toHeaders value)
        where
          lookupMapKey = liftA2 (,) (Dhall.Map.lookup "mapKey" m) (Dhall.Map.lookup "mapValue" m)
    toOriginPair _ = Nothing

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

originHeadersTypeExpr :: Expr Src Void
originHeadersTypeExpr =
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

normalizeOriginHeaders :: Expr Src Void -> IO (Expr Src Void)
normalizeOriginHeaders = typecheck originHeadersTypeExpr
