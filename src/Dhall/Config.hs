{-# LANGUAGE RecordWildCards #-}

module Dhall.Config
    ( -- *
      Type
    , input
    , natural
    , integer
    , double
    , text
    , vectorOf
    ) where

import Control.Exception (Exception)
import Data.Text.Lazy (Text)
import Data.Vector (Vector)
import Dhall.Core (Expr(..), X)
import Numeric.Natural (Natural)

import qualified Control.Exception
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Parser

throws :: Exception e => Either e a -> IO a
throws (Left  e) = Control.Exception.throwIO e
throws (Right r) = return r

data Type a = Type
    { extract  :: Expr X -> Maybe a
    , expected :: Expr X
    }

input :: Type a -> Text -> IO a
input (Type {..}) t = do
    expr     <- throws (Dhall.Parser.exprFromText t)
    expr'    <- Dhall.Import.load Nothing expr
    typeExpr <- throws (Dhall.Core.typeOf (Annot expr' expected))
    case extract (Dhall.Core.normalize expr') of
        Just x  -> return x
        Nothing -> fail "input: malformed `Type`"

natural :: Type Natural
natural = Type {..}
  where
    extract (NaturalLit n) = Just n
    extract  _             = Nothing

    expected = Natural

integer :: Type Integer
integer = Type {..}
  where
    extract (IntegerLit n) = Just n
    extract  _             = Nothing

    expected = Integer

double :: Type Double
double = Type {..}
  where
    extract (DoubleLit n) = Just n
    extract  _            = Nothing

    expected = Double

text :: Type Text
text = Type {..}
  where
    extract (TextLit t) = Just t
    extract  _          = Nothing

    expected = Text

vectorOf :: Type a -> Type (Vector a)
vectorOf (Type extractIn expectedIn) = Type extractOut expectedOut
  where
    extractOut (ListLit _ es) = traverse extractIn es

    expectedOut = List expectedIn
