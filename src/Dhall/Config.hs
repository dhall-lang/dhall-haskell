{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dhall.Config
    ( -- *
      Type
    , input
    , natural
    , integer
    , double
    , text
    , vector
    , pair2
    , pair3
    , Interpret(..)
    ) where

import Control.Applicative (empty)
import Control.Exception (Exception)
import Data.Text.Lazy (Text)
import Data.Vector (Vector)
import Dhall.Core (Expr(..), X)
import Numeric.Natural (Natural)

import qualified Control.Exception
import qualified Data.Map
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
    extract (NaturalLit n) = pure n
    extract  _             = empty

    expected = Natural

integer :: Type Integer
integer = Type {..}
  where
    extract (IntegerLit n) = pure n
    extract  _             = empty

    expected = Integer

double :: Type Double
double = Type {..}
  where
    extract (DoubleLit n) = pure n
    extract  _            = empty

    expected = Double

text :: Type Text
text = Type {..}
  where
    extract (TextLit t) = pure t
    extract  _          = empty

    expected = Text

vector :: Type a -> Type (Vector a)
vector (Type extractIn expectedIn) = Type extractOut expectedOut
  where
    extractOut (ListLit _ es) = traverse extractIn es

    expectedOut = List expectedIn

pair2 :: Type a -> Type b -> Type (a, b)
pair2
    (Type extractA expectedA)
    (Type extractB expectedB) = Type {..}
  where
    extract (RecordLit m) = do
        eA <- Data.Map.lookup "_1" m
        vA <- extractA eA
        eB <- Data.Map.lookup "_2" m
        vB <- extractB eB
        return (vA, vB)
    extract  _            = empty

    expected = Record (Data.Map.fromList kts)
      where
        kts =
            [ ("_1", expectedA)
            , ("_2", expectedB)
            ]

pair3 :: Type a -> Type b -> Type c -> Type (a, b, c)
pair3
    (Type extractA expectedA)
    (Type extractB expectedB)
    (Type extractC expectedC) = Type {..}
  where
    extract (RecordLit m) = do
        eA <- Data.Map.lookup "_1" m
        vA <- extractA eA
        eB <- Data.Map.lookup "_2" m
        vB <- extractB eB
        eC <- Data.Map.lookup "_3" m
        vC <- extractC eC
        return (vA, vB, vC)
    extract  _            = empty

    expected = Record (Data.Map.fromList kts)
      where
        kts =
            [ ("_1", expectedA)
            , ("_2", expectedB)
            , ("_3", expectedC)
            ]

class Interpret a where
    auto :: Type a

instance Interpret Natural where
    auto = natural

instance Interpret Integer where
    auto = integer

instance Interpret Double where
    auto = double

instance Interpret Text where
    auto = text

instance Interpret a => Interpret (Vector a) where
    auto = vector auto

instance (Interpret a, Interpret b) => Interpret (a, b) where
    auto = pair2 auto auto

instance (Interpret a, Interpret b, Interpret c) => Interpret (a, b, c) where
    auto = pair3 auto auto auto
