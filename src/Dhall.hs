{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-| Please read the "Dhall.Tutorial" module, which contains a tutorial explaining
    how to use the language, the compiler, and this library
-}

module Dhall
    (
    -- * Input
      input
    , detailed

    -- * Types
    , Type
    , Interpret(..)
    , bool
    , natural
    , integer
    , double
    , text
    , maybe
    , vector

    -- * Re-exports
    , Natural
    , Text
    , Vector
    , Generic
    ) where

import Control.Applicative (empty, liftA2, (<|>))
import Control.Exception (Exception)
import Data.Monoid ((<>))
import Data.Text.Buildable (Buildable(..))
import Data.Text.Lazy (Text)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Dhall.Core (Expr(..))
import Dhall.Import (Imported(..))
import Dhall.Parser (Src(..))
import Dhall.TypeCheck (DetailedTypeError(..), TypeError, X)
import GHC.Generics
import Numeric.Natural (Natural)
import Prelude hiding (maybe)
import Text.Trifecta.Delta (Delta(..))

import qualified Control.Exception
import qualified Data.ByteString.Lazy
import qualified Data.Map
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Encoding
import qualified Data.Vector
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified GHC.Generics

throws :: Exception e => Either e a -> IO a
throws (Left  e) = Control.Exception.throwIO e
throws (Right r) = return r

{-| Type-check and evaluate a Dhall program, decoding the result into Haskell

    The first argument determines the type of value that you decode:

>>> input integer "2"
2
>>> input (vector double) "[ 1.0, 2.0 ] : List Bool"
[1.0,2.0]

    Use `auto` to automatically select which type to decode based on the
    inferred return type:

>>> input auto "True" :: IO Bool
True
-}
input
    :: Type a
    -- ^ The type of value to decode from Dhall to Haskell
    -> Text
    -- ^ The Dhall program
    -> IO a
    -- ^ The decoded value in Haskell
input (Type {..}) text = do
    let delta = Directed "(input)" 0 0 0 0
    expr     <- throws (Dhall.Parser.exprFromText delta text)
    expr'    <- Dhall.Import.load Nothing expr
    let suffix =
            ( Data.ByteString.Lazy.toStrict
            . Data.Text.Lazy.Encoding.encodeUtf8
            . Data.Text.Lazy.Builder.toLazyText
            . build
            ) expected
    let annot = case expr' of
            Note (Src begin end bytes) _ ->
                Note (Src begin end bytes') (Annot expr' expected)
              where
                bytes' = bytes <> " : " <> suffix
            _ ->
                Annot expr' expected
    typeExpr <- throws (Dhall.TypeCheck.typeOf annot)
    case extract (Dhall.Core.normalize expr') of
        Just x  -> return x
        Nothing -> fail "input: malformed `Type`"

{-| Use this to provide more detailed error messages

>> input auto "True" :: IO Integer
> *** Exception: Error: Expression doesn't match annotation
> 
> True : Integer
> 
> (input):1:1

>> detailed (input auto "True") :: IO Integer
> *** Exception: Error: Expression doesn't match annotation
> 
> Explanation: You can annotate an expression with its type or kind using the
> ❰:❱ symbol, like this:
> 
> 
>     ┌───────┐
>     │ x : t │  ❰x❱ is an expression and ❰t❱ is the annotated type or kind of ❰x❱
>     └───────┘
> 
> The type checker verifies that the expression's type or kind matches the
> provided annotation
> 
> For example, all of the following are valid annotations that the type checker
> accepts:
> 
> 
>     ┌─────────────┐
>     │ 1 : Integer │  ❰1❱ is an expression that has type ❰Integer❱, so the type
>     └─────────────┘  checker accepts the annotation
> 
> 
>     ┌────────────────────────┐
>     │ Natural/even +2 : Bool │  ❰Natural/even +2❱ has type ❰Bool❱, so the type
>     └────────────────────────┘  checker accepts the annotation
> 
> 
>     ┌────────────────────┐
>     │ List : Type → Type │  ❰List❱ is an expression that has kind ❰Type → Type❱,
>     └────────────────────┘  so the type checker accepts the annotation
> 
> 
>     ┌──────────────────┐
>     │ List Text : Type │  ❰List Text❱ is an expression that has kind ❰Type❱, so
>     └──────────────────┘  the type checker accepts the annotation
> 
> 
> However, the following annotations are not valid and the type checker will
> reject them:
> 
> 
>     ┌──────────┐
>     │ 1 : Text │  The type checker rejects this because ❰1❱ does not have type
>     └──────────┘  ❰Text❱
> 
> 
>     ┌─────────────┐
>     │ List : Type │  ❰List❱ does not have kind ❰Type❱
>     └─────────────┘
> 
> 
> You or the interpreter annotated this expression:
> 
> ↳ True
> 
> ... with this type or kind:
> 
> ↳ Integer
> 
> ... but the inferred type or kind of the expression is actually:
> 
> ↳ Bool
> 
> Some common reasons why you might get this error:
> 
> ● The Haskell Dhall interpreter implicitly inserts a top-level annotation
>   matching the expected type
> 
>   For example, if you run the following Haskell code:
> 
> 
>     ┌───────────────────────────────┐
>     │ >>> input auto "1" :: IO Text │
>     └───────────────────────────────┘
> 
> 
>   ... then the interpreter will actually type check the following annotated
>   expression:
> 
> 
>     ┌──────────┐
>     │ 1 : Text │
>     └──────────┘
> 
> 
>   ... and then type-checking will fail
> 
> ────────────────────────────────────────────────────────────────────────────────
> 
> True : Integer
> 
> (input):1:1

-}
detailed :: IO a -> IO a
detailed =
    Control.Exception.handle handler1 . Control.Exception.handle handler0
  where
    handler0 :: Imported (TypeError Src) -> IO a
    handler0 (Imported ps e) =
        Control.Exception.throwIO (Imported ps (DetailedTypeError e))

    handler1 :: TypeError Src -> IO a
    handler1 e = Control.Exception.throwIO (DetailedTypeError e)

{-| A @(Type a)@ represents a way to marshal a value of type @\'a\'@ from Dhall
    into Haskell

    You can produce `Type`s either explicitly:

> example :: Type (Vector Text)
> example = vector text

    ... or implicitly using `auto`:

> example :: Type (Vector Text)
> example = auto

    You can consume `Type`s using the `input` function:

> input :: Type a -> Text -> IO a
-}
data Type a = Type
    { extract  :: Expr X X -> Maybe a
    , expected :: Expr Src X
    }
    deriving (Functor)

{-| Decode a `Bool`

>>> input bool "True"
True
-}
bool :: Type Bool
bool = Type {..}
  where
    extract (BoolLit b) = pure b
    extract  _          = Nothing

    expected = Bool

{-| Decode a `Natural`

>>> input natural "+42"
42
-}
natural :: Type Natural
natural = Type {..}
  where
    extract (NaturalLit n) = pure n
    extract  _             = empty

    expected = Natural

{-| Decode an `Integer`

>>> input integer "42"
42
-}
integer :: Type Integer
integer = Type {..}
  where
    extract (IntegerLit n) = pure n
    extract  _             = empty

    expected = Integer

{-| Decode a `Double`

>>> input double "42.0"
42.0
-}
double :: Type Double
double = Type {..}
  where
    extract (DoubleLit n) = pure n
    extract  _            = empty

    expected = Double

{-| Decode `Text`

>>> input text "\"Test\""
"Test"
-}
text :: Type Text
text = Type {..}
  where
    extract (TextLit t) = pure (Data.Text.Lazy.Builder.toLazyText t)
    extract  _          = empty

    expected = Text

{-| Decode a `Maybe`

>>> input (maybe integer) "[1] : Optional Integer"
Just 1
-}
maybe :: Type a -> Type (Maybe a)
maybe (Type extractIn expectedIn) = Type extractOut expectedOut
  where
    extractOut (OptionalLit _ es) = traverse extractIn es'
      where
        es' = if Data.Vector.null es then Nothing else Just (Data.Vector.head es)

    expectedOut = App Optional expectedIn

{-| Decode a `Vector`

>>> input (vector integer) "[ 1, 2, 3 ] : List Integer"
[1,2,3]
-}
vector :: Type a -> Type (Vector a)
vector (Type extractIn expectedIn) = Type extractOut expectedOut
  where
    extractOut (ListLit _ es) = traverse extractIn es

    expectedOut = App List expectedIn

{-| Any value that implements `Interpret` can be automatically decoded based on
    the inferred return type of `input`

>>> input auto "[1, 2, 3 ] : List Integer" :: IO (Vector Integer)
[1,2,3]

    This class auto-generates a default implementation for records that
    implement `Generic`.  This does not auto-generate an instance for sum types
    nor recursive types.
-}
class Interpret a where
    auto :: Type a
    default auto :: (Generic a, GenericInterpret (Rep a)) => Type a
    auto = fmap GHC.Generics.to genericAuto

instance Interpret Bool where
    auto = bool

instance Interpret Natural where
    auto = natural

instance Interpret Integer where
    auto = integer

instance Interpret Double where
    auto = double

instance Interpret Text where
    auto = text

instance Interpret a => Interpret (Maybe a) where
    auto = maybe auto

instance Interpret a => Interpret (Vector a) where
    auto = vector auto

class GenericInterpret f where
    genericAuto :: Type (f a)

instance GenericInterpret f => GenericInterpret (M1 D d f) where
    genericAuto = fmap M1 genericAuto

instance GenericInterpret V1 where
    genericAuto = Type {..}
      where
        extract _ = Nothing

        expected = Union Data.Map.empty

instance (GenericInterpret f, GenericInterpret g) => GenericInterpret (f :+: g) where
    genericAuto = Type {..}
      where
        extract e = fmap L1 (extractL e) <|> fmap R1 (extractR e)

        expected = Union (Data.Map.union expectedL expectedR)

        Type extractL (Union expectedL) = genericAuto
        Type extractR (Union expectedR) = genericAuto

instance (Constructor c, GenericInterpret f) => GenericInterpret (M1 C c f) where
    genericAuto = Type {..}
      where
        n :: M1 i c f a
        n = undefined

        name = Data.Text.Lazy.pack (conName n)

        extract (UnionLit name' e _)
            | name == name' = fmap M1 (extract' e)
            | otherwise     = Nothing

        expected = Union (Data.Map.singleton name expected')

        Type extract' expected' = genericAuto

instance GenericInterpret U1 where
    genericAuto = Type {..}
      where
        extract _ = Just U1

        expected = Record (Data.Map.fromList [])

instance (GenericInterpret f, GenericInterpret g) => GenericInterpret (f :*: g) where
    genericAuto = Type {..}
      where
        extract = liftA2 (liftA2 (:*:)) extractL extractR

        expected = Record (Data.Map.union ktsL ktsR)
          where
            Record ktsL = expectedL
            Record ktsR = expectedR
        Type extractL expectedL = genericAuto
        Type extractR expectedR = genericAuto

instance (Selector s, Interpret a) => GenericInterpret (M1 S s (K1 i a)) where
    genericAuto = Type {..}
      where
        n :: M1 i s f a
        n = undefined

        extract (RecordLit m) = do
            case selName n of
                ""   -> Nothing
                name -> do
                    e <- Data.Map.lookup (Data.Text.Lazy.pack name) m
                    fmap (M1 . K1) (extract' e)
        extract  _            = Nothing

        expected = Record (Data.Map.fromList [(key, expected')])
          where
            key = Data.Text.Lazy.pack (selName n)

        Type extract' expected' = auto
