{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
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
    , InvalidType(..)
    , auto
    , InterpretOptions(..)
    , defaultInterpretOptions
    , bool
    , natural
    , integer
    , double
    , text
    , maybe
    , vector
    , GenericInterpret(..)

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
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Encoding
import qualified Data.Vector
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified NeatInterpolation

throws :: Exception e => Either e a -> IO a
throws (Left  e) = Control.Exception.throwIO e
throws (Right r) = return r

{-| Every `Type` must obey the contract that if an expression's type matches the
    the `expected` type then the `extract` function must succeed.  If not, then
    this exception is thrown

    This exception indicates that an invalid `Type` was provided to the `input`
    function
-}
data InvalidType = InvalidType deriving (Typeable)

_ERROR :: Data.Text.Text
_ERROR = "\ESC[1;31mError\ESC[0m"

instance Show InvalidType where
    show InvalidType = Data.Text.unpack [NeatInterpolation.text|
$_ERROR: Invalid Dhall.Type

Every Type must provide an extract function that succeeds if an expression
matches the expected type.  You provided a Type that disobeys this contract
|]

instance Exception InvalidType

{-| Type-check and evaluate a Dhall program, decoding the result into Haskell

    The first argument determines the type of value that you decode:

>>> input integer "2"
2
>>> input (vector double) "[1.0, 2.0]"
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
input (Type {..}) txt = do
    let delta = Directed "(input)" 0 0 0 0
    expr  <- throws (Dhall.Parser.exprFromText delta txt)
    expr' <- Dhall.Import.load expr
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
    _ <- throws (Dhall.TypeCheck.typeOf annot)
    case extract (Dhall.Core.normalize expr') of
        Just x  -> return x
        Nothing -> Control.Exception.throwIO InvalidType

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
    extractOut _ = Nothing

    expectedOut = App Optional expectedIn

{-| Decode a `Vector`

>>> input (vector integer) "[1, 2, 3]"
[1,2,3]
-}
vector :: Type a -> Type (Vector a)
vector (Type extractIn expectedIn) = Type extractOut expectedOut
  where
    extractOut (ListLit _ es) = traverse extractIn es
    extractOut  _             = Nothing

    expectedOut = App List expectedIn

{-| Any value that implements `Interpret` can be automatically decoded based on
    the inferred return type of `input`

>>> input auto "[1, 2, 3]" :: IO (Vector Integer)
[1,2,3]

    This class auto-generates a default implementation for records that
    implement `Generic`.  This does not auto-generate an instance for recursive
    types.
-}
class Interpret a where
    autoWith:: InterpretOptions -> Type a
    default autoWith
        :: (Generic a, GenericInterpret (Rep a)) => InterpretOptions -> Type a
    autoWith options = fmap GHC.Generics.to (genericAutoWith options)

instance Interpret Bool where
    autoWith _ = bool

instance Interpret Natural where
    autoWith _ = natural

instance Interpret Integer where
    autoWith _ = integer

instance Interpret Double where
    autoWith _ = double

instance Interpret Text where
    autoWith _ = text

instance Interpret a => Interpret (Maybe a) where
    autoWith opts = maybe (autoWith opts)

instance Interpret a => Interpret (Vector a) where
    autoWith opts = vector (autoWith opts)

{-| Use the default options for interpreting a configuration file

> auto = autoWith defaultInterpretOptions
-}
auto :: Interpret a => Type a
auto = autoWith defaultInterpretOptions

{-| Use these options to tweak how Dhall derives a generic implementation of
    `Interpret`
-}
data InterpretOptions = InterpretOptions
    { fieldModifier       :: Text -> Text
    -- ^ Function used to transform Haskell field names into their corresponding
    --   Dhall field names
    , constructorModifier :: Text -> Text
    -- ^ Function used to transform Haskell constructor names into their
    --   corresponding Dhall alternative names
    }

{-| Default interpret options, which you can tweak or override, like this:

> autoWith
>     (defaultInterpretOptions { fieldModifier = Data.Text.Lazy.dropWhile (== '_') })
-}
defaultInterpretOptions :: InterpretOptions
defaultInterpretOptions = InterpretOptions
    { fieldModifier       = id
    , constructorModifier = id
    }

{-| This is the underlying class that powers the `Interpret` class's support
    for automatically deriving a generic implementation
-}
class GenericInterpret f where
    genericAutoWith :: InterpretOptions -> Type (f a)

instance GenericInterpret f => GenericInterpret (M1 D d f) where
    genericAutoWith = fmap (fmap M1) genericAutoWith

instance GenericInterpret V1 where
    genericAutoWith _ = Type {..}
      where
        extract _ = Nothing

        expected = Union Data.Map.empty

instance (Constructor c1, Constructor c2, GenericInterpret f1, GenericInterpret f2) => GenericInterpret (M1 C c1 f1 :+: M1 C c2 f2) where
    genericAutoWith options@(InterpretOptions {..}) = Type {..}
      where
        nL :: M1 i c1 f1 a
        nL = undefined

        nR :: M1 i c2 f2 a
        nR = undefined

        nameL = constructorModifier (Data.Text.Lazy.pack (conName nL))
        nameR = constructorModifier (Data.Text.Lazy.pack (conName nR))

        extract (UnionLit name e _)
            | name == nameL = fmap (L1 . M1) (extractL e)
            | name == nameR = fmap (R1 . M1) (extractR e)
            | otherwise     = Nothing
        extract _ = Nothing

        expected =
            Union (Data.Map.fromList [(nameL, expectedL), (nameR, expectedR)])

        Type extractL expectedL = genericAutoWith options
        Type extractR expectedR = genericAutoWith options

instance (Constructor c, GenericInterpret (f :+: g), GenericInterpret h) => GenericInterpret ((f :+: g) :+: M1 C c h) where
    genericAutoWith options@(InterpretOptions {..}) = Type {..}
      where
        n :: M1 i c h a
        n = undefined

        name = constructorModifier (Data.Text.Lazy.pack (conName n))

        extract u@(UnionLit name' e _)
            | name == name' = fmap (R1 . M1) (extractR e)
            | otherwise     = fmap  L1       (extractL u)
        extract _ = Nothing

        expected = Union (Data.Map.insert name expectedR expectedL)

        Type extractL (Union expectedL) = genericAutoWith options
        Type extractR        expectedR  = genericAutoWith options

instance (Constructor c, GenericInterpret f, GenericInterpret (g :+: h)) => GenericInterpret (M1 C c f :+: (g :+: h)) where
    genericAutoWith options@(InterpretOptions {..}) = Type {..}
      where
        n :: M1 i c f a
        n = undefined

        name = constructorModifier (Data.Text.Lazy.pack (conName n))

        extract u@(UnionLit name' e _)
            | name == name' = fmap (L1 . M1) (extractL e)
            | otherwise     = fmap  R1       (extractR u)
        extract _ = Nothing

        expected = Union (Data.Map.insert name expectedL expectedR)

        Type extractL        expectedL  = genericAutoWith options
        Type extractR (Union expectedR) = genericAutoWith options

instance (GenericInterpret (f :+: g), GenericInterpret (h :+: i)) => GenericInterpret ((f :+: g) :+: (h :+: i)) where
    genericAutoWith options = Type {..}
      where
        extract e = fmap L1 (extractL e) <|> fmap R1 (extractR e)

        expected = Union (Data.Map.union expectedL expectedR)

        Type extractL (Union expectedL) = genericAutoWith options
        Type extractR (Union expectedR) = genericAutoWith options

instance GenericInterpret f => GenericInterpret (M1 C c f) where
    genericAutoWith = fmap (fmap M1) genericAutoWith

instance GenericInterpret U1 where
    genericAutoWith _ = Type {..}
      where
        extract _ = Just U1

        expected = Record (Data.Map.fromList [])

instance (GenericInterpret f, GenericInterpret g) => GenericInterpret (f :*: g) where
    genericAutoWith options = Type {..}
      where
        extract = liftA2 (liftA2 (:*:)) extractL extractR

        expected = Record (Data.Map.union ktsL ktsR)
          where
            Record ktsL = expectedL
            Record ktsR = expectedR
        Type extractL expectedL = genericAutoWith options
        Type extractR expectedR = genericAutoWith options

instance (Selector s, Interpret a) => GenericInterpret (M1 S s (K1 i a)) where
    genericAutoWith opts@(InterpretOptions {..}) = Type {..}
      where
        n :: M1 i s f a
        n = undefined

        extract (RecordLit m) = do
            case selName n of
                ""   -> Nothing
                name -> do
                    let name' = fieldModifier (Data.Text.Lazy.pack name)
                    e <- Data.Map.lookup name' m
                    fmap (M1 . K1) (extract' e)
        extract  _            = Nothing

        expected = Record (Data.Map.fromList [(key, expected')])
          where
            key = fieldModifier (Data.Text.Lazy.pack (selName n))

        Type extract' expected' = autoWith opts
