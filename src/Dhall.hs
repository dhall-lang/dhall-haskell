{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Dhall is a programming language specialized for configuration files.  This
-- tutorial explains how to author configuration files using this language.
--
-- The simplest way to use Dhall is to ignore the programming language features
-- and use it as a strongly typed configuration format.  For example, suppose that
-- you have the following configuration file:
-- 
-- > $ cat config
-- > < Example =
-- >     { foo = 1
-- >     , bar = [3.0, 4.0, 5.0] : List Double
-- >     }
-- > >
-- 
-- You can read the above configuration file into Haskell using the following
-- code:
-- 
-- > -- example.hs
-- > 
-- > {-# LANGUAGE DeriveGeneric     #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- > 
-- > import Dhall
-- > 
-- > data Example = Example { foo :: Integer , bar :: Vector Double }
-- >     deriving (Generic, Show)
-- > 
-- > instance Interpret Example
-- > 
-- > main :: IO ()
-- > main = do
-- >     x <- input auto "./config"
-- >     print (x :: Example)
-- 
-- If you compile and run the above program, the program prints the
-- corresponding Haskell record:
-- 
-- > $ ./example
-- > Example {foo = 1, bar = [3.0,4.0,5.0]}
--
-- In the above code, the data type definition for the @Example@ record
-- represents the schema for our configuration file.  Suppose that we modify our
-- configuration file to no longer match the schema, like this:
--
-- > $ echo "1" > config
--
-- Then our program will throw an exception when we try to load the
-- configuration file:
--
-- > $ ./example
-- > example: 
-- > Error: Expression doesn't match annotation
-- > 
-- > ./config : < Example : { bar : List Double, foo : Integer } >
-- > 
-- > (input):1:1
--
-- The Dhall programming language is a typed language and the above error
-- message is the output of the language's type-checker.  Every expression we
-- read into Haskell is type-checked against the expected schema.
--
-- The above error message says that the type-checker expected our @./config@ to
-- be a record with two fields: a field named @bar@ that is a @List@ of
-- @Double@s, and a field named @foo@ that is an @Integer@.  However, the type
-- checker found an expression whose inferred type was an `Integer`.  Since an
-- `Integer` is not the same thing as a record the type-checking step fails and
-- the code does not bother to marshal the configuration into Haskell.
--
-- More specifically, the code excerpt from the above error message has two
-- components:
--
-- * the expression being type checked (i.e. @./config@)
-- * the expression's expected type
--
-- > ./config  :  < Example : { bar : List Double, foo : Integer } >
-- > ⇧            ⇧
-- > Expression   Expected type
--
-- The @:@ symbol is how Dhall annotates values with their expected types.
-- Whenever you see:
--
-- > x : t
--
-- ... you should read that as \"we expect the expression @x@ to have type @t@\". 
-- If you are familiar with other functional programming languages, this is
-- exactly analogous to type annotations in Haskell or Purescript using the @(::)@
-- symbol or type annotations in Elm or ML using the @(:)@ symbol.
--
-- File paths like @./config@ are valid expressions which expand out to the
-- corresponding file's contents.  The @./config@ file's contents are currently
-- @1@, so @./config@ is just an elaborate synonym for the number @1@.  This
-- means that we could equivalently write:
--
-- > 1 : < Example : { bar : List Double, foo : Integer } >
--
-- The type checker rejects the above expression because the expression @1@
-- does not have type @\< Example : { bar : List Double, foo : Integer } \>@.
-- The actual type of @1@ is @Integer@, which is not even close to the same type.
--
-- The Dhall programming language also supports anonymous functions.  For
-- example, we can define a configuration file that is a function like this:
--
-- > $ cat > makeBools
-- > \(n : Bool) ->
-- >         [ n && True, n && False, n || True, n || False ] : List Bool
-- > <Ctrl-D>
--
-- ... or we can use Dhall's support for Unicode characters to use @λ@ instead of
-- @\\@ and @→@ instead of @->@:
--
-- > $ cat > makeBools
-- > λ(n : Bool) →
-- >         [ n && True, n && False, n || True, n || False ] : List Bool
-- > <Ctrl-D>
--
-- You can read this as a function of one argument named @n@ that has type @Bool@
-- This function returns a @List@ of @Bool@s.  Each element of the @List@ depends
-- on the input argument.
--
-- We can test our @makeBools@ function without having to modify and recompile
-- our Haskell program.  This library comes with a command-line executable program
-- named @dhall@ that you can use to both type-check configuration files and
-- convert them to a normal form.  Our compiler takes a program on standard input
-- and then prints the program's type to standard error followed by the program's
-- normal form to standard output:
--
-- > $ dhall <<< "./makeBools"
-- > ∀(n : Bool) → List Bool
-- > 
-- > λ(n : Bool) → [n && True, n && False, n || True, n || False] : List Bool
--
-- The first line says that @makeBools@ is a function of one argument named @n@
-- that has type @Bool@ and the function returns a @List@ of @Bool@s.  The
-- second line is our program's normal form, which in this case happens to be
-- identical to our original program.
--
-- We can \"apply\" our file to a @Bool@ argument, like this:
--
-- > $ dhall <<< "./makeBools True"
-- > List Bool
-- > 
-- > [True, False, True, True] : List Bool
--
-- Remember that file paths are synonymous with their contents, so the above
-- code is equivalent to:
-- 
-- > $ dhall <<< "(λ(n : Bool) → [n && True, n && False, n || True, n || False] : List Bool) True"
-- > List Bool
-- > 
-- > [True, False, True, True] : List Bool
--
-- Functions are separated from their arguments by whitespace.  So if you see:
--
-- @f x@
--
-- ... you should read that as \"apply the function @f@ to the argument @x@\".
--
-- When you apply an anonymous function to an argument, you substitute the
-- \"bound variable" with the function's argument:
--
-- > (λ(n : Bool) → ...) True
-- >    ⇧                ⇧
-- >    Bound variable   Function argument
--
-- So in our above example, we would replace all occurrences of @n@ with @True@,
-- like this:
--
-- > -- If we replace all of these `n`s with `True` ...
-- > [n && True, n && False, n || True, n || False] : List Bool
-- >
-- > -- ... then we get this:
-- > [True && True, True && False, True || True, True || False] : List Bool
-- >
-- > -- ... which further reduces to:
-- > [True, False, True, True] : List Bool
--
-- Now that we've verified that our function type checks and works, we can use
-- the same function within our Haskell program:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Dhall
-- >
-- > main :: IO ()
-- > main = do
-- >     x <- input auto "./makeBools True"
-- >     print (x :: Vector Bool)
--
-- This produces the following output:
--
-- > $ ./example
-- > [True,False,True,True]
--
-- Note that the `input` function accepts any arbitrary Dhall expression and is
-- not limited to just file paths.  For example, we could write:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Dhall
-- >
-- > main :: IO ()
-- > main = do
-- >     x <- input auto "True && False"
-- >     print (x :: Bool)
--
-- ... and that would print:
--
-- > $ ./example
-- > False
--
-- We can also decode into some types without declaring a corresponding Haskell
-- record to store the output.  In the last two examples we decoded the result
-- directly into either a `Vector` of `Bool`s or a `Bool`.  You can see what types
-- are supported \"out-of-the-box\" by examining the instances for the `Interpret`
-- class.
--
-- For example, the following instance says that we can directly decode any
-- Dhall expression that evaluates to a @Bool@ into a Haskell `Bool`:
--
-- > instance Interpret Bool
--
-- ... and there is another instance that says that if we can decode a value of
-- type @a@, then we can also decode a @List@ of values as a `Vector` of @a@s:
--
-- > instance Interpret a => Interpret (Vector a)
--
-- You can also use the Dhall compiler to evaluate expressions which have no
-- file references.  For example:
--
-- > $ dhall
-- > "Hello, " <> "world!"
-- > <Ctrl-D>
-- > Text
-- > 
-- > "Hello, world!"
--
-- > $ dhall
-- > +10 * +10
-- > Natural
-- > 
-- > +100
--
-- Dhall is a very restricted programming language that only supports simple
-- operations.  For example, Dhall only support addition and subtraction on
-- `Natural` numbers (i.e. non-negative numbers), which are not the same type of
-- number as `Integer`s (which can be negative).  A `Natural` number is a number
-- prefixed with the @+@ symbol.  If you try to add or multiply two `Integer`s
-- (without the @+@ prefix) you will get a type error:
--
-- > $ dhall
-- > 2 + 2
-- > <Ctrl-D>
-- > dhall: 
-- > Expression: 2 + 2
-- > 
-- > Error: Cannot use `(+)` on a value that's not a `Natural`
-- > 
-- > Explanation: The `(+)` operator expects two arguments of type `Natural`
-- > 
-- > You provided this argument:
-- > 
-- >     2 + ...
-- > 
-- > ... whose type is not `Natural`.  The type is actually:
-- > ↳ Integer
-- > 
-- > An `Integer` is not the same thing as a `Natural` number.  They are distinct
-- > types: `Integer`s can be negative, but `Natural` numbers must be non-negative
-- > 
-- > You can prefix an `Integer` literal with a `+` to create a `Natural` literal
-- > 
-- > Example:
-- > 
-- >     +2 + ...
--
-- The Dhall language doesn't just type-check the final schema; the language
-- also ensures that every expression is internally consistent.  For example,
-- suppose that we call @./makeBools@ on a non-`Bool` argument:
--
--
-- > $ dhall
-- > ./makeBools "ABC"
-- > dhall: 
-- > Expression: (λ(n : Bool) → [n && True, n && False, n || True, n || False] : List Bool) "ABC"
-- > 
-- > Error: Function applied to the wrong type or kind of argument
-- > 
-- > Explanation: Every function declares what type or kind of argument to accept
-- > 
-- >     λ(x : Bool) → x    -- Anonymous function which only accepts `Bool` arguments
-- > 
-- >     let f (x : Bool) = x   -- Named function which only accepts `Bool` arguments
-- >     in  f True
-- > 
-- >     λ(a : Type) → a    -- Anonymous function which only accepts `Type` arguments
-- > 
-- > You *cannot* apply a function to the wrong type or kind of argument:
-- > 
-- >     (λ(x : Bool) → x) "A"  -- "A" is `Text`, but the function expects a `Bool`
-- > 
-- > You tried to invoke a function which expects an argument of type or kind:
-- > ↳ Bool
-- > ... on an argument of type or kind:
-- > ↳ Text
--
-- We get a type error saying that our function expects a `Bool` argument, but
-- we supplied an argument of type `Text` instead.
--
-- Our `input` function also doesn't need to reference any files at all:
--
-- >>> input auto "True && False" :: IO Bool
-- False
--
-- Reading from an external configuration file is just a special case of Dhall's
-- support for embedding files as expressions.  There's no limit to how many
-- files-as-expressions that you can nest this way.  For example, we can define
-- one file that is a Dhall expression that in turn depends on another file
-- which is also a Dhall expression:
--
-- > $ echo './bool1 && ./bool2' > both
-- > $ echo 'True'  > bool1
-- > $ echo 'False' > bool2
-- > $ dhall
-- > [ ./bool1 , ./bool2 , ./both ] : List Bool
-- > <Ctrl-D>
-- > List Bool
-- > 
-- > [ True, False, False ] : List Bool
--
-- The only restriction is that the Dhall language will forbid cycles in these
-- file references:
--
-- > $ echo './bar' > foo
-- > $ echo './foo' > bar
-- > $ dhall < ./foo
-- > dhall: 
-- > ⤷ ./bar 
-- > ⤷ ./foo 
-- > Cyclic import: ./bar 
--
-- Dhall is a total programming language, which means that Dhall is not
-- Turing-complete and evaluation of every Dhall program is guaranteed to
-- eventually halt.  There is no upper bound on how long the program might take
-- to evaluate, but the program is guaranteed to terminate in a finite amount of
-- time and not hang forever.
--
-- This guarantees that all Dhall programs can be safely reduced to a normal
-- form where all functions have been evaluated.  In fact, Dhall expressions can
-- be evaluated even if all function arguments haven't been fully applied.  For
-- example, the following program is an anonymous function:
--
-- > $ dhall
-- > \(n : Bool) -> +10 * +10
-- > <Ctrl-D>
-- > ∀(n : Bool) → Natural
-- > 
-- > λ(n : Bool) → +100
--
-- ... and even though the function is still missing the first argument named
-- @n@ the compiler is smart enough to evaluate the body of the anonymous
-- function ahead of time before the function has even been invoked.
--
-- Similarly, you can use this normalization process to remove indirection
-- introduced by well-meaning software engineers over-architecting the
-- configuration file.

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
    , Text
    , Vector
    , Generic
    ) where

import Control.Applicative (empty, liftA2, (<|>))
import Control.Exception (Exception)
import Data.Monoid ((<>))
import Data.Text.Buildable (Buildable)
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
            . Dhall.Core.buildExpr0
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

>>> input (maybe integer) "[] : Maybe Integer"
Nothing
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
