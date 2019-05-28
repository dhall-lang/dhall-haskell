{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TupleSections              #-}

{-| Please read the "Dhall.Tutorial" module, which contains a tutorial explaining
    how to use the language, the compiler, and this library
-}

module Dhall
    (
    -- * Input
      input
    , inputWithSettings
    , inputFile
    , inputFileWithSettings
    , inputExpr
    , inputExprWithSettings
    , rootDirectory
    , sourceName
    , startingContext
    , normalizer
    , standardVersion
    , defaultInputSettings
    , InputSettings
    , defaultEvaluateSettings
    , EvaluateSettings
    , HasEvaluateSettings
    , detailed

    -- * Types
    , Type(..)
    , RecordType(..)
    , UnionType(..)
    , InputType(..)
    , Interpret(..)
    , InvalidType(..)
    , auto
    , genericAuto
    , InterpretOptions(..)
    , defaultInterpretOptions
    , bool
    , natural
    , integer
    , scientific
    , double
    , lazyText
    , strictText
    , maybe
    , sequence
    , list
    , vector
    , unit
    , string
    , pair
    , record
    , field
    , union
    , constructor
    , GenericInterpret(..)
    , GenericInject(..)

    , Inject(..)
    , inject
    , genericInject
    , RecordInputType(..)
    , inputFieldWith
    , inputField
    , inputRecord
    , UnionInputType(..)
    , inputConstructorWith
    , inputConstructor
    , inputUnion
    , (>|<)

    -- * Miscellaneous
    , rawInput
    , (>$<)
    , (>*<)

    -- * Re-exports
    , Natural
    , Seq
    , Text
    , Vector
    , Generic
    ) where

import Control.Applicative (empty, liftA2, (<|>), Alternative)
import Control.Exception (Exception)
import Control.Monad.Trans.State.Strict
import Control.Monad (guard)
import Data.Coerce (coerce)
import Data.Functor.Contravariant (Contravariant(..), (>$<), Op(..))
import Data.Functor.Contravariant.Divisible (Divisible(..), divided)
import Data.Monoid ((<>))
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)
import Dhall.Binary (StandardVersion(..))
import Dhall.Core (Expr(..), Chunks(..))
import Dhall.Import (Imported(..))
import Dhall.Parser (Src(..))
import Dhall.TypeCheck (DetailedTypeError(..), TypeError, X)
import GHC.Generics
import Lens.Family (LensLike', set, view)
import Numeric.Natural (Natural)
import Prelude hiding (maybe, sequence)
import System.FilePath (takeDirectory)

import qualified Control.Applicative
import qualified Control.Exception
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable
import qualified Data.Functor.Compose
import qualified Data.Functor.Product
import qualified Data.Semigroup
import qualified Data.Scientific
import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Text.Lazy
import qualified Data.Vector
import qualified Dhall.Binary
import qualified Dhall.Context
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Map
import qualified Dhall.Parser
import qualified Dhall.Pretty.Internal
import qualified Dhall.TypeCheck
import qualified Dhall.Util

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XRecordWildCards

{-| Every `Type` must obey the contract that if an expression's type matches the
    the `expected` type then the `extract` function must succeed.  If not, then
    this exception is thrown

    This exception indicates that an invalid `Type` was provided to the `input`
    function
-}
data InvalidType s a = InvalidType
  { invalidTypeExpected :: Expr s a
  , invalidTypeExpression :: Expr s a
  }
  deriving (Typeable)

_ERROR :: String
_ERROR = "\ESC[1;31mError\ESC[0m"

instance (Pretty s, Pretty a, Typeable s, Typeable a) => Show (InvalidType s a) where
    show InvalidType { .. } =
        _ERROR <> ": Invalid Dhall.Type                                                  \n\
        \                                                                                \n\
        \Every Type must provide an extract function that succeeds if an expression      \n\
        \matches the expected type.  You provided a Type that disobeys this contract     \n\
        \                                                                                \n\
        \The Type provided has the expected dhall type:                                  \n\
        \                                                                                \n\
        \" <> show txt0 <> "\n\
        \                                                                                \n\
        \and it couldn't extract a value from the well-typed expression:                 \n\
        \                                                                                \n\
        \" <> show txt1 <> "\n\
        \                                                                                \n"
        where
          txt0 = Dhall.Util.insert invalidTypeExpected
          txt1 = Dhall.Util.insert invalidTypeExpression


instance (Pretty s, Pretty a, Typeable s, Typeable a) => Exception (InvalidType s a)

-- | @since 1.16
data InputSettings = InputSettings
  { _rootDirectory :: FilePath
  , _sourceName :: FilePath
  , _evaluateSettings :: EvaluateSettings
  }

-- | Default input settings: resolves imports relative to @.@ (the
-- current working directory), report errors as coming from @(input)@,
-- and default evaluation settings from 'defaultEvaluateSettings'.
--
-- @since 1.16
defaultInputSettings :: InputSettings
defaultInputSettings = InputSettings
  { _rootDirectory = "."
  , _sourceName = "(input)"
  , _evaluateSettings = defaultEvaluateSettings
  }

-- | Access the directory to resolve imports relative to.
--
-- @since 1.16
rootDirectory
  :: (Functor f)
  => LensLike' f InputSettings FilePath
rootDirectory k s =
  fmap (\x -> s { _rootDirectory = x }) (k (_rootDirectory s))

-- | Access the name of the source to report locations from; this is
-- only used in error messages, so it's okay if this is a best guess
-- or something symbolic.
--
-- @since 1.16
sourceName
  :: (Functor f)
  => LensLike' f InputSettings FilePath
sourceName k s =
  fmap (\x -> s { _sourceName = x}) (k (_sourceName s))

-- | @since 1.16
data EvaluateSettings = EvaluateSettings
  { _startingContext :: Dhall.Context.Context (Expr Src X)
  , _normalizer      :: Maybe (Dhall.Core.ReifiedNormalizer X)
  , _standardVersion :: StandardVersion
  }

-- | Default evaluation settings: no extra entries in the initial
-- context, and no special normalizer behaviour.
--
-- @since 1.16
defaultEvaluateSettings :: EvaluateSettings
defaultEvaluateSettings = EvaluateSettings
  { _startingContext = Dhall.Context.empty
  , _normalizer      = Nothing
  , _standardVersion = Dhall.Binary.defaultStandardVersion
  }

-- | Access the starting context used for evaluation and type-checking.
--
-- @since 1.16
startingContext
  :: (Functor f, HasEvaluateSettings s)
  => LensLike' f s (Dhall.Context.Context (Expr Src X))
startingContext = evaluateSettings . l
  where
    l :: (Functor f)
      => LensLike' f EvaluateSettings (Dhall.Context.Context (Expr Src X))
    l k s = fmap (\x -> s { _startingContext = x}) (k (_startingContext s))

-- | Access the custom normalizer.
--
-- @since 1.16
normalizer
  :: (Functor f, HasEvaluateSettings s)
  => LensLike' f s (Maybe (Dhall.Core.ReifiedNormalizer X))
normalizer = evaluateSettings . l
  where
    l :: (Functor f)
      => LensLike' f EvaluateSettings (Maybe (Dhall.Core.ReifiedNormalizer X))
    l k s = fmap (\x -> s { _normalizer = x }) (k (_normalizer s))

-- | Access the standard version (used primarily when encoding or decoding
-- Dhall expressions to and from a binary representation)
--
-- @since 1.17
standardVersion
    :: (Functor f, HasEvaluateSettings s)
    => LensLike' f s StandardVersion
standardVersion = evaluateSettings . l
  where
  l k s = fmap (\x -> s { _standardVersion = x}) (k (_standardVersion s))

-- | @since 1.16
class HasEvaluateSettings s where
  evaluateSettings
    :: (Functor f)
    => LensLike' f s EvaluateSettings

instance HasEvaluateSettings InputSettings where
  evaluateSettings k s =
    fmap (\x -> s { _evaluateSettings = x }) (k (_evaluateSettings s))

instance HasEvaluateSettings EvaluateSettings where
  evaluateSettings = id

{-| Type-check and evaluate a Dhall program, decoding the result into Haskell

    The first argument determines the type of value that you decode:

>>> input integer "+2"
2
>>> input (vector double) "[1.0, 2.0]"
[1.0,2.0]

    Use `auto` to automatically select which type to decode based on the
    inferred return type:

>>> input auto "True" :: IO Bool
True

    This uses the settings from 'defaultInputSettings'.
-}
input
    :: Type a
    -- ^ The type of value to decode from Dhall to Haskell
    -> Text
    -- ^ The Dhall program
    -> IO a
    -- ^ The decoded value in Haskell
input =
  inputWithSettings defaultInputSettings

{-| Extend 'input' with a root directory to resolve imports relative
    to, a file to mention in errors as the source, a custom typing
    context, and a custom normalization process.

@since 1.16
-}
inputWithSettings
    :: InputSettings
    -> Type a
    -- ^ The type of value to decode from Dhall to Haskell
    -> Text
    -- ^ The Dhall program
    -> IO a
    -- ^ The decoded value in Haskell
inputWithSettings settings (Type {..}) txt = do
    expr  <- Dhall.Core.throws (Dhall.Parser.exprFromText (view sourceName settings) txt)

    let InputSettings {..} = settings

    let EvaluateSettings {..} = _evaluateSettings

    let transform =
               set Dhall.Import.standardVersion _standardVersion
            .  set Dhall.Import.normalizer      _normalizer
            .  set Dhall.Import.startingContext _startingContext

    let status = transform (Dhall.Import.emptyStatus _rootDirectory)

    expr' <- State.evalStateT (Dhall.Import.loadWith expr) status

    let suffix = Dhall.Pretty.Internal.prettyToStrictText expected
    let annot = case expr' of
            Note (Src begin end bytes) _ ->
                Note (Src begin end bytes') (Annot expr' expected)
              where
                bytes' = bytes <> " : " <> suffix
            _ ->
                Annot expr' expected
    _ <- Dhall.Core.throws (Dhall.TypeCheck.typeWith (view startingContext settings) annot)
    let normExpr = Dhall.Core.normalizeWith (view normalizer settings) expr'

    case extract normExpr  of
        Just x  -> return x
        Nothing -> Control.Exception.throwIO
                     (InvalidType expected expr')

{-| Type-check and evaluate a Dhall program that is read from the
    file-system.

    This uses the settings from 'defaultEvaluateSettings'.

    @since 1.16
-}
inputFile
  :: Type a
  -- ^ The type of value to decode from Dhall to Haskell
  -> FilePath
  -- ^ The path to the Dhall program.
  -> IO a
  -- ^ The decoded value in Haskell.
inputFile =
  inputFileWithSettings defaultEvaluateSettings

{-| Extend 'inputFile' with a custom typing context and a custom
    normalization process.

@since 1.16
-}
inputFileWithSettings
  :: EvaluateSettings
  -> Type a
  -- ^ The type of value to decode from Dhall to Haskell
  -> FilePath
  -- ^ The path to the Dhall program.
  -> IO a
  -- ^ The decoded value in Haskell.
inputFileWithSettings settings ty path = do
  text <- Data.Text.IO.readFile path
  let inputSettings = InputSettings
        { _rootDirectory = takeDirectory path
        , _sourceName = path
        , _evaluateSettings = settings
        }
  inputWithSettings inputSettings ty text

{-| Similar to `input`, but without interpreting the Dhall `Expr` into a Haskell
    type.

    Uses the settings from 'defaultInputSettings'.
-}
inputExpr
    :: Text
    -- ^ The Dhall program
    -> IO (Expr Src X)
    -- ^ The fully normalized AST
inputExpr =
  inputExprWithSettings defaultInputSettings

{-| Extend 'inputExpr' with a root directory to resolve imports relative
    to, a file to mention in errors as the source, a custom typing
    context, and a custom normalization process.

@since 1.16
-}
inputExprWithSettings
    :: InputSettings
    -> Text
    -- ^ The Dhall program
    -> IO (Expr Src X)
    -- ^ The fully normalized AST
inputExprWithSettings settings txt = do
    expr  <- Dhall.Core.throws (Dhall.Parser.exprFromText (view sourceName settings) txt)

    let InputSettings {..} = settings

    let EvaluateSettings {..} = _evaluateSettings

    let transform =
               set Dhall.Import.standardVersion _standardVersion
            .  set Dhall.Import.normalizer      _normalizer
            .  set Dhall.Import.startingContext _startingContext

    let status = transform (Dhall.Import.emptyStatus _rootDirectory)

    expr' <- State.evalStateT (Dhall.Import.loadWith expr) status

    _ <- Dhall.Core.throws (Dhall.TypeCheck.typeWith (view startingContext settings) expr')
    pure (Dhall.Core.normalizeWith (view normalizer settings) expr')

-- | Use this function to extract Haskell values directly from Dhall AST.
--   The intended use case is to allow easy extraction of Dhall values for
--   making the function `Dhall.Core.normalizeWith` easier to use.
--
--   For other use cases, use `input` from `Dhall` module. It will give you
--   a much better user experience.
rawInput
    :: Alternative f
    => Type a
    -- ^ The type of value to decode from Dhall to Haskell
    -> Expr s X
    -- ^ a closed form Dhall program, which evaluates to the expected type
    -> f a
    -- ^ The decoded value in Haskell
rawInput (Type {..}) expr = do
    case extract (Dhall.Core.normalize expr) of
        Just x  -> pure x
        Nothing -> empty

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
>     │ 1 : Natural │  ❰1❱ is an expression that has type ❰Natural❱, so the type
>     └─────────────┘  checker accepts the annotation
>
>
>     ┌───────────────────────┐
>     │ Natural/even 2 : Bool │  ❰Natural/even 2❱ has type ❰Bool❱, so the type
>     └───────────────────────┘  checker accepts the annotation
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
    handler0 :: Imported (TypeError Src X) -> IO a
    handler0 (Imported ps e) =
        Control.Exception.throwIO (Imported ps (DetailedTypeError e))

    handler1 :: TypeError Src X -> IO a
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
    { extract  :: Expr Src X -> Maybe a
    -- ^ Extracts Haskell value from the Dhall expression
    , expected :: Expr Src X
    -- ^ Dhall type of the Haskell value
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

>>> input natural "42"
42
-}
natural :: Type Natural
natural = Type {..}
  where
    extract (NaturalLit n) = pure n
    extract  _             = empty

    expected = Natural

{-| Decode an `Integer`

>>> input integer "+42"
42
-}
integer :: Type Integer
integer = Type {..}
  where
    extract (IntegerLit n) = pure n
    extract  _             = empty

    expected = Integer

{-| Decode a `Scientific`

>>> input scientific "1e100"
1.0e100
-}
scientific :: Type Scientific
scientific = fmap Data.Scientific.fromFloatDigits double

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

{-| Decode lazy `Text`

>>> input lazyText "\"Test\""
"Test"
-}
lazyText :: Type Data.Text.Lazy.Text
lazyText = Type {..}
  where
    extract (TextLit (Chunks [] t)) = pure (Data.Text.Lazy.fromStrict t)
    extract  _                      = empty

    expected = Text

{-| Decode strict `Text`

>>> input strictText "\"Test\""
"Test"
-}
strictText :: Type Text
strictText = fmap Data.Text.Lazy.toStrict lazyText

{-| Decode a `Maybe`

>>> input (maybe natural) "Some 1"
Just 1
-}
maybe :: Type a -> Type (Maybe a)
maybe (Type extractIn expectedIn) = Type extractOut expectedOut
  where
    extractOut (Some e    ) = fmap Just (extractIn e)
    extractOut (App None _) = return Nothing
    extractOut _            = empty

    expectedOut = App Optional expectedIn

{-| Decode a `Seq`

>>> input (sequence natural) "[1, 2, 3]"
fromList [1,2,3]
-}
sequence :: Type a -> Type (Seq a)
sequence (Type extractIn expectedIn) = Type extractOut expectedOut
  where
    extractOut (ListLit _ es) = traverse extractIn es
    extractOut  _             = Nothing

    expectedOut = App List expectedIn

{-| Decode a list

>>> input (list natural) "[1, 2, 3]"
[1,2,3]
-}
list :: Type a -> Type [a]
list = fmap Data.Foldable.toList . sequence

{-| Decode a `Vector`

>>> input (vector natural) "[1, 2, 3]"
[1,2,3]
-}
vector :: Type a -> Type (Vector a)
vector = fmap Data.Vector.fromList . list

{-| Decode @()@ from an empty record.

>>> input unit "{=}"  -- GHC doesn't print the result if it is ()

-}
unit :: Type ()
unit = Type extractOut expectedOut
  where
    extractOut (RecordLit fields)
        | Data.Foldable.null fields = return ()
    extractOut _ = Nothing

    expectedOut = Record mempty

{-| Decode a `String`

>>> input string "\"ABC\""
"ABC"

-}
string :: Type String
string = Data.Text.Lazy.unpack <$> lazyText

{-| Given a pair of `Type`s, decode a tuple-record into their pairing.

>>> input (pair natural bool) "{ _1 = 42, _2 = False }"
(42,False)
-}
pair :: Type a -> Type b -> Type (a, b)
pair l r = Type extractOut expectedOut
  where
    extractOut (RecordLit fields) =
      (,) <$> ( Dhall.Map.lookup "_1" fields >>= extract l )
          <*> ( Dhall.Map.lookup "_2" fields >>= extract r )
    extractOut _ = Nothing

    expectedOut =
        Record
            (Dhall.Map.fromList
                [ ("_1", expected l)
                , ("_2", expected r)
                ]
            )

{-| Any value that implements `Interpret` can be automatically decoded based on
    the inferred return type of `input`

>>> input auto "[1, 2, 3]" :: IO (Vector Natural)
[1,2,3]

    This class auto-generates a default implementation for records that
    implement `Generic`.  This does not auto-generate an instance for recursive
    types.
-}
class Interpret a where
    autoWith:: InterpretOptions -> Type a
    default autoWith
        :: (Generic a, GenericInterpret (Rep a)) => InterpretOptions -> Type a
    autoWith options = fmap GHC.Generics.to (evalState (genericAutoWith options) 1)

instance Interpret Bool where
    autoWith _ = bool

instance Interpret Natural where
    autoWith _ = natural

instance Interpret Integer where
    autoWith _ = integer

instance Interpret Scientific where
    autoWith _ = scientific

instance Interpret Double where
    autoWith _ = double

instance {-# OVERLAPS #-} Interpret [Char] where
    autoWith _ = string

instance Interpret Data.Text.Lazy.Text where
    autoWith _ = lazyText

instance Interpret Text where
    autoWith _ = strictText

instance Interpret a => Interpret (Maybe a) where
    autoWith opts = maybe (autoWith opts)

instance Interpret a => Interpret (Seq a) where
    autoWith opts = sequence (autoWith opts)

instance Interpret a => Interpret [a] where
    autoWith = fmap (fmap Data.Vector.toList) autoWith

instance Interpret a => Interpret (Vector a) where
    autoWith opts = vector (autoWith opts)

instance (Inject a, Interpret b) => Interpret (a -> b) where
    autoWith opts = Type extractOut expectedOut
      where
        normalizer_ = Just (inputNormalizer opts)

        extractOut e = Just (\i -> case extractIn (Dhall.Core.normalizeWith normalizer_ (App e (embed i))) of
            Just o  -> o
            Nothing -> error "Interpret: You cannot decode a function if it does not have the correct type" )

        expectedOut = Pi "_" declared expectedIn

        InputType {..} = inject

        Type extractIn expectedIn = autoWith opts

instance (Interpret a, Interpret b) => Interpret (a, b)

{-| Use the default options for interpreting a configuration file

> auto = autoWith defaultInterpretOptions
-}
auto :: Interpret a => Type a
auto = autoWith defaultInterpretOptions

{-| `genericAuto` is the default implementation for `auto` if you derive
    `Interpret`.  The difference is that you can use `genericAuto` without
    having to explicitly provide an `Interpret` instance for a type as long as
    the type derives `Generic`
-}
genericAuto :: (Generic a, GenericInterpret (Rep a)) => Type a
genericAuto = fmap to (evalState (genericAutoWith defaultInterpretOptions) 1)

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
    , inputNormalizer     :: Dhall.Core.ReifiedNormalizer X
    -- ^ This is only used by the `Interpret` instance for functions in order
    --   to normalize the function input before marshaling the input into a
    --   Dhall expression
    }

{-| Default interpret options, which you can tweak or override, like this:

> autoWith
>     (defaultInterpretOptions { fieldModifier = Data.Text.Lazy.dropWhile (== '_') })
-}
defaultInterpretOptions :: InterpretOptions
defaultInterpretOptions = InterpretOptions
    { fieldModifier       = id
    , constructorModifier = id
    , inputNormalizer     = Dhall.Core.ReifiedNormalizer (const (pure Nothing))
    }

{-| This is the underlying class that powers the `Interpret` class's support
    for automatically deriving a generic implementation
-}
class GenericInterpret f where
    genericAutoWith :: InterpretOptions -> State Int (Type (f a))

instance GenericInterpret f => GenericInterpret (M1 D d f) where
    genericAutoWith options = do
        res <- genericAutoWith options
        pure (fmap M1 res)

instance GenericInterpret V1 where
    genericAutoWith _ = pure Type {..}
      where
        extract _ = Nothing

        expected = Union mempty

unsafeExpectUnion
    :: Text -> Expr Src X -> Dhall.Map.Map Text (Maybe (Expr Src X))
unsafeExpectUnion _ (Union kts) =
    kts
unsafeExpectUnion name expression =
    Dhall.Core.internalError
        (name <> ": Unexpected constructor: " <> Dhall.Core.pretty expression)

unsafeExpectRecord :: Text -> Expr Src X -> Dhall.Map.Map Text (Expr Src X)
unsafeExpectRecord _ (Record kts) =
    kts
unsafeExpectRecord name expression =
    Dhall.Core.internalError
        (name <> ": Unexpected constructor: " <> Dhall.Core.pretty expression)

unsafeExpectUnionLit
    :: Text
    -> Expr Src X
    -> (Text, Maybe (Expr Src X))
unsafeExpectUnionLit _ (Field (Union _) k) =
    (k, Nothing)
unsafeExpectUnionLit _ (App (Field (Union _) k) v) =
    (k, Just v)
unsafeExpectUnionLit name expression =
    Dhall.Core.internalError
        (name <> ": Unexpected constructor: " <> Dhall.Core.pretty expression)

unsafeExpectRecordLit :: Text -> Expr Src X -> Dhall.Map.Map Text (Expr Src X)
unsafeExpectRecordLit _ (RecordLit kvs) =
    kvs
unsafeExpectRecordLit name expression =
    Dhall.Core.internalError
        (name <> ": Unexpected constructor: " <> Dhall.Core.pretty expression)

notEmptyRecordLit :: Expr s a -> Maybe (Expr s a)
notEmptyRecordLit e = case e of
    RecordLit m | null m -> Nothing
    _                    -> Just e

notEmptyRecord :: Expr s a -> Maybe (Expr s a)
notEmptyRecord e = case e of
    Record m | null m -> Nothing
    _                 -> Just e
extractUnionConstructor
    :: Expr s a -> Maybe (Text, Expr s a, Dhall.Map.Map Text (Maybe (Expr s a)))
extractUnionConstructor (UnionLit fld e rest) =
  return (fld, e, rest)
extractUnionConstructor (App (Field (Union kts) fld) e) =
  return (fld, e, Dhall.Map.delete fld kts)
extractUnionConstructor (Field (Union kts) fld) =
  return (fld, RecordLit mempty, Dhall.Map.delete fld kts)
extractUnionConstructor _ =
  empty

instance (Constructor c1, Constructor c2, GenericInterpret f1, GenericInterpret f2) => GenericInterpret (M1 C c1 f1 :+: M1 C c2 f2) where
    genericAutoWith options@(InterpretOptions {..}) = pure (Type {..})
      where
        nL :: M1 i c1 f1 a
        nL = undefined

        nR :: M1 i c2 f2 a
        nR = undefined

        nameL = constructorModifier (Data.Text.pack (conName nL))
        nameR = constructorModifier (Data.Text.pack (conName nR))

        extract e0 = do
          (name, e1, _) <- extractUnionConstructor e0
          if
            | name == nameL -> fmap (L1 . M1) (extractL e1)
            | name == nameR -> fmap (R1 . M1) (extractR e1)
            | otherwise     -> Nothing

        expected =
            Union
                (Dhall.Map.fromList
                    [ (nameL, notEmptyRecord expectedL)
                    , (nameR, notEmptyRecord expectedR)
                    ]
                )

        Type extractL expectedL = evalState (genericAutoWith options) 1
        Type extractR expectedR = evalState (genericAutoWith options) 1

instance (Constructor c, GenericInterpret (f :+: g), GenericInterpret h) => GenericInterpret ((f :+: g) :+: M1 C c h) where
    genericAutoWith options@(InterpretOptions {..}) = pure (Type {..})
      where
        n :: M1 i c h a
        n = undefined

        name = constructorModifier (Data.Text.pack (conName n))

        extract u = do
          (name', e, _) <- extractUnionConstructor u
          if
            | name == name' -> fmap (R1 . M1) (extractR e)
            | otherwise     -> fmap  L1       (extractL u)

        expected =
            Union (Dhall.Map.insert name (notEmptyRecord expectedR) ktsL)

        Type extractL expectedL = evalState (genericAutoWith options) 1
        Type extractR expectedR = evalState (genericAutoWith options) 1

        ktsL = unsafeExpectUnion "genericAutoWith (:+:)" expectedL

instance (Constructor c, GenericInterpret f, GenericInterpret (g :+: h)) => GenericInterpret (M1 C c f :+: (g :+: h)) where
    genericAutoWith options@(InterpretOptions {..}) = pure (Type {..})
      where
        n :: M1 i c f a
        n = undefined

        name = constructorModifier (Data.Text.pack (conName n))

        extract u = do
          (name', e, _) <- extractUnionConstructor u
          if
            | name == name' -> fmap (L1 . M1) (extractL e)
            | otherwise     -> fmap  R1       (extractR u)

        expected =
            Union (Dhall.Map.insert name (notEmptyRecord expectedL) ktsR)

        Type extractL expectedL = evalState (genericAutoWith options) 1
        Type extractR expectedR = evalState (genericAutoWith options) 1

        ktsR = unsafeExpectUnion "genericAutoWith (:+:)" expectedR

instance (GenericInterpret (f :+: g), GenericInterpret (h :+: i)) => GenericInterpret ((f :+: g) :+: (h :+: i)) where
    genericAutoWith options = pure (Type {..})
      where
        extract e = fmap L1 (extractL e) <|> fmap R1 (extractR e)

        expected = Union (Dhall.Map.union ktsL ktsR)

        Type extractL expectedL = evalState (genericAutoWith options) 1
        Type extractR expectedR = evalState (genericAutoWith options) 1

        ktsL = unsafeExpectUnion "genericAutoWith (:+:)" expectedL
        ktsR = unsafeExpectUnion "genericAutoWith (:+:)" expectedR

instance GenericInterpret f => GenericInterpret (M1 C c f) where
    genericAutoWith options = do
        res <- genericAutoWith options
        pure (fmap M1 res)

instance GenericInterpret U1 where
    genericAutoWith _ = pure (Type {..})
      where
        extract _ = Just U1

        expected = Record (Dhall.Map.fromList [])

instance (GenericInterpret f, GenericInterpret g) => GenericInterpret (f :*: g) where
    genericAutoWith options = do
        Type extractL expectedL <- genericAutoWith options
        Type extractR expectedR <- genericAutoWith options
        let ktsL = unsafeExpectRecord "genericAutoWith (:*:)"expectedL
        let ktsR = unsafeExpectRecord "genericAutoWith (:*:)"expectedR
        pure
            (Type
                { extract = liftA2 (liftA2 (:*:)) extractL extractR
                , expected = Record (Dhall.Map.union ktsL ktsR)
                }
            )

getSelName :: Selector s => M1 i s f a -> State Int String
getSelName n = case selName n of
    "" -> do i <- get
             put (i + 1)
             pure ("_" ++ show i)
    nn -> pure nn

instance (Selector s, Interpret a) => GenericInterpret (M1 S s (K1 i a)) where
    genericAutoWith opts@(InterpretOptions {..}) = do
        name <- getSelName n
        let extract (RecordLit m) = do
                    let name' = fieldModifier (Data.Text.pack name)
                    e <- Dhall.Map.lookup name' m
                    fmap (M1 . K1) (extract' e)
            extract _            = Nothing
        let expected =
                Record (Dhall.Map.fromList [(key, expected')])
              where
                key = fieldModifier (Data.Text.pack name)
        pure (Type {..})
      where
        n :: M1 i s f a
        n = undefined

        Type extract' expected' = autoWith opts

{-| An @(InputType a)@ represents a way to marshal a value of type @\'a\'@ from
    Haskell into Dhall
-}
data InputType a = InputType
    { embed    :: a -> Expr Src X
    -- ^ Embeds a Haskell value as a Dhall expression
    , declared :: Expr Src X
    -- ^ Dhall type of the Haskell value
    }

instance Contravariant InputType where
    contramap f (InputType embed declared) = InputType embed' declared
      where
        embed' x = embed (f x)

{-| This class is used by `Interpret` instance for functions:

> instance (Inject a, Interpret b) => Interpret (a -> b)

    You can convert Dhall functions with "simple" inputs (i.e. instances of this
    class) into Haskell functions.  This works by:

    * Marshaling the input to the Haskell function into a Dhall expression (i.e.
      @x :: Expr Src X@)
    * Applying the Dhall function (i.e. @f :: Expr Src X@) to the Dhall input
      (i.e. @App f x@)
    * Normalizing the syntax tree (i.e. @normalize (App f x)@)
    * Marshaling the resulting Dhall expression back into a Haskell value
-}
class Inject a where
    injectWith :: InterpretOptions -> InputType a
    default injectWith
        :: (Generic a, GenericInject (Rep a)) => InterpretOptions -> InputType a
    injectWith options
        = contramap GHC.Generics.from (evalState (genericInjectWith options) 1)

{-| Use the default options for injecting a value

> inject = inject defaultInterpretOptions
-}
inject :: Inject a => InputType a
inject = injectWith defaultInterpretOptions

{-| Use the default options for injecting a value, whose structure is
determined generically.

This can be used when you want to use 'Inject' on types that you don't
want to define orphan instances for.
-}
genericInject
  :: (Generic a, GenericInject (Rep a)) => InputType a
genericInject
    = contramap GHC.Generics.from (evalState (genericInjectWith defaultInterpretOptions) 1)

instance Inject Bool where
    injectWith _ = InputType {..}
      where
        embed = BoolLit

        declared = Bool

instance Inject Data.Text.Lazy.Text where
    injectWith _ = InputType {..}
      where
        embed text =
            TextLit (Chunks [] (Data.Text.Lazy.toStrict text))

        declared = Text

instance Inject Text where
    injectWith _ = InputType {..}
      where
        embed text = TextLit (Chunks [] text)

        declared = Text

instance Inject Natural where
    injectWith _ = InputType {..}
      where
        embed = NaturalLit

        declared = Natural

instance Inject Integer where
    injectWith _ = InputType {..}
      where
        embed = IntegerLit

        declared = Integer

instance Inject Int where
    injectWith _ = InputType {..}
      where
        embed = IntegerLit . toInteger

        declared = Integer

instance Inject Word8 where
    injectWith _ = InputType {..}
      where
        embed = IntegerLit . toInteger

        declared = Integer

instance Inject Word16 where
    injectWith _ = InputType {..}
      where
        embed = IntegerLit . toInteger

        declared = Integer

instance Inject Word32 where
    injectWith _ = InputType {..}
      where
        embed = IntegerLit . toInteger

        declared = Integer

instance Inject Word64 where
    injectWith _ = InputType {..}
      where
        embed = IntegerLit . toInteger

        declared = Integer

instance Inject Double where
    injectWith _ = InputType {..}
      where
        embed = DoubleLit

        declared = Double

instance Inject () where
    injectWith _ = InputType {..}
      where
        embed = const (RecordLit mempty)

        declared = Record mempty

instance Inject a => Inject (Maybe a) where
    injectWith options = InputType embedOut declaredOut
      where
        embedOut (Just x) =
            OptionalLit declaredIn (pure (embedIn x))
        embedOut Nothing =
            OptionalLit declaredIn  empty

        InputType embedIn declaredIn = injectWith options

        declaredOut = App Optional declaredIn

instance Inject a => Inject (Seq a) where
    injectWith options = InputType embedOut declaredOut
      where
        embedOut xs = ListLit (Just declaredIn) (fmap embedIn xs)

        declaredOut = App List declaredIn

        InputType embedIn declaredIn = injectWith options

instance Inject a => Inject [a] where
    injectWith = fmap (contramap Data.Sequence.fromList) injectWith

instance Inject a => Inject (Vector a) where
    injectWith = fmap (contramap Data.Vector.toList) injectWith

instance Inject a => Inject (Data.Set.Set a) where
    injectWith = fmap (contramap Data.Set.toList) injectWith

instance (Inject a, Inject b) => Inject (a, b)

{-| This is the underlying class that powers the `Interpret` class's support
    for automatically deriving a generic implementation
-}
class GenericInject f where
    genericInjectWith :: InterpretOptions -> State Int (InputType (f a))

instance GenericInject f => GenericInject (M1 D d f) where
    genericInjectWith options = do
        res <- genericInjectWith options
        pure (contramap unM1 res)

instance GenericInject f => GenericInject (M1 C c f) where
    genericInjectWith options = do
        res <- genericInjectWith options
        pure (contramap unM1 res)

instance (Constructor c1, Constructor c2, GenericInject f1, GenericInject f2) => GenericInject (M1 C c1 f1 :+: M1 C c2 f2) where
    genericInjectWith options@(InterpretOptions {..}) = pure (InputType {..})
      where
        embed (L1 (M1 l)) =
            case notEmptyRecordLit (embedL l) of
                Nothing ->
                    Field declared keyL
                Just valL ->
                    App (Field declared keyL) valL

        embed (R1 (M1 r)) =
            case notEmptyRecordLit (embedR r) of
                Nothing ->
                    Field declared keyR
                Just valR ->
                    App (Field declared keyR) valR

        declared =
            Union
                (Dhall.Map.fromList
                    [ (keyL, notEmptyRecord declaredL)
                    , (keyR, notEmptyRecord declaredR)
                    ]
                )

        nL :: M1 i c1 f1 a
        nL = undefined

        nR :: M1 i c2 f2 a
        nR = undefined

        keyL = constructorModifier (Data.Text.pack (conName nL))
        keyR = constructorModifier (Data.Text.pack (conName nR))

        InputType embedL declaredL = evalState (genericInjectWith options) 1
        InputType embedR declaredR = evalState (genericInjectWith options) 1

instance (Constructor c, GenericInject (f :+: g), GenericInject h) => GenericInject ((f :+: g) :+: M1 C c h) where
    genericInjectWith options@(InterpretOptions {..}) = pure (InputType {..})
      where
        embed (L1 l) =
            case maybeValL of
                Nothing   -> Field declared keyL
                Just valL -> App (Field declared keyL) valL
          where
            (keyL, maybeValL) =
              unsafeExpectUnionLit "genericInjectWith (:+:)" (embedL l)
        embed (R1 (M1 r)) =
            case notEmptyRecordLit (embedR r) of
                Nothing   -> Field declared keyR
                Just valR -> App (Field declared keyR) valR

        nR :: M1 i c h a
        nR = undefined

        keyR = constructorModifier (Data.Text.pack (conName nR))

        declared = Union (Dhall.Map.insert keyR (notEmptyRecord declaredR) ktsL)

        InputType embedL declaredL = evalState (genericInjectWith options) 1
        InputType embedR declaredR = evalState (genericInjectWith options) 1

        ktsL = unsafeExpectUnion "genericInjectWith (:+:)" declaredL

instance (Constructor c, GenericInject f, GenericInject (g :+: h)) => GenericInject (M1 C c f :+: (g :+: h)) where
    genericInjectWith options@(InterpretOptions {..}) = pure (InputType {..})
      where
        embed (L1 (M1 l)) =
            case notEmptyRecordLit (embedL l) of
                Nothing   -> Field declared keyL
                Just valL -> App (Field declared keyL) valL
        embed (R1 r) =
            case maybeValR of
                Nothing   -> Field declared keyR
                Just valR -> App (Field declared keyR) valR
          where
            (keyR, maybeValR) =
                unsafeExpectUnionLit "genericInjectWith (:+:)" (embedR r)

        nL :: M1 i c f a
        nL = undefined

        keyL = constructorModifier (Data.Text.pack (conName nL))

        declared = Union (Dhall.Map.insert keyL (notEmptyRecord declaredL) ktsR)

        InputType embedL declaredL = evalState (genericInjectWith options) 1
        InputType embedR declaredR = evalState (genericInjectWith options) 1

        ktsR = unsafeExpectUnion "genericInjectWith (:+:)" declaredR

instance (GenericInject (f :+: g), GenericInject (h :+: i)) => GenericInject ((f :+: g) :+: (h :+: i)) where
    genericInjectWith options = pure (InputType {..})
      where
        embed (L1 l) =
            case maybeValL of
                Nothing   -> Field declared keyL
                Just valL -> App (Field declared keyL) valL
          where
            (keyL, maybeValL) =
                unsafeExpectUnionLit "genericInjectWith (:+:)" (embedL l)
        embed (R1 r) =
            case maybeValR of
                Nothing   -> Field declared keyR
                Just valR -> App (Field declared keyR) valR
          where
            (keyR, maybeValR) =
                unsafeExpectUnionLit "genericInjectWith (:+:)" (embedR r)

        declared = Union (Dhall.Map.union ktsL ktsR)

        InputType embedL declaredL = evalState (genericInjectWith options) 1
        InputType embedR declaredR = evalState (genericInjectWith options) 1

        ktsL = unsafeExpectUnion "genericInjectWith (:+:)" declaredL
        ktsR = unsafeExpectUnion "genericInjectWith (:+:)" declaredR

instance (GenericInject f, GenericInject g) => GenericInject (f :*: g) where
    genericInjectWith options = do
        InputType embedInL declaredInL <- genericInjectWith options
        InputType embedInR declaredInR <- genericInjectWith options

        let embed (l :*: r) =
                RecordLit (Dhall.Map.union mapL mapR)
              where
                mapL =
                    unsafeExpectRecordLit "genericInjectWith (:*:)" (embedInL l)

                mapR =
                    unsafeExpectRecordLit "genericInjectWith (:*:)" (embedInR r)

        let declared = Record (Dhall.Map.union mapL mapR)
              where
                mapL = unsafeExpectRecord "genericInjectWith (:*:)" declaredInL
                mapR = unsafeExpectRecord "genericInjectWith (:*:)" declaredInR

        pure (InputType {..})

instance GenericInject U1 where
    genericInjectWith _ = pure (InputType {..})
      where
        embed _ = RecordLit mempty

        declared = Record mempty

instance (Selector s, Inject a) => GenericInject (M1 S s (K1 i a)) where
    genericInjectWith opts@(InterpretOptions {..}) = do
        name <- fieldModifier . Data.Text.pack <$> getSelName n
        let embed (M1 (K1 x)) =
                RecordLit (Dhall.Map.singleton name (embedIn x))
        let declared =
                Record (Dhall.Map.singleton name declaredIn)
        pure (InputType {..})
      where
        n :: M1 i s f a
        n = undefined

        InputType embedIn declaredIn = injectWith opts

{-| The 'RecordType' applicative functor allows you to build a 'Type' parser
    from a Dhall record.

    For example, let's take the following Haskell data type:

>>> :{
data Project = Project
  { projectName :: Text
  , projectDescription :: Text
  , projectStars :: Natural
  }
:}

    And assume that we have the following Dhall record that we would like to
    parse as a @Project@:

> { name =
>     "dhall-haskell"
> , description =
>     "A configuration language guaranteed to terminate"
> , stars =
>     289
> }

    Our parser has type 'Type' @Project@, but we can't build that out of any
    smaller parsers, as 'Type's cannot be combined (they are only 'Functor's).
    However, we can use a 'RecordType' to build a 'Type' for @Project@:

>>> :{
project :: Type Project
project =
  record
    ( Project <$> field "name" strictText
              <*> field "description" strictText
              <*> field "stars" natural
    )
:}
-}

newtype RecordType a =
  RecordType
    ( Data.Functor.Product.Product
        ( Control.Applicative.Const
            ( Dhall.Map.Map
                Text
                ( Expr Src X )
            )
        )
        ( Data.Functor.Compose.Compose
            ( (->) ( Expr Src X ) )
            Maybe
        )
        a
    )
  deriving (Functor, Applicative)


-- | Run a 'RecordType' parser to build a 'Type' parser.
record :: RecordType a -> Dhall.Type a
record ( RecordType ( Data.Functor.Product.Pair ( Control.Applicative.Const fields ) ( Data.Functor.Compose.Compose extractF ) ) ) =
  Type
    { extract =
        extractF
    , expected =
        Record fields
    }


-- | Parse a single field of a record.
field :: Text -> Type a -> RecordType a
field key valueType =
  let
    extractBody expr = do
      RecordLit fields <-
        return expr

      Dhall.Map.lookup key fields
        >>= extract valueType

  in
    RecordType
      ( Data.Functor.Product.Pair
          ( Control.Applicative.Const
              ( Dhall.Map.singleton
                  key
                  ( Dhall.expected valueType )
              )
          )
          ( Data.Functor.Compose.Compose extractBody )
      )

{-| The 'UnionType' monoid allows you to build a 'Type' parser
    from a Dhall union

    For example, let's take the following Haskell data type:

>>> :{
data Status = Queued Natural
            | Result Text
            | Errored Text
:}

    And assume that we have the following Dhall union that we would like to
    parse as a @Status@:

> < Result = "Finish succesfully"
> | Queued : Natural
> | Errored : Text
> >

    Our parser has type 'Type' @Status@, but we can't build that out of any
    smaller parsers, as 'Type's cannot be combined (they are only 'Functor's).
    However, we can use a 'UnionType' to build a 'Type' for @Status@:

>>> :{
status :: Type Status
status = union
  (  ( Queued  <$> constructor "Queued"  natural )
  <> ( Result  <$> constructor "Result"  strictText )
  <> ( Errored <$> constructor "Errored" strictText )
  )
:}

-}
newtype UnionType a =
    UnionType
      ( Data.Functor.Compose.Compose (Dhall.Map.Map Text) Type a )
  deriving (Functor)

instance Data.Semigroup.Semigroup (UnionType a) where
    (<>) = coerce ((<>) :: Dhall.Map.Map Text (Type a) -> Dhall.Map.Map Text (Type a) -> Dhall.Map.Map Text (Type a))

instance Monoid (UnionType a) where
    mempty = coerce (mempty :: Dhall.Map.Map Text (Type a))
    mappend = (Data.Semigroup.<>)

-- | Run a 'UnionType' parser to build a 'Type' parser.
union :: UnionType a -> Type a
union (UnionType (Data.Functor.Compose.Compose mp)) = Type
    { extract  = extractF
    , expected = Union expect
    }
  where
    expect = (notEmptyRecord . Dhall.expected) <$> mp

    extractF e0 = do
      (fld, e1, rest) <- extractUnionConstructor e0

      t <- Dhall.Map.lookup fld mp

      guard $ Dhall.Core.Union rest `Dhall.Core.judgmentallyEqual`
                Dhall.Core.Union (Dhall.Map.delete fld expect)

      extract t e1

-- | Parse a single constructor of a union
constructor :: Text -> Type a -> UnionType a
constructor key valueType = UnionType
    ( Data.Functor.Compose.Compose (Dhall.Map.singleton key valueType) )

{-| The 'RecordInputType' divisible (contravariant) functor allows you to build
    an 'InputType' injector for a Dhall record.

    For example, let's take the following Haskell data type:

>>> :{
data Project = Project
  { projectName :: Text
  , projectDescription :: Text
  , projectStars :: Natural
  }
:}

    And assume that we have the following Dhall record that we would like to
    parse as a @Project@:

> { name =
>     "dhall-haskell"
> , description =
>     "A configuration language guaranteed to terminate"
> , stars =
>     289
> }

    Our injector has type 'InputType' @Project@, but we can't build that out of any
    smaller injectors, as 'InputType's cannot be combined (they are only 'Contravariant's).
    However, we can use an 'InputRecordType' to build an 'InputType' for @Project@:

>>> :{
injectProject :: InputType Project
injectProject =
  inputRecord
    ( adapt >$< inputFieldWith "name" inject
            >*< inputFieldWith "description" inject
            >*< inputFieldWith "stars" inject
    )
  where
    adapt (Project{..}) = (projectName, (projectDescription, projectStars))
:}

    Or, since we are simply using the `Inject` instance to inject each field, we could write

>>> :{
injectProject :: InputType Project
injectProject =
  inputRecord
    ( adapt >$< inputField "name"
            >*< inputField "description"
            >*< inputField "stars"
    )
  where
    adapt (Project{..}) = (projectName, (projectDescription, projectStars))
:}

-}

-- | Infix 'divided'
(>*<) :: Divisible f => f a -> f b -> f (a, b)
(>*<) = divided

infixr 5 >*<

newtype RecordInputType a
  = RecordInputType (Dhall.Map.Map Text (InputType a))

instance Contravariant RecordInputType where
  contramap f (RecordInputType inputTypeRecord) = RecordInputType $ contramap f <$> inputTypeRecord

instance Divisible RecordInputType where
  divide f (RecordInputType bInputTypeRecord) (RecordInputType cInputTypeRecord) =
      RecordInputType
    $ Dhall.Map.union
      ((contramap $ fst . f) <$> bInputTypeRecord)
      ((contramap $ snd . f) <$> cInputTypeRecord)
  conquer = RecordInputType mempty

inputFieldWith :: Text -> InputType a -> RecordInputType a
inputFieldWith name inputType = RecordInputType $ Dhall.Map.singleton name inputType

inputField :: Inject a => Text -> RecordInputType a
inputField name = inputFieldWith name inject

inputRecord :: RecordInputType a -> InputType a
inputRecord (RecordInputType inputTypeRecord) = InputType makeRecordLit recordType
  where
    recordType = Record $ declared <$> inputTypeRecord
    makeRecordLit x = RecordLit $ (($ x) . embed) <$> inputTypeRecord

{-| The 'UnionInputType' monoid allows you to build
    an 'InputType' injector for a Dhall record.

    For example, let's take the following Haskell data type:

>>> :{
data Status = Queued Natural
            | Result Text
            | Errored Text
:}

    And assume that we have the following Dhall union that we would like to
    parse as a @Status@:

> < Result = "Finish succesfully"
> | Queued : Natural
> | Errored : Text
> >

    Our injector has type 'InputType' @Status@, but we can't build that out of any
    smaller injectors, as 'InputType's cannot be combined.
    However, we can use an 'UnionInputType' to build an 'InputType' for @Status@:

>>> :{
injectStatus :: InputType Status
injectStatus = adapt >$< inputUnion
  (   inputConstructorWith "Queued"  inject
  >|< inputConstructorWith "Result"  inject
  >|< inputConstructorWith "Errored" inject
  )
  where
    adapt (Queued  n) = Left n
    adapt (Result  t) = Right (Left t)
    adapt (Errored e) = Right (Right e)
:}

    Or, since we are simply using the `Inject` instance to inject each branch, we could write

>>> :{
injectStatus :: InputType Status
injectStatus = adapt >$< inputUnion
  (   inputConstructor "Queued"
  >|< inputConstructor "Result"
  >|< inputConstructor "Errored"
  )
  where
    adapt (Queued  n) = Left n
    adapt (Result  t) = Right (Left t)
    adapt (Errored e) = Right (Right e)
:}

-}
newtype UnionInputType a =
  UnionInputType
    ( Data.Functor.Product.Product
        ( Control.Applicative.Const
            ( Dhall.Map.Map
                Text
                ( Expr Src X )
            )
        )
        ( Op (Text, Expr Src X) )
        a
    )
  deriving (Contravariant)

-- | Combines two 'UnionInputType' values.  See 'UnionInputType' for usage
-- notes.
--
-- Ideally, this matches 'Data.Functor.Contravariant.Divisible.chosen';
-- however, this allows 'UnionInputType' to not need a 'Divisible' instance
-- itself (since no instance is possible).
(>|<) :: UnionInputType a -> UnionInputType b -> UnionInputType (Either a b)
UnionInputType (Data.Functor.Product.Pair (Control.Applicative.Const mx) (Op fx))
    >|< UnionInputType (Data.Functor.Product.Pair (Control.Applicative.Const my) (Op fy)) =
    UnionInputType
      ( Data.Functor.Product.Pair
          ( Control.Applicative.Const (mx <> my) )
          ( Op (either fx fy) )
      )

infixr 5 >|<

inputUnion :: UnionInputType a -> InputType a
inputUnion ( UnionInputType ( Data.Functor.Product.Pair ( Control.Applicative.Const fields ) ( Op embedF ) ) ) =
    InputType
      { embed = \x ->
          let (name, y) = embedF x
          in  case notEmptyRecordLit y of
                  Nothing  -> Field (Union fields') name
                  Just val -> App (Field (Union fields') name) val
      , declared =
          Union fields'
      }
  where
    fields' = fmap notEmptyRecord fields

inputConstructorWith
    :: Text
    -> InputType a
    -> UnionInputType a
inputConstructorWith name inputType = UnionInputType $
    Data.Functor.Product.Pair
      ( Control.Applicative.Const
          ( Dhall.Map.singleton
              name
              ( declared inputType )
          )
      )
      ( Op ( (name,) . embed inputType )
      )

inputConstructor
    :: Inject a
    => Text
    -> UnionInputType a
inputConstructor name = inputConstructorWith name inject
