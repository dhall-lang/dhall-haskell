{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

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
    , interpretExpr
    , interpretExprWithSettings
    , fromExpr
    , fromExprWithSettings
    , rootDirectory
    , sourceName
    , startingContext
    , substitutions
    , normalizer
    , newManager
    , defaultInputSettings
    , InputSettings
    , defaultEvaluateSettings
    , EvaluateSettings
    , HasEvaluateSettings(..)
    , detailed

    -- * Decoders
    , module Dhall.Marshal.Decode

    -- * Encoders
    , module Dhall.Marshal.Encode

    -- * Individual phases
    , parseWithSettings
    , resolveWithSettings
    , typecheckWithSettings
    , expectWithSettings
    , normalizeWithSettings

    -- * Miscellaneous
    , rawInput
    ) where

import Control.Applicative    (Alternative, empty)
import Data.Either.Validation (Validation (..))
import Data.Void              (Void)
import Dhall.Import           (Imported (..))
import Dhall.Parser           (Src (..))
import Dhall.Syntax           (Expr (..), Import)
import Dhall.TypeCheck        (DetailedTypeError (..), TypeError)
import GHC.Generics
import Lens.Family            (LensLike', view)
import Prelude                hiding (maybe, sequence)
import System.FilePath        (takeDirectory)

import qualified Control.Exception
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text.IO
import qualified Dhall.Context
import qualified Dhall.Core                       as Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.Pretty.Internal
import qualified Dhall.Substitution
import qualified Dhall.TypeCheck
import qualified Lens.Family

import Dhall.Marshal.Decode
import Dhall.Marshal.Encode

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
  { _substitutions   :: Dhall.Substitution.Substitutions Src Void
  , _startingContext :: Dhall.Context.Context (Expr Src Void)
  , _normalizer      :: Maybe (Core.ReifiedNormalizer Void)
  , _newManager      :: IO Dhall.Import.Manager
  }

-- | Default evaluation settings: no extra entries in the initial
-- context, and no special normalizer behaviour.
--
-- @since 1.16
defaultEvaluateSettings :: EvaluateSettings
defaultEvaluateSettings = EvaluateSettings
  { _substitutions   = Dhall.Substitution.empty
  , _startingContext = Dhall.Context.empty
  , _normalizer      = Nothing
  , _newManager      = Dhall.Import.defaultNewManager
  }

-- | Access the starting context used for evaluation and type-checking.
--
-- @since 1.16
startingContext
  :: (Functor f, HasEvaluateSettings s)
  => LensLike' f s (Dhall.Context.Context (Expr Src Void))
startingContext = evaluateSettings . l
  where
    l :: (Functor f)
      => LensLike' f EvaluateSettings (Dhall.Context.Context (Expr Src Void))
    l k s = fmap (\x -> s { _startingContext = x}) (k (_startingContext s))

-- | Access the custom substitutions.
--
-- @since 1.30
substitutions
  :: (Functor f, HasEvaluateSettings s)
  => LensLike' f s (Dhall.Substitution.Substitutions Src Void)
substitutions = evaluateSettings . l
  where
    l :: (Functor f)
      => LensLike' f EvaluateSettings (Dhall.Substitution.Substitutions Src Void)
    l k s = fmap (\x -> s { _substitutions = x }) (k (_substitutions s))

-- | Access the custom normalizer.
--
-- @since 1.16
normalizer
  :: (Functor f, HasEvaluateSettings s)
  => LensLike' f s (Maybe (Core.ReifiedNormalizer Void))
normalizer = evaluateSettings . l
  where
    l :: (Functor f)
      => LensLike' f EvaluateSettings (Maybe (Core.ReifiedNormalizer Void))
    l k s = fmap (\x -> s { _normalizer = x }) (k (_normalizer s))

-- | Access the HTTP manager initializer.
--
-- @since 1.36
newManager
  :: (Functor f, HasEvaluateSettings s)
  => LensLike' f s (IO Dhall.Import.Manager)
newManager = evaluateSettings . l
  where
    l :: (Functor f)
      => LensLike' f EvaluateSettings (IO Dhall.Import.Manager)
    l k s = fmap (\x -> s { _newManager = x }) (k (_newManager s))

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

-- | Parse an expression, using the supplied `InputSettings`
parseWithSettings :: InputSettings -> Text -> IO (Expr Src Import)
parseWithSettings settings text = do
    Core.throws (Dhall.Parser.exprFromText (view sourceName settings) text)

-- | Type-check an expression, using the supplied `InputSettings`
typecheckWithSettings :: InputSettings -> Expr Src Void -> IO ()
typecheckWithSettings settings expression = do
    _ <- Core.throws (Dhall.TypeCheck.typeWith (view startingContext settings) expression)

    return ()

{-| Type-check an expression against a `Decoder`'s expected type, using the
    supplied `InputSettings`
-}
expectWithSettings :: InputSettings -> Decoder a -> Expr Src Void -> IO ()
expectWithSettings settings Decoder{..} expression = do
    expected' <- case expected of
        Success x -> return x
        Failure e -> Control.Exception.throwIO e

    let suffix = Dhall.Pretty.Internal.prettyToStrictText expected'

    let annotated = case expression of
            Note (Src begin end bytes) _ ->
                Note (Src begin end bytes') (Annot expression expected')
              where
                bytes' = bytes <> " : " <> suffix
            _ ->
                Annot expression expected'

    typecheckWithSettings settings annotated

    return ()

{-| Resolve an expression, using the supplied `InputSettings`

    Note that this also applies any substitutions specified in the
    `InputSettings`
-}
resolveWithSettings :: InputSettings -> Expr Src Import -> IO (Expr Src Void)
resolveWithSettings settings expression = do
    let InputSettings{..} = settings

    let EvaluateSettings{..} = _evaluateSettings

    let transform =
               Lens.Family.set Dhall.Import.substitutions   _substitutions
            .  Lens.Family.set Dhall.Import.normalizer      _normalizer
            .  Lens.Family.set Dhall.Import.startingContext _startingContext

    let status = transform (Dhall.Import.emptyStatusWithManager _newManager _rootDirectory)

    resolved <- State.evalStateT (Dhall.Import.loadWith expression) status

    pure (Dhall.Substitution.substitute resolved (view substitutions settings))

-- | Normalize an expression, using the supplied `InputSettings`
normalizeWithSettings :: InputSettings -> Expr Src Void -> Expr Src Void
normalizeWithSettings settings =
    Core.normalizeWith (view normalizer settings)

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
    :: Decoder a
    -- ^ The decoder for the Dhall value
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
    -> Decoder a
    -- ^ The decoder for the Dhall value
    -> Text
    -- ^ The Dhall program
    -> IO a
    -- ^ The decoded value in Haskell
inputWithSettings settings decoder@Decoder{..} text = do
    parsed <- parseWithSettings settings text

    resolved <- resolveWithSettings settings parsed

    expectWithSettings settings decoder resolved

    let normalized = normalizeWithSettings settings resolved

    case extract normalized of
        Success x -> return x
        Failure e -> Control.Exception.throwIO e

{-| Type-check and evaluate a Dhall program that is read from the
    file-system.

    This uses the settings from 'defaultEvaluateSettings'.

    @since 1.16
-}
inputFile
  :: Decoder a
  -- ^ The decoder for the Dhall value
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
  -> Decoder a
  -- ^ The decoder for the Dhall value
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
    -> IO (Expr Src Void)
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
    -> IO (Expr Src Void)
    -- ^ The fully normalized AST
inputExprWithSettings settings text = do
    parsed <- parseWithSettings settings text

    resolved <- resolveWithSettings settings parsed

    _ <- typecheckWithSettings settings resolved

    pure (Core.normalizeWith (view normalizer settings) resolved)

{-| Interpret a Dhall Expression

    This takes care of import resolution, type-checking, and normalization
-}
interpretExpr :: Expr Src Import -> IO (Expr Src Void)
interpretExpr = interpretExprWithSettings defaultInputSettings

-- | Like `interpretExpr`, but customizable using `InputSettings`
interpretExprWithSettings
    :: InputSettings -> Expr Src Import -> IO (Expr Src Void)
interpretExprWithSettings settings parsed = do
    resolved <- resolveWithSettings settings parsed

    typecheckWithSettings settings resolved

    pure (Core.normalizeWith (view normalizer settings) resolved)

{- | Decode a Dhall expression

    This takes care of import resolution, type-checking and normalization
-}
fromExpr :: Decoder a -> Expr Src Import -> IO a
fromExpr = fromExprWithSettings defaultInputSettings

-- | Like `fromExpr`, but customizable using `InputSettings`
fromExprWithSettings :: InputSettings -> Decoder a -> Expr Src Import -> IO a
fromExprWithSettings settings decoder@Decoder{..} expression = do
    resolved <- resolveWithSettings settings expression

    expectWithSettings settings decoder resolved

    let normalized = Core.normalizeWith (view normalizer settings) resolved

    case extract normalized of
        Success x -> return x
        Failure e -> Control.Exception.throwIO e

-- | Use this function to extract Haskell values directly from Dhall AST.
--   The intended use case is to allow easy extraction of Dhall values for
--   making the function `Core.normalizeWith` easier to use.
--
--   For other use cases, use `input` from "Dhall" module. It will give you
--   a much better user experience.
rawInput
    :: Alternative f
    => Decoder a
    -- ^ The decoder for the Dhall value
    -> Expr s Void
    -- ^ a closed form Dhall program, which evaluates to the expected type
    -> f a
    -- ^ The decoded value in Haskell
rawInput (Decoder {..}) expr =
    case extract (Core.normalize expr) of
        Success x  -> pure x
        Failure _e -> empty

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
    handler0 :: Imported (TypeError Src Void) -> IO a
    handler0 (Imported ps e) =
        Control.Exception.throwIO (Imported ps (DetailedTypeError e))

    handler1 :: TypeError Src Void -> IO a
    handler1 e = Control.Exception.throwIO (DetailedTypeError e)
