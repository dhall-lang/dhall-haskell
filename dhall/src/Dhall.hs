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
    , detailed

    -- ** Input settings
    , Dhall.Settings.InputSettings
    , Dhall.Settings.defaultInputSettings
    , Dhall.Settings.rootDirectory
    , Dhall.Settings.sourceName
    , Dhall.Settings.HasInputSettings(..)

    -- ** Evaluation settings
    , Dhall.Settings.EvaluateSettings
    , Dhall.Settings.defaultEvaluateSettings
    , Dhall.Settings.newManager
    , Dhall.Settings.normalizer
    , Dhall.Settings.startingContext
    , Dhall.Settings.substitutions
    , Dhall.Settings.HasEvaluateSettings(..)

    -- * Decoders
    , module Dhall.Marshal.Decode

    -- * Encoders
    , module Dhall.Marshal.Encode

    -- * Individual phases
    , parseWithSettings
    , resolveWithSettings
    , resolveAndStatusWithSettings
    , typecheckWithSettings
    , checkWithSettings
    , expectWithSettings
    , normalizeWithSettings

    -- * Miscellaneous
    , rawInput
    ) where

import Control.Applicative    (Alternative, empty)
import Control.Monad.Catch    (MonadThrow, throwM)
import Data.Either.Validation (Validation (..))
import Data.Void              (Void)
import Dhall.Import           (Imported (..), Status)
import Dhall.Parser           (Src (..))
import Dhall.Settings
    ( EvaluateSettings
    , HasEvaluateSettings
    , HasInputSettings
    , InputSettings
    , defaultEvaluateSettings
    , defaultInputSettings
    )
import Dhall.Syntax           (Expr (..), Import)
import Dhall.TypeCheck        (DetailedTypeError (..), TypeError)
import GHC.Generics
import Lens.Micro.Extras      (view)
import Prelude                hiding (maybe, sequence)
import System.FilePath        (takeDirectory)

import qualified Control.Exception
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text.IO
import qualified Dhall.Core                       as Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.Pretty.Internal
import qualified Dhall.Settings
import qualified Dhall.Substitution
import qualified Dhall.TypeCheck
import qualified Lens.Micro                       as Lens

import Dhall.Marshal.Decode
import Dhall.Marshal.Encode

--------------------------------------------------------------------------------
-- Individual phases
--------------------------------------------------------------------------------

-- | Parse an expression, using the supplied `InputSettings`
parseWithSettings
    :: (HasInputSettings s, MonadThrow m)
    => s -> Text -> m (Expr Src Import)
parseWithSettings settings text = do
    let sourceName = view Dhall.Settings.sourceName settings

    either throwM return (Dhall.Parser.exprFromText sourceName text)

-- | Type-check an expression, using the supplied `InputSettings`
typecheckWithSettings
    :: (HasEvaluateSettings s, MonadThrow m)
    => s -> Expr Src Void -> m ()
typecheckWithSettings settings expression = do
    let startingContext = view Dhall.Settings.startingContext settings

    either throwM (return . const ())
        (Dhall.TypeCheck.typeWith startingContext expression)

{-| Type-check an expression against a type provided as a Dhall expreession,
    using the supplied `InputSettings`
-}
checkWithSettings ::
    (HasEvaluateSettings s, MonadThrow m) =>
    -- | The input settings
    s ->
    -- | The expected type of the expression
    Expr Src Void ->
    -- | The expression to check
    Expr Src Void ->
    m ()
checkWithSettings settings type_ expression = do
    let suffix = Dhall.Pretty.Internal.prettyToStrictText type_

    let annotated = case expression of
            Note (Src begin end bytes) _ ->
                Note (Src begin end bytes') (Annot expression type_)
              where
                bytes' = bytes <> " : " <> suffix
            _ ->
                Annot expression type_

    typecheckWithSettings settings annotated

{-| Type-check an expression against a `Decoder`'s expected type, using the
    supplied `InputSettings`.
    This is equivalent of using the 'expected' type of a @Decoder@ as the second
    argument to 'checkWithSettings'.
-}
expectWithSettings
    :: (HasEvaluateSettings s, MonadThrow m)
    => s -> Decoder a -> Expr Src Void -> m ()
expectWithSettings settings Decoder{..} expression = do
    expected' <- case expected of
        Success x -> return x
        Failure e -> throwM e

    checkWithSettings settings expected' expression

{-| Resolve an expression, using the supplied `InputSettings`

    Note that this also applies any substitutions specified in the
    `InputSettings`
-}
resolveWithSettings
    :: (HasInputSettings s)
    => s -> Expr Src Import -> IO (Expr Src Void)
resolveWithSettings settings expression =
    fst <$> resolveAndStatusWithSettings settings expression

-- | A version of 'resolveWithSettings' that also returns the import 'Status'
-- together with the resolved expression.
resolveAndStatusWithSettings
    :: (HasInputSettings s)
    => s -> Expr Src Import -> IO (Expr Src Void, Status)
resolveAndStatusWithSettings settings expression = do
    let inputSettings = view Dhall.Settings.inputSettings settings

    let evaluateSettings = view Dhall.Settings.evaluateSettings inputSettings

    let rootDirectory = view Dhall.Settings.rootDirectory inputSettings

    let substitutions = view Dhall.Settings.substitutions evaluateSettings

    let status = Dhall.Import.emptyStatusWith evaluateSettings rootDirectory

    (resolved, status') <- State.runStateT (Dhall.Import.loadWith expression) status

    let substituted = Dhall.Substitution.substitute resolved substitutions

    pure (substituted, status')

-- | Normalize an expression, using the supplied `InputSettings`
normalizeWithSettings
    :: (HasEvaluateSettings s)
    => s -> Expr Src Void -> Expr Src Void
normalizeWithSettings settings =
    Core.normalizeWith (view Dhall.Settings.normalizer settings)

--------------------------------------------------------------------------------
-- High-level entrypoints
--------------------------------------------------------------------------------

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
  let inputSettings
        = Lens.set Dhall.Settings.evaluateSettings settings
        . Lens.set Dhall.Settings.rootDirectory (takeDirectory path)
        . Lens.set Dhall.Settings.sourceName path
        $ Dhall.Settings.defaultInputSettings
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

    let normalizer = view Dhall.Settings.normalizer settings

    pure (Core.normalizeWith normalizer resolved)

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

    let normalizer = view Dhall.Settings.normalizer settings

    pure (Core.normalizeWith normalizer resolved)

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

    let normalizer = view Dhall.Settings.normalizer settings

    let normalized = Core.normalizeWith normalizer resolved

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
