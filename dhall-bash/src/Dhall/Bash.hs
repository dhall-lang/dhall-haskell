{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}

{-| This library exports two utilities for compiling Dhall expressions to Bash:

    * `dhallToExpression`, which emits a Bash expression (i.e. a valid
      right-hand side for an assignment)

    * `dhallToStatement`, which emits a Bash @declare@ or @unset@ statement
      suitable for use with `eval`

    `dhallToExpression` only supports the conversion of primitive values, such
    as:

    * @Bool@ - which translates to a string that is either @"true"@ or @"false"@
    * @Natural@ - which translates to a Bash integer
    * @Integer@ - which translates to a Bash integer
    * @Text@ - which translates to a Bash string (properly escaped if necessary)

    The @dhall-to-bash@ executable by default tries to compile Dhall expressions
    to Bash expressions using the `dhallToExpression` function.  For example:

> $ dhall-to-bash <<< 'True'
> true
> $ dhall-to-bash <<< 'False'
> false
> $ dhall-to-bash <<< '1'
> 1
> $ dhall-to-bash <<< '+1'
> 1
> $ dhall-to-bash <<< '"ABC"'
> ABC
> $ dhall-to-bash <<< '" X "'
> $' X '
> $ dhall-to-bash <<< 'Natural/even +100'
> true

    The output of `dhallToExpression` is a valid Bash expression that can be
    embedded anywhere Bash expressions are valid, such as the right-hand side of
    an assignment statement:

> $ FOO=$(dhall-to-bash <<< 'List/length Integer [1, 2, 3]')
> $ echo "${FOO}"
> 3

    `dhallToStatement` supports a wider range of expressions by also adding
    support for:

    * @Optional@ - which translates to a variable which is either set or unset
    * @List@ - which translates to a Bash array
    * records - which translate to Bash associative arrays

    The @dhall-to-bash@ executable can emit a statement instead of an expression
    if you add the @--declare@ flag specifying which variable to set or unset.
    For example:

> $ dhall-to-bash --declare FOO <<< '[] : Optional Integer'
> unset FOO
> $ dhall-to-bash --declare FOO <<< '[1] : Optional Integer'
> declare -r -i FOO=1
> $ dhall-to-bash --declare FOO <<< '[[1] : Optional Integer] : Optional (Optional Integer)'
> declare -r -i FOO=1
> $ dhall-to-bash --declare FOO <<< '[[] : Optional Integer] : Optional (Optional Integer)'
> unset FOO
> $ dhall-to-bash --declare FOO <<< '[1, 2, 3]'
> declare -r -a FOO=(1 2 3)
> $ dhall-to-bash --declare FOO <<< '{ bar = 1, baz = True }'
> declare -r -A FOO=([bar]=1 [baz]=true)

    The output of `dhallToExpression` is either a @declare@ or @unset@ Bash
    statement that you can pass to @eval@:

> $ eval $(dhall-to-bash --declare FOO <<< '{ bar = 1, baz = True }')
> $ echo "${FOO[bar]}"
> 1
> $ echo "${FOO[baz]}"
> true

    @dhall-to-bash@ declares variables read-only (i.e. @-r@) to prevent you from
    accidentally overwriting, deleting or mutating variables:

> $ eval $(dist/build/dhall-to-bash/dhall-to-bash --declare BAR <<< '1')
> $ echo "${BAR"}
> 1
> $ unset BAR
> bash: unset: BAR: cannot unset: readonly variable
> $ eval $(dist/build/dhall-to-bash/dhall-to-bash --declare BAR <<< '2')
> bash: declare: BAR: readonly variable

-}

module Dhall.Bash (
    -- * Dhall to Bash
      dhallToExpression
    , dhallToStatement

    -- * Exceptions
    , ExpressionError(..)
    , StatementError(..)
    ) where

import Control.Exception (Exception)
import Data.Bifunctor (first)
import Data.ByteString
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Dhall.Core (Expr(..), Chunks(..))
import Dhall.TypeCheck

import qualified Data.Foldable
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Dhall.Core
import qualified Dhall.Map
import qualified NeatInterpolation
import qualified Text.ShellEscape

_ERROR :: Data.Text.Text
_ERROR = "\ESC[1;31mError\ESC[0m"

{-| This is the exception type for errors that might arise when translating
    Dhall expressions to Bash statements

    Because the majority of Dhall language features do not easily translate to
    Bash this just returns the expression that failed
-}
data StatementError
    = UnsupportedStatement (Expr X X)
    | UnsupportedSubexpression (Expr X X)
    deriving (Typeable)

instance Show StatementError where
    show (UnsupportedStatement e) =
        Data.Text.unpack [NeatInterpolation.text|
$_ERROR: Cannot translate to a Bash statement

Explanation: Only primitive values, records, ❰List❱s, and ❰Optional❱ values can
be translated from Dhall to a Bash statement

The following Dhall expression could not be translated to a Bash statement:

↳ $txt
|]
      where
        txt = Dhall.Core.pretty e

    show (UnsupportedSubexpression e) =
        -- Carefully note: No tip suggesting `--declare` since it won't work
        -- here (and the user is already using `--declare`)
        Data.Text.unpack [NeatInterpolation.text|
$_ERROR: Cannot translate to a Bash expression

Explanation: Only primitive values can be translated from Dhall to a Bash
expression

The following Dhall expression could not be translated to a Bash expression:

↳ $txt
|]
      where
        txt = Dhall.Core.pretty e

instance Exception StatementError

{-| This is the exception type for errors that might arise when translating
    Dhall expressions to Bash expressions

    Because the majority of Dhall language features do not easily translate to
    Bash this just returns the expression that failed
-}
data ExpressionError = UnsupportedExpression (Expr X X) deriving (Typeable)

instance Show ExpressionError where
    show (UnsupportedExpression e) =
        Data.Text.unpack [NeatInterpolation.text|
$_ERROR: Cannot translate to a Bash expression

Explanation: Only primitive values can be translated from Dhall to a Bash
expression

The following Dhall expression could not be translated to a Bash expression:

↳ $txt$tip
|]
      where
        txt = Dhall.Core.pretty e

        tip = case e of
            OptionalLit _ _ -> "\n\n" <> [NeatInterpolation.text|
Tip: You can convert an ❰Optional❱ value to a Bash statement using the --declare
flag
|]
            ListLit _ _ -> "\n\n" <> [NeatInterpolation.text|
Tip: You can convert a ❰List❱ to a Bash statement using the --declare flag
|]
            RecordLit _ -> "\n\n" <> [NeatInterpolation.text|
Tip: You can convert a record to a Bash statement using the --declare flag
|]
            _ -> ""

instance Exception ExpressionError

{-| Compile a Dhall expression to a Bash statement that @declare@s or @unset@s a
    a variable of your choice

    This only supports:

    * @Bool@s
    * @Natural@s
    * @Integer@s
    * @Text@s
    * @Optional@s
    * @List@s
    * records
 -}
dhallToStatement
    :: Expr s X
    -- ^ Dhall expression to compile
    -> ByteString
    -- ^ Variable to @declare@ or @unset@
    -> Either StatementError ByteString
    -- ^ Bash statement or compile failure
dhallToStatement expr0 var0 = go (Dhall.Core.normalize expr0)
  where
    var = Text.ShellEscape.bytes (Text.ShellEscape.bash var0)

    adapt (UnsupportedExpression e) = UnsupportedSubexpression e

    go (BoolLit a) = do
        go (TextLit (if a then "true" else "false"))
    go (NaturalLit a) = do
        go (IntegerLit (fromIntegral a))
    go (IntegerLit a) = do
        e <- first adapt (dhallToExpression (IntegerLit a))
        let bytes = "declare -r -i " <> var <> "=" <> e
        return bytes
    go (TextLit a) = do
        e <- first adapt (dhallToExpression (TextLit a))
        let bytes = "declare -r " <> var <> "=" <> e
        return bytes
    go (ListLit _ bs) = do
        bs' <- first adapt (mapM dhallToExpression bs)
        let bytes
                =   "declare -r -a "
                <>  var
                <>  "=("
                <>  Data.ByteString.intercalate " " (Data.Foldable.toList bs')
                <>  ")"
        return bytes
    go (OptionalLit _ bs) = do
        case bs of
            Nothing -> return ("unset " <> var)
            Just b  -> go b
    go (RecordLit a) = do
        let process (k, v) = do
                v' <- dhallToExpression v
                let bytes = Data.Text.Encoding.encodeUtf8 k
                let k'    = Text.ShellEscape.bytes (Text.ShellEscape.bash bytes)
                return ("[" <> k' <> "]=" <> v')
        kvs' <- first adapt (traverse process (Dhall.Map.toList a))
        let bytes
                =   "declare -r -A "
                <>  var
                <>  "=("
                <>  Data.ByteString.intercalate " " kvs'
                <>  ")"
        return bytes
    go e = Left (UnsupportedStatement e)

{-| Compile a Dhall expression to a Bash expression

    This only supports:

    * @Bool@s
    * @Natural@s
    * @Integer@s
    * @Text@s
 -}
dhallToExpression
    :: Expr s X
    -- ^ Dhall expression to compile
    -> Either ExpressionError ByteString
    -- ^ Bash expression or compile failure
dhallToExpression expr0 = go (Dhall.Core.normalize expr0)
  where
    go (BoolLit a) = do
        go (TextLit (if a then "true" else "false"))
    go (NaturalLit a) = do
        go (IntegerLit (fromIntegral a))
    go (IntegerLit a) = do
        go (TextLit (Chunks [] (Data.Text.pack (show a))))
    go (TextLit (Chunks [] a)) = do
        let bytes = Data.Text.Encoding.encodeUtf8 a
        return (Text.ShellEscape.bytes (Text.ShellEscape.bash bytes))
    go e = Left (UnsupportedExpression e)
