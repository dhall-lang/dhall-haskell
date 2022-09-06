{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}

{-| This library only exports a single `dhallToNix` function for translating a
    Dhall syntax tree to a Nix syntax tree for the @hnix@ library

    See the @dhall@ package if you would like to transform Dhall source code
    into a Dhall syntax tree.  Similarly, see the @hnix@ package if you would
    like to translate a Nix syntax tree into Nix source code.

    This package also provides a @dhall-to-nix@ executable which you can use to
    compile Dhall source code directly to Nix source code for your convenience.

    Any Dhall expression can be converted into an equivalent Nix expression.
    For example, Dhall records can be converted into Nix records:

> $ dhall-to-nix <<< "{ foo = 1, bar = True }"
> { bar = true; foo = 1; }

    ... and you can also convert Dhall functions to Nix functions, too:

> $ dhall-to-nix <<< "λ(x : Bool) → x == False"
> x: x == false

    Many Dhall expressions have a straightforward translation to Nix expressions
    but there are some translations that are not as obvious.  The following
    section documents these trickier conversions:

    First, all Dhall types translate to an empty record:

> $ dhall-to-nix <<< "Integer"
> {}

    Polymorphic Dhall functions translate to Nix functions that ignore their
    type argument:

> $ dhall-to-nix <<< "List/head"
> t: xs: if xs == []
>       then null
>       else builtins.head xs

    `Optional` values translate to @null@ if missing or the unwrapped value if
    present:

> $ dhall-to-nix <<< "None Natural"
> null

> $ dhall-to-nix <<< "Some 1"
> 1

    Unions are Church-encoded:

> $ dhall-to-nix <<< "< Left : Bool | Right : Natural >.Left True"
> { Left, Right }: Left true

    Also, all Dhall expressions are normalized before translation to Nix:

> $ dhall-to-nix <<< "True == False"
> false

    You can use the @dhall-to-nix@ executable within Nix to assemble Nix
    expressions from Dhall expressions using the following @dhallToNix@ utility
    function:

> dhallToNix = code :
>   let
>     file = builtins.toFile "dhall-expr" code;
>
>     drv = pkgs.stdenv.mkDerivation {
>       name = "dhall-expr-as-nix";
>
>       buildCommand = ''
>         dhall-to-nix <<< "${file}" > $out
>       '';
>
>       buildInputs = [ pkgs.haskellPackages.dhall-nix ];
>     };
>   in
>     import "${drv}";
-}

module Dhall.Nix (
    -- * Dhall to Nix
      dhallToNix

    -- * Exceptions
    , CompileError(..)
    ) where

import Control.Exception (Exception)
import Data.Fix (Fix (..))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)
import Lens.Family (toListOf)

import Dhall.Core
    ( Binding (..)
    , Chunks (..)
    , DhallDouble (..)
    , Expr (..)
    , FieldSelection (..)
    , FunctionBinding (..)
    , PreferAnnotation (..)
    , Var (..)
    , WithComponent (..)
    )
import Dhall.Parser  (Src)
import Dhall.Context (Context)

import Nix.Expr
    ( Antiquoted (..)
    , NExpr
    , NExprF (NStr, NSet)
    , NRecordType (NNonRecursive)
    , Binding (NamedVar)
    , NKeyName (..)
    , NString (..)
    , Params (Param)
    , ($!=)
    , ($&&)
    , ($*)
    , ($+)
    , ($++)
    , ($-)
    , ($/)
    , ($//)
    , ($<)
    , ($<=)
    , ($==)
    , ($==)
    , ($||)
    , (==>)
    , (@.)
    , (@@)
    )

import qualified Data.Text
import qualified Dhall.Context
import qualified Dhall.Core
import qualified Dhall.Map
import qualified Dhall.Optics
import qualified Dhall.Pretty
import qualified Dhall.TypeCheck
import qualified NeatInterpolation
import qualified Nix

{-| This is the exception type for all possible errors that might arise when
    translating the Dhall syntax tree to the Nix syntax tree
-}
data CompileError
    = CannotReferenceShadowedVariable Var
    -- ^ Nix does not provide a way to reference a shadowed variable
    | CannotProjectByType
    -- ^ We currently do not support threading around type information
    | CannotShowConstructor
    -- ^ We currently do not support the `showConstructor` keyword
    | CannotTypecheck Text
    -- ^ Typecheck failed in a merge expression
    deriving (Typeable)

instance Show CompileError where
    show (CannotReferenceShadowedVariable v) =
        Data.Text.unpack [NeatInterpolation.text|
$_ERROR: Cannot reference shadowed variable

Explanation: Whenever you introduce two variables of the same name, the latter
variable takes precedence:


                                  This ❰x❱ ...
                                  ⇩
    ┌───────────────────────────────┐
    │ λ(x : Text) → λ(x : Text) → x │
    └───────────────────────────────┘
                      ⇧
                      ... refers to this ❰x❱


The former variable is "shadowed":


    ┌───────────────────────────────┐
    │ λ(x : Text) → λ(x : Text) → x │
    └───────────────────────────────┘
        ⇧
        This ❰x❱ is shadowed


... and Dhall lets you reference shadowed variables using the ❰@❱ notation:


                                  This ❰x❱ ...
                                  ⇩
    ┌─────────────────────────────────┐
    │ λ(x : Text) → λ(x : Text) → x@1 │
    └─────────────────────────────────┘
        ⇧
        ... now refers to this ❰x❱


However, the Nix language does not let you reference shadowed variables and
there is nothing analogous to ❰@❱ in Nix

Your code contains the following expression:

↳ $txt

... which references a shadowed variable and therefore cannot be translated to
Nix
|]
      where
        txt = Dhall.Core.pretty v

    show CannotProjectByType =
        Data.Text.unpack [NeatInterpolation.text|
$_ERROR: Cannot project by type

The ❰dhall-to-nix❱ compiler does not support projecting out a subset of a record
by the expected type (i.e. ❰someRecord.(someType)❱ 
    |]

    show CannotShowConstructor =
        Data.Text.unpack [NeatInterpolation.text|
$_ERROR: Cannot translate the ❰showConstructor❱ keyword

The ❰dhall-to-nix❱ compiler does not support the ❰showConstructor❱ keyword.

In theory this keyword shouldn't need to be translated anyway since the keyword
doesn't survive β-normalization, so if you see this error message there might be
an internal error in ❰dhall-to-nix❱ that you should report.
    |]

    show (CannotTypecheck txt) =
        Data.Text.unpack [NeatInterpolation.text|

$_ERROR: The compiler reported an error during type-checking:

$txt
|]

_ERROR :: Data.Text.Text
_ERROR = "\ESC[1;31mError\ESC[0m"

instance Exception CompileError

{-| Convert a Dhall expression to the equivalent Nix expression

>>> :set -XOverloadedStrings
>>> dhallToNix (Lam "x" Natural (Lam "y" Natural (NaturalPlus "x" "y"))) 
Right (NAbs (Param "x") (NAbs (Param "y") (NBinary NPlus (NSym "x") (NSym "y"))))
>>> fmap Nix.Pretty.prettyNix it
Right x: y: x + y

    Precondition: You must first type-check the Dhall expression before passing
    the expression to `dhallToNix`
-}
dhallToNix :: Expr Src Void -> Either CompileError NExpr
dhallToNix e =
    loop (Dhall.Context.empty, rewriteShadowed (Dhall.Core.normalize e))
  where
    untranslatable = Nix.attrsE []

    -- This is an intermediate utility used to remove all occurrences of
    -- shadowing (since Nix does not support references to shadowed variables)
    --
    -- This finds how many bound variables of the same name that we need to
    -- descend past to reach the "deepest" reference to the current bound
    -- variable.  In other words, the result is the "depth" of the deepest
    -- reference.
    --
    -- If `Nothing` then the current bound variable doesn't need to be renamed.
    -- If any other number, then rename the variable to include the maximum
    -- depth.
    maximumDepth :: Var -> Expr s Void -> Maybe Int
    maximumDepth v@(V x n) (Lam _ FunctionBinding {functionBindingVariable = x', functionBindingAnnotation = a} b)
        | x == x' =
            max (maximumDepth v a) (fmap (+ 1) (maximumDepth (V x (n + 1)) b))
    maximumDepth v@(V x n) (Pi _ x' a b)
        | x == x' =
            max (maximumDepth v a) (fmap (+ 1) (maximumDepth (V x (n + 1)) b))
    maximumDepth (V x n) (Let (Binding { variable = x' }) a)
        | x == x' = fmap (+ 1) (maximumDepth (V x (n + 1)) a)
    maximumDepth v (Var v')
        | v == v' = Just 0
    maximumDepth v expression =
        foldr max Nothing
            (map
                (maximumDepth v)
                (toListOf Dhall.Core.subExpressions expression)
            )

    -- Higher-level utility that builds on top of `maximumDepth` to rename a
    -- variable if there are shadowed references to that variable
    rename :: (Text, Expr s Void) -> Maybe (Text, Expr s Void)
    rename (x, expression) =
        case maximumDepth (V x 0) expression of
            Nothing ->
                Nothing
            Just 0 ->
                Nothing
            Just n ->
                Just
                  ( x'
                  , Dhall.Core.subst (V x 0) (Var (V x' 0)) (Dhall.Core.shift 1 (V x' 0) expression)
                  )
              where
                x' = x <> Data.Text.pack (show n)

    renameShadowed :: Expr s Void -> Maybe (Expr s Void)
    renameShadowed (Lam cs FunctionBinding { functionBindingVariable = x, functionBindingAnnotation = a} b) = do
        (x', b') <- rename (x, b)

        return (Lam cs (Dhall.Core.makeFunctionBinding x' a) b')
    renameShadowed (Pi cs x a b) = do
        (x', b') <- rename (x, b)

        return (Pi cs x' a b')
    renameShadowed (Let Binding{ variable = x, .. } a) = do
        (x' , a') <- rename (x, a)

        return (Let Binding{ variable = x', .. } a')
    renameShadowed _ =
        Nothing

    mergeFuncFor :: Expr s Void -> NExpr -> NExpr -> NExpr
    mergeFuncFor (App Optional _) = \a -> \b -> Nix.mkIf (b $== Nix.mkNull) (a @. "None") ((a @. "Some") @@ b)
    mergeFuncFor _ = \a -> \b -> b @@ a

    -- Even higher-level utility that renames all shadowed references
    rewriteShadowed =
        Dhall.Optics.rewriteOf Dhall.Core.subExpressions renameShadowed

    loop :: (Context (Expr Src Void), Expr Src Void) -> Either CompileError NExpr
    loop (_, Const _) = return untranslatable
    loop (_, Var (V a 0)) = return (Nix.mkSym a)
    loop (_, Var  a     ) = Left (CannotReferenceShadowedVariable a)
    loop (ctx, Lam _ FunctionBinding { functionBindingVariable = a, functionBindingAnnotation = t } c) = do
        c' <- loop (Dhall.Context.insert a t ctx, c)
        return (Param a ==> c')
    loop (_, Pi _ _ _ _) = return untranslatable
    loop (_, App None _) =
      return Nix.mkNull
    loop (ctx, App (Field (Union kts) (Dhall.Core.fieldSelectionLabel -> k)) v) = do
        v' <- loop (ctx, v)
        let e0 = do
                k' <- Dhall.Map.keys kts
                return (k', Nothing)
        let e2 = Nix.mkSym k @@ v'
        return (Nix.mkParamset e0 False ==> e2)
    loop (ctx, App a b) = do
        a' <- loop (ctx, a)
        b' <- loop (ctx, b)
        return (a' @@ b')
    loop (ctx, Let (Binding { variable, value }) b) = do
        -- TODO: use multiLet
        case Dhall.TypeCheck.typeWith ctx value of
          Left err -> Left (CannotTypecheck (Dhall.Core.pretty err))
          Right ty -> do
            let newContext = Dhall.Context.insert variable ty ctx
            value' <- loop (ctx, value)
            b' <- loop (newContext, b)
            return (Nix.letE variable value' b')
    loop (ctx, Annot a _) = loop (ctx, a)
    loop (_, Bool) = return untranslatable
    loop (_, BoolLit b) = return (Nix.mkBool b)
    loop (ctx, BoolAnd a b) = do
        a' <- loop (ctx, a)
        b' <- loop (ctx, b)
        return (a' $&& b')
    loop (ctx, BoolOr a b) = do
        a' <- loop (ctx, a)
        b' <- loop (ctx, b)
        return (a' $|| b')
    loop (ctx, BoolEQ a b) = do
        a' <- loop (ctx, a)
        b' <- loop (ctx, b)
        return (a' $== b')
    loop (ctx, BoolNE a b) = do
        a' <- loop (ctx, a)
        b' <- loop (ctx, b)
        return (a' $!= b')
    loop (ctx, BoolIf a b c) = do
        a' <- loop (ctx, a)
        b' <- loop (ctx, b)
        c' <- loop (ctx, c)
        return (Nix.mkIf a' b' c')
    loop (_, Natural) = return untranslatable
    loop (_, NaturalLit n) = return (Nix.mkInt (fromIntegral n))
    loop (_, NaturalFold) = do
        let naturalFold =
                    "n"
                ==> "t"
                ==> "succ"
                ==> "zero"
                ==> Nix.mkIf ("n" $<= Nix.mkInt 0)
                        "zero"
                        (   "succ"
                        @@  (   "naturalFold"
                            @@  ("n" $- Nix.mkInt 1)
                            @@  "t"
                            @@  "succ"
                            @@  "zero"
                            )
                        )
        return (Nix.letsE [ ("naturalFold", naturalFold) ] "naturalFold")
    loop (_, NaturalBuild) = do
        return
            (   "k"
            ==> (   "k"
                @@  untranslatable
                @@  ("n" ==> ("n" $+ Nix.mkInt 1))
                @@  Nix.mkInt 0
                )
            )
    loop (_, NaturalIsZero) = do
        return ("n" ==> ("n" $== Nix.mkInt 0))
    loop (_, NaturalEven) = do
        return ("n" ==> ("n" $/ Nix.mkInt 2) $* Nix.mkInt 2 $== "n")
    loop (_, NaturalOdd) = do
        return ("n" ==> ("n" $/ Nix.mkInt 2) $* Nix.mkInt 2 $!= "n")
    loop (_, NaturalShow) =
        return "toString"
    loop (_, NaturalSubtract) = do
        return
            (   "x"
            ==> "y"
            ==> Nix.letE "z" ("y" $- "x")
                    (Nix.mkIf ("z" $< Nix.mkInt 0) (Nix.mkInt 0) "z")
            )
    loop (_, NaturalToInteger) =
        return ("n" ==> "n")
    loop (ctx, NaturalPlus a b) = do
        a' <- loop (ctx, a)
        b' <- loop (ctx, b)
        return (a' $+ b')
    loop (ctx, NaturalTimes a b) = do
        a' <- loop (ctx, a)
        b' <- loop (ctx, b)
        return (a' $* b')
    loop (_, Integer) = return untranslatable
    loop (_, IntegerLit n) = return (Nix.mkInt n)
    loop (_, IntegerClamp) = do
        return ("x" ==> Nix.mkIf (Nix.mkInt 0 $<= "x") "x" (Nix.mkInt 0))
    loop (_, IntegerNegate) = do
        return ("x" ==> (Nix.mkInt 0 $- "x"))
    loop (_, IntegerShow) = do
        let e0 = "toString" @@ "x"
        return ("x" ==> Nix.mkIf (Nix.mkInt 0 $<= "x") (Nix.mkStr "+" $+ e0) e0)
    loop (_, IntegerToDouble) =
        return ("x" ==> "x")
    loop (_, Double) = return untranslatable
    loop (_, DoubleLit (DhallDouble n)) = return (Nix.mkFloat (realToFrac n))
    loop (_, DoubleShow) =
        return "toString"
    loop (_, Text) = return untranslatable
    loop (ctx, TextLit (Chunks abs_ c)) = do
        let process (a, b) = do
                b' <- loop (ctx, b)
                return [Plain a, Antiquoted b']
        abs' <- mapM process abs_

        let chunks = concat abs' ++ [Plain c]
        return (Fix (NStr (DoubleQuoted chunks)))
    loop (ctx, TextAppend a b) = do
        a' <- loop (ctx, a)
        b' <- loop (ctx, b)
        return (a' $+ b')
    loop (_, TextReplace) = do
        let from = Nix.mkList [ "needle" ]

        let to = Nix.mkList [ "replacement" ]

        return
            (   "needle"
            ==> "replacement"
            ==> "haystack"
            ==> ("builtins" @. "replaceStrings" @@ from @@ to @@ "haystack")
            )
    loop (_, TextShow) = do
        let from =
                Nix.mkList
                    [ Nix.mkStr "\""
                    , Nix.mkStr "$"
                    , Nix.mkStr "\\"
                 -- Nix doesn't support \b and \f
                 -- , Nix.mkStr "\b"
                 -- , Nix.mkStr "\f"
                    , Nix.mkStr "\n"
                    , Nix.mkStr "\r"
                    , Nix.mkStr "\t"
                    ]

        let to =
                Nix.mkList
                    [ Nix.mkStr "\\\""
                    , Nix.mkStr "\\u0024"
                    , Nix.mkStr "\\\\"
                 -- , Nix.mkStr "\\b"
                 -- , Nix.mkStr "\\f"
                    , Nix.mkStr "\\n"
                    , Nix.mkStr "\\r"
                    , Nix.mkStr "\\t"
                    ]

        let replaced = "builtins" @. "replaceStrings" @@ from @@ to @@ "t"

        let quoted = Nix.mkStr "\"" $+ replaced $+ Nix.mkStr "\""

        return ("t" ==> quoted)
    loop (_, Date) = return untranslatable
    loop (_, Time) = return untranslatable
    loop (_, TimeZone) = return untranslatable
    loop (_, List) = return ("t" ==> untranslatable)
    loop (ctx, ListAppend a b) = do
        a' <- loop (ctx, a)
        b' <- loop (ctx, b)
        return (a' $++ b')
    loop (ctx, ListLit _ bs) = do
        bs' <- mapM (\v -> loop (ctx, v)) (toList bs)
        return (Nix.mkList bs')
    loop (_, ListBuild) = do
        return
            (   "t"
            ==> "k"
            ==> (   "k"
                @@  untranslatable
                @@  ("x" ==> "xs" ==> (Nix.mkList ["x"] $++ "xs"))
                @@  Nix.mkList []
                )
            )
    loop (_, ListFold) = do
        return
            (   "t"
            ==> "xs"
            ==> "t"
            ==> "cons"
            ==> (   "builtins.foldl'"
                @@  (   "f"
                    ==> "y"
                    ==> "ys"
                    ==> ("f" @@ ("cons" @@ "y" @@ "ys"))
                    )
                @@  ("ys" ==> "ys")
                @@  "xs"
                )
            )
    loop (_, ListLength) = return ("t" ==> "builtins.length")
    loop (_, ListHead) = do
        return
            (   "t"
            ==> "xs"
            ==> Nix.mkIf ("xs" $== Nix.mkList [])
                    Nix.mkNull
                    ("builtins.head" @@ "xs")
            )
    loop (_, ListLast) = do
        return
            (   "t"
            ==> "xs"
            ==> Nix.mkIf ("xs" $== Nix.mkList [])
                    Nix.mkNull
                    (   "builtins.elemAt"
                    @@ "xs"
                    @@ (("builtins.length" @@ "xs") $- Nix.mkInt 1)
                    )
            )
    loop (_, ListIndexed) = do
        return
            (   "t"
            ==> "xs"
            ==> (   "builtins.genList"
                @@  (   "i"
                    ==> Nix.attrsE
                            [ ("index", "i")
                            , ("value", "builtins.elemAt" @@ "xs" @@ "i")
                            ]
                    )
                @@  ("builtins.length" @@ "xs")
                )
            )
    loop (_, ListReverse) = do
        return
            (   "t"
            ==> "xs"
            ==> Nix.letE "n" ("builtins.length" @@ "xs")
                    (   "builtins.genList"
                    @@  (   "i"
                        ==> (   "builtins.elemAt"
                            @@  "xs"
                            @@  ("n" $- "i" $- Nix.mkInt 1)
                            )
                        )
                    @@  "n"
                    )
            )
    loop (_, Optional) = return ("t" ==> untranslatable)
    loop (ctx, Some a) = loop (ctx, a)
    loop (_, None) = return ("t" ==> Nix.mkNull)
    loop (ctx, t)
        | Just text <- Dhall.Pretty.temporalToText t = do
            loop (ctx, Dhall.Core.TextLit (Dhall.Core.Chunks [] text))
    -- The next three cases are not necessary, because they are handled by the
    -- previous case
    loop (_, DateLiteral{}) = undefined
    loop (_, TimeLiteral{}) = undefined
    loop (_, TimeZoneLiteral{}) = undefined
    loop (_, Record _) = return untranslatable
    loop (ctx, RecordLit a) = do
        a' <- traverse ((\v -> loop (ctx, v)) . Dhall.Core.recordFieldValue) a
        return (nixAttrs (Dhall.Map.toList a'))
      where
        -- nonrecursive attrset that uses correctly quoted keys
        -- see https://github.com/dhall-lang/dhall-haskell/issues/2414
        nixAttrs pairs =
          Fix $ NSet NNonRecursive $
          (\(key, val) -> NamedVar (DynamicKey (Plain (DoubleQuoted [Plain key])) :| []) val Nix.nullPos)
          <$> pairs
    loop (_, Union _) = return untranslatable
    loop (ctx, Combine _ _ a b) = do
        a' <- loop (ctx, a)
        b' <- loop (ctx, b)

        let defL = "builtins.hasAttr" @@ "k" @@ "kvsL"
        let defR = "builtins.hasAttr" @@ "k" @@ "kvsR"
        let valL = "builtins.getAttr" @@ "k" @@ "kvsL"
        let valR = "builtins.getAttr" @@ "k" @@ "kvsR"

        let toNameValue v =
                Nix.mkList [ Nix.attrsE [ ("name", "k"), ("value", v) ] ]

        let toKeyVals =
                    "k"
                ==> Nix.mkIf defL
                        (Nix.mkIf defR
                            (Nix.mkIf
                                (   ("builtins.isAttrs" @@ valL)
                                $&& ("builtins.isAttrs" @@ valR)
                                )
                                (toNameValue ("combine" @@ valL @@ valR))
                                (toNameValue valR)
                            )
                            (toNameValue valL)
                        )
                        (Nix.mkIf defR
                            (toNameValue valR)
                            (Nix.mkList [])
                        )

        return
            (Nix.letE "combine"
                (   "kvsL"
                ==> "kvsR"
                ==> Nix.letsE
                        [ ( "ks"
                          ,     ("builtins.attrNames" @@ "kvsL")
                            $++ ("builtins.attrNames" @@ "kvsR")
                          )
                        , ("toKeyVals", toKeyVals)
                        ]
                        (   "builtins.listToAttrs"
                        @@  (   "builtins.concatLists"
                            @@  ("map" @@ "toKeyVals" @@ "ks")
                            )
                        )
                )
                ("combine" @@ a' @@ b')
            )
    loop (_, CombineTypes _ _ _) = return untranslatable
    loop (ctx, Merge a b _) = do
        a' <- loop (ctx, a)
        b' <- loop (ctx, b)
        case Dhall.TypeCheck.typeWith ctx b of
          Left err -> Left (CannotTypecheck (Dhall.Core.pretty err))
          Right ty -> return (mergeFuncFor (Dhall.Core.normalize ty) a' b')
    loop (ctx, ToMap a _) = do
        a' <- loop (ctx, a)
        return
            (Nix.letE "kvs" a'
                (   "map"
                @@  (   "k"
                    ==> Nix.attrsE
                            [ ("mapKey", "k")
                            , ("mapValue", "builtins.getAttr" @@ "k" @@ "kvs")
                            ]
                    )
                @@  ("builtins.attrNames" @@ "kvs")
                )
            )
    loop (_, ShowConstructor _) = do
        Left CannotShowConstructor
    loop (ctx, Prefer _ _ b c) = do
        b' <- loop (ctx, b)
        c' <- loop (ctx, c)
        return (b' $// c')
    loop (ctx, RecordCompletion a b) =
        loop (ctx, Annot (Prefer mempty PreferFromCompletion (Field a def) b) (Field a typ))
      where
        def = Dhall.Core.makeFieldSelection "default"
        typ = Dhall.Core.makeFieldSelection "Type"
    loop (_, Field (Union kts) (Dhall.Core.fieldSelectionLabel -> k)) =
        case Dhall.Map.lookup k kts of
            -- If the selected alternative has an associated payload, then we
            -- need introduce the partial application through an extra abstraction
            -- (here "x").
            --
            -- This translates `< Foo : T >.Foo` to `x: { Foo }: Foo x`
            Just (Just _) -> do
                let e0 = do
                        k' <- Dhall.Map.keys kts
                        return (k', Nothing)
                return ("x" ==> Nix.mkParamset e0 False ==> (Nix.mkSym k @@ "x"))

            _ -> do
                let e0 = do
                        k' <- Dhall.Map.keys kts
                        return (k', Nothing)
                return (Nix.mkParamset e0 False ==> Nix.mkSym k)
    loop (ctx, Field a (Dhall.Core.fieldSelectionLabel -> b)) = do
        a' <- loop (ctx, a)
        return (a' @. b)
    loop (ctx, Project a (Left b)) = do
        a' <- loop (ctx, a)
        let b' = fmap StaticKey (toList b)
        return (Nix.mkNonRecSet [ Nix.inheritFrom a' b' Nix.nullPos ])
    loop (_, Project _ (Right _)) =
        Left CannotProjectByType
    loop (_, Assert _) =
        return untranslatable
    loop (_, Equivalent _ _ _) =
        return untranslatable
    loop (ctx, With a (WithLabel k :| []) b) = do
        a' <- loop (ctx, a)
        b' <- loop (ctx, b)

        return (a' $// Nix.attrsE [(k, b')])
    loop (ctx, With a (WithLabel k :| k' : ks) b) = do
        a' <- loop (ctx, a)
        b' <- loop (ctx, With (Field "_" (FieldSelection Nothing k Nothing)) (k' :| ks) (Dhall.Core.shift 1 "_" b))

        return (Nix.letE "_" a' ("_" $// Nix.attrsE [(k, b')]))
    loop (ctx, With a (WithQuestion :| []) b) = do
        a' <- loop (ctx, a)
        b' <- loop (ctx, b)
        return (Nix.mkIf (a' $== Nix.mkNull) Nix.mkNull b')
    loop (ctx, With a (WithQuestion :| k : ks) b) = do
        a' <- loop (ctx, a)
        b' <- loop (ctx, With "_" (k :| ks) (Dhall.Core.shift 1 "_" b))
        return (Nix.letE "_" a' (Nix.mkIf (a' $== Nix.mkNull) Nix.mkNull b'))
    loop (ctx, ImportAlt a _) = loop (ctx, a)
    loop (ctx, Note _ b) = loop (ctx, b)
    loop (_, Embed x) = absurd x
