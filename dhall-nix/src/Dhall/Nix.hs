{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
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
import Data.Fix          (Fix (..))
import Data.Foldable     (toList)
import Data.Text         (Text)
import Data.Traversable  (for)
import Data.Typeable     (Typeable)
import Data.Void         (Void, absurd)
import Dhall.Core
    ( Binding (..)
    , Chunks (..)
    , DhallDouble (..)
    , Expr (..)
    , FunctionBinding (..)
    , MultiLet (..)
    , PreferAnnotation (..)
    , Var (..)
    )
import Lens.Family       (toListOf)
import Nix.Atoms         (NAtom (..))
import Nix.Expr
    ( Antiquoted (..)
    , Binding (..)
    , NBinaryOp (..)
    , NExprF (..)
    , NKeyName (..)
    , NRecordType (..)
    , NString (..)
    , Params (..)
    , ($+)
    , (==>)
    , (@@)
    , (@.)
    )

import qualified Data.Text
import qualified Dhall.Core
import qualified Dhall.Map
import qualified Dhall.Optics
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
dhallToNix :: Expr s Void -> Either CompileError (Fix NExprF)
dhallToNix e =
    loop (rewriteShadowed (Dhall.Core.normalize e))
  where
    untranslatable = Fix (NSet NNonRecursive [])

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

    -- Even higher-level utility that renames all shadowed references
    rewriteShadowed =
        Dhall.Optics.rewriteOf Dhall.Core.subExpressions renameShadowed

    loop (Const _) = return untranslatable
    loop (Var (V a 0)) = return (Fix (NSym a))
    loop (Var  a     ) = Left (CannotReferenceShadowedVariable a)
    loop (Lam _ FunctionBinding { functionBindingVariable = a } c) = do
        c' <- loop c
        return (Fix (NAbs (Param a) c'))
    loop (Pi _ _ _ _) = return untranslatable
    -- None needs a type to convert to an Optional
    loop (App None _) =
      return (Fix (NConstant NNull))
    loop (App (Field (Union kts) (Dhall.Core.fieldSelectionLabel -> k)) v) = do
        v' <- loop v
        let e0 = do
                k' <- Dhall.Map.keys kts
                return (k', Nothing)
        let e2 = Fix (NBinary NApp (Fix (NSym k)) v')
        return (Fix (NAbs (ParamSet e0 False Nothing) e2))
    loop (App a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NApp a' b'))
    loop (Let a0 b0) = do
        let MultiLet as b = Dhall.Core.multiLet a0 b0
        as' <- for as $ \a -> do
          val <- loop $ Dhall.Core.value a
          pure $ NamedVar [StaticKey $ Dhall.Core.variable a] val Nix.nullPos
        b' <- loop b
        return (Fix (NLet (toList as') b'))
    loop (Annot a _) = loop a
    loop Bool = return untranslatable
    loop (BoolLit b) = return (Fix (NConstant (NBool b)))
    loop (BoolAnd a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NAnd a' b'))
    loop (BoolOr a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NOr a' b'))
    loop (BoolEQ a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NEq a' b'))
    loop (BoolNE a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NNEq a' b'))
    loop (BoolIf a b c) = do
        a' <- loop a
        b' <- loop b
        c' <- loop c
        return (Fix (NIf a' b' c'))
    loop Natural = return untranslatable
    loop (NaturalLit n) = return (Fix (NConstant (NInt (fromIntegral n))))
    loop NaturalFold = do
        let e0 = Fix (NBinary NMinus "n" (Fix (NConstant (NInt 1))))
        let e1 = Fix (NBinary NApp (Fix (NBinary NApp (Fix (NBinary NApp "naturalFold" e0)) "t")) "succ")
        let e2 = Fix (NBinary NApp "succ" (Fix (NBinary NApp e1 "zero")))
        let e3 = Fix (NBinary NLte "n" (Fix (NConstant (NInt 0))))
        let e4 = Fix (NAbs "succ" (Fix (NAbs "zero" (Fix (NIf e3 "zero" e2)))))
        let e5 = Fix (NAbs "n" (Fix (NAbs "t" e4)))
        return (Fix (NLet [NamedVar ["naturalFold"] e5 Nix.nullPos] "naturalFold"))
    loop NaturalBuild = do
        let e0 = Fix (NBinary NPlus "n" (Fix (NConstant (NInt 1))))
        let e1 = Fix (NBinary NApp (Fix (NBinary NApp "k" untranslatable)) (Fix (NAbs "n" e0)))
        return (Fix (NAbs "k" (Fix (NBinary NApp e1 (Fix (NConstant (NInt 0)))))))
    loop NaturalIsZero = do
        let e0 = Fix (NBinary NEq "n" (Fix (NConstant (NInt 0))))
        return (Fix (NAbs "n" e0))
    loop NaturalEven = do
        let e0 = Fix (NBinary NMinus "n" (Fix (NConstant (NInt 2))))
        let e1 = Fix (NBinary NApp "naturalEven" e0)
        let e2 = Fix (NBinary NNEq "n" (Fix (NConstant (NInt 1))))
        let e3 = Fix (NBinary NEq "n" (Fix (NConstant (NInt 0))))
        let e4 = Fix (NBinary NOr e3 (Fix (NBinary NAnd e2 e1)))
        let e5 = NamedVar ["naturalEven"] (Fix (NAbs "n" e4)) Nix.nullPos
        let e6 = Fix (NBinary NMinus (Fix (NConstant (NInt 0))) "n")
        let e7 = Fix (NBinary NLte "n" (Fix (NConstant (NInt 0))))
        let e8 = Fix (NAbs "n" (Fix (NBinary NApp "naturalEven" (Fix (NIf e7 e6 "n")))))
        return (Fix (NLet [e5] e8))
    loop NaturalOdd = do
        let e0 = Fix (NBinary NMinus "n" (Fix (NConstant (NInt 2))))
        let e1 = Fix (NBinary NApp "naturalOdd" e0)
        let e2 = Fix (NBinary NNEq "n" (Fix (NConstant (NInt 0))))
        let e3 = Fix (NBinary NEq "n" (Fix (NConstant (NInt 1))))
        let e4 = Fix (NBinary NOr e3 (Fix (NBinary NAnd e2 e1)))
        let e5 = NamedVar ["naturalOdd"] (Fix (NAbs "n" e4)) Nix.nullPos
        let e6 = Fix (NBinary NMinus (Fix (NConstant (NInt 0))) "n")
        let e7 = Fix (NBinary NLte "n" (Fix (NConstant (NInt 0))))
        let e8 = Fix (NAbs "n" (Fix (NBinary NApp "naturalOdd" (Fix (NIf e7 e6 "n")))))
        return (Fix (NLet [e5] e8))
    loop NaturalShow =
        return "toString"
    loop NaturalSubtract = do
        let e0 = NamedVar ["z"] (Fix (NBinary NMinus "y" "x")) Nix.nullPos
        let e1 = Fix (NBinary NLt "z" (Fix (NConstant (NInt 0))))
        let e2 = Fix (NConstant (NInt 0))
        let e3 = "z"
        let e4 = Fix (NIf e1 e2 e3)
        let e5 = Fix (NLet [e0] e4)
        return (Fix (NAbs "x" (Fix (NAbs "y" e5))))
    loop NaturalToInteger =
        return (Fix (NAbs "n" "n"))
    loop (NaturalPlus a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NPlus a' b'))
    loop (NaturalTimes a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NMult a' b'))
    loop Integer = return untranslatable
    loop (IntegerLit n) = return (Fix (NConstant (NInt (fromIntegral n))))
    loop IntegerClamp = do
        let e0 = Fix (NConstant (NInt 0))
        let e1 = Fix (NBinary NLte (Fix (NConstant (NInt 0))) "x")
        let e2 = Fix (NAbs "x" (Fix (NIf e1 "x" e0)))
        return e2
    loop IntegerNegate = do
        let e0 = Fix (NBinary NMinus (Fix (NConstant (NInt 0))) "x")
        let e1 = Fix (NAbs "x" e0)
        return e1
    loop IntegerShow = do
        let e0 = Fix (NBinary NApp "toString" "x")
        let e1 = Fix (NBinary NPlus (Fix (NStr "+")) e0)
        let e2 = Fix (NBinary NLte (Fix (NConstant (NInt 0))) "x")
        let e3 = Fix (NAbs "x" (Fix (NIf e2 e1 e0)))
        return e3
    loop IntegerToDouble =
        return (Fix (NAbs "x" "x"))
    loop Double = return untranslatable
    loop (DoubleLit (DhallDouble n)) = return (Fix (NConstant (NFloat (realToFrac n))))
    loop DoubleShow =
        return "toString"
    loop Text = return untranslatable
    loop (TextLit (Chunks abs_ c)) = do
        let process (a, b) = do
                b' <- loop b
                return [Plain a, Antiquoted b']
        abs' <- mapM process abs_

        let chunks = concat abs' ++ [Plain c]
        return (Fix (NStr (DoubleQuoted chunks)))
    loop (TextAppend a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NPlus a' b'))
    loop TextReplace = do
        let from = Nix.mkList [ "needle" ]

        let to = Nix.mkList [ "replacement" ]

        return
            (   "needle"
            ==> "replacement"
            ==> "haystack"
            ==> ("builtins" @. "replaceStrings" @@ from @@ to @@ "haystack")
            )
    loop TextShow = do
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
    loop List = return (Fix (NAbs "t" untranslatable))
    loop (ListAppend a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NConcat a' b'))
    loop (ListLit _ bs) = do
        bs' <- mapM loop (toList bs)
        return (Fix (NList bs'))
    loop ListBuild = do
        let e0 = Fix (NBinary NApp "k" untranslatable)
        let e1 = Fix (NBinary NConcat (Fix (NList ["x"])) "xs")
        let e2 = Fix (NBinary NApp e0 (Fix (NAbs "x" (Fix (NAbs "xs" e1)))))
        let e3 = Fix (NAbs "k" (Fix (NBinary NApp e2 (Fix (NList [])))))
        return (Fix (NAbs "t" e3))
    loop ListFold = do
        let e0 = Fix (NBinary NApp "f" (Fix (NBinary NApp (Fix (NBinary NApp "cons" "y")) "ys")))
        let e1 = Fix (NAbs "f" (Fix (NAbs "y" (Fix (NAbs "ys" e0)))))
        let e2 = Fix (NBinary NApp "builtins.foldl'" e1)
        let e3 = Fix (NBinary NApp (Fix (NBinary NApp e2 (Fix (NAbs "ys" "ys")))) "xs")
        let e4 = Fix (NAbs "xs" (Fix (NAbs "t" (Fix (NAbs "cons" e3)))))
        return (Fix (NAbs "t" e4))
    loop ListLength = return (Fix (NAbs "t" "builtins.length"))
    loop ListHead = do
        let e0 = Fix (NBinary NApp "builtins.head" "xs")
        let e1 = Fix (NBinary NEq "xs" (Fix (NList [])))
        let e2 = Fix (NAbs "xs" (Fix (NIf e1 (Fix (NConstant NNull)) e0)))
        return (Fix (NAbs "t" e2))
    loop ListLast = do
        let e0 = Fix (NBinary NApp "builtins.length" "xs")
        let e1 = Fix (NBinary NMinus e0 (Fix (NConstant (NInt 1))))
        let e2 = Fix (NBinary NApp (Fix (NBinary NApp "builtins.elemAt" "xs")) e1)
        let e3 = Fix (NBinary NEq "xs" (Fix (NList [])))
        let e4 = Fix (NAbs "xs" (Fix (NIf e3 (Fix (NConstant NNull)) e2)))
        return (Fix (NAbs "t" e4))
    loop ListIndexed = do
        let e0 = Fix (NBinary NApp "builtins.length" "xs")
        let e1 = Fix (NBinary NApp (Fix (NBinary NApp "builtins.elemAt" "xs")) "i")
        let e2 =
                [ NamedVar ["index"] "i" Nix.nullPos
                , NamedVar ["value"] e1  Nix.nullPos
                ]
        let e3 = Fix (NBinary NApp "builtins.genList" (Fix (NAbs "i" (Fix (NSet NNonRecursive e2)))))
        return (Fix (NAbs "t" (Fix (NAbs "xs" (Fix (NBinary NApp e3 e0))))))
    loop ListReverse = do
        let e0 = Fix (NBinary NMinus "n" "i")
        let e1 = Fix (NBinary NMinus e0 (Fix (NConstant (NInt 1))))
        let e2 = Fix (NBinary NApp (Fix (NBinary NApp "builtins.elemAt" "xs")) e1)
        let e3 = Fix (NBinary NApp "builtins.genList" (Fix (NAbs "i" e2)))
        let e4 = Fix (NBinary NApp e3 "n")
        let e5 = Fix (NBinary NApp "builtins.length" "xs")
        let e6 = Fix (NAbs "xs" (Fix (NLet [NamedVar ["n"] e5 Nix.nullPos] e4)))
        return (Fix (NAbs "t" e6))
    loop Optional = return (Fix (NAbs "t" untranslatable))
    loop (Some a) = loop a
    loop None = return (Fix (NConstant NNull))
    loop (Record _) = return untranslatable
    loop (RecordLit a) = do
        a' <- traverse (loop . Dhall.Core.recordFieldValue) a
        let a'' = do
                (k, v) <- Dhall.Map.toList a'
                return (NamedVar [StaticKey k] v Nix.nullPos)
        return (Fix (NSet NNonRecursive a''))
    loop (Union _) = return untranslatable
    loop (Combine _ _ a b) = do
        a' <- loop a
        b' <- loop b
        let e0 = Fix (NBinary NApp (Fix (NBinary NApp "map" "toKeyVals")) "ks")
        let e1 = Fix (NBinary NApp "builtins.concatLists" e0)
        let e2 = Fix (NBinary NApp "builtins.listToAttrs" e1)

        let defL = Fix (NBinary NApp (Fix (NBinary NApp "builtins.hasAttr" "k")) "kvsL")
        let defR = Fix (NBinary NApp (Fix (NBinary NApp "builtins.hasAttr" "k")) "kvsR")
        let valL = Fix (NBinary NApp (Fix (NBinary NApp "builtins.getAttr" "k")) "kvsL")
        let valR = Fix (NBinary NApp (Fix (NBinary NApp "builtins.getAttr" "k")) "kvsR")

        let empty_ = Fix (NList [])
        let toNameValue v =
                let bindings =
                        [ NamedVar ["name" ] "k" Nix.nullPos
                        , NamedVar ["value"] v   Nix.nullPos
                        ]
                in  Fix (NList [Fix (NSet NNonRecursive bindings)])

        let e3 = Fix (NBinary NApp (Fix (NBinary NApp "combine" valL)) valR)
        let e4 = Fix (NBinary NApp "builtins.isAttrs" valL)
        let e5 = Fix (NBinary NApp "builtins.isAttrs" valR)
        let e6 = Fix (NBinary NAnd e4 e5)
        let e7 = Fix (NIf e6 (toNameValue e3) (toNameValue valR))
        let e8 = Fix (NIf defR e7 (toNameValue valL))
        let e9 = Fix (NIf defR (toNameValue valR) empty_)
        let toKeyVals = Fix (NAbs "k" (Fix (NIf defL e8 e9)))

        let ksL = Fix (NBinary NApp "builtins.attrNames" "kvsL")
        let ksR = Fix (NBinary NApp "builtins.attrNames" "kvsR")
        let ks  = Fix (NBinary NConcat ksL ksR)

        let e10 =
                [ NamedVar ["ks"       ] ks        Nix.nullPos
                , NamedVar ["toKeyVals"] toKeyVals Nix.nullPos
                ]
        let combine = Fix (NAbs "kvsL" (Fix (NAbs "kvsR" (Fix (NLet e10 e2)))))

        let e11 = Fix (NBinary NApp (Fix (NBinary NApp "combine" a')) b')
        return (Fix (NLet [NamedVar ["combine"] combine Nix.nullPos] e11))
    loop (CombineTypes _ _ _) = return untranslatable
    loop (Merge a b _) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NApp b' a'))
    loop (ToMap a _) = do
        a' <- loop a
        let ks = Fix (NBinary NApp "builtins.attrNames" "kvs")
        let v = Fix (NBinary NApp (Fix (NBinary NApp "builtins.getAttr" "k")) "kvs")
        let setBindings =
                [ NamedVar [StaticKey "mapKey"] "k" Nix.nullPos
                , NamedVar [StaticKey "mapValue"] v Nix.nullPos
                ]
        let map_ = Fix (NBinary NApp "map" (Fix (NAbs "k" (Fix (NSet NNonRecursive setBindings)))))
        let toMap = Fix (NAbs "kvs" (Fix (NBinary NApp map_ ks)))
        return (Fix (NBinary NApp toMap a'))
    loop (Prefer _ _ b c) = do
        b' <- loop b
        c' <- loop c
        return (Fix (NBinary NUpdate b' c'))
    loop (RecordCompletion a b) =
        loop (Annot (Prefer mempty PreferFromCompletion (Field a def) b) (Field a typ))
      where
        def = Dhall.Core.makeFieldSelection "default"
        typ = Dhall.Core.makeFieldSelection "Type"
    loop (Field (Union kts) (Dhall.Core.fieldSelectionLabel -> k)) =
        case Dhall.Map.lookup k kts of
            -- If the selected alternative has an associated payload, then we
            -- need introduce the partial application through an extra abstraction
            -- (here "x").
            --
            -- This translates `< Foo : T >.Foo` to `x: { Foo }: Foo x`
            Just ( Just _ ) -> do
                let e0 = do
                        k' <- Dhall.Map.keys kts
                        return (k', Nothing)
                let e2 = Fix (NBinary NApp (Fix (NSym k)) (Fix (NSym "x")))
                return (Fix (NAbs (Param "x") (Fix (NAbs (ParamSet e0 False Nothing) e2))))

            _ -> do
                let e0 = do
                        k' <- Dhall.Map.keys kts
                        return (k', Nothing)
                let e2 = Fix (NSym k)
                return (Fix (NAbs (ParamSet e0 False Nothing) e2))
    loop (Field a (Dhall.Core.fieldSelectionLabel -> b)) = do
        a' <- loop a
        return (Fix (NSelect a' [StaticKey b] Nothing))
    loop (Project a (Left b)) = do
        a' <- loop a
        let b' = fmap StaticKey (toList b)
        return (Fix (NSet NNonRecursive [Inherit (Just a') b' Nix.nullPos]))
    loop (Project _ (Right _)) =
        Left CannotProjectByType
    loop (Assert _) =
        return untranslatable
    loop (Equivalent _ _ _) =
        return untranslatable
    loop a@With{} =
        loop (Dhall.Core.desugarWith a)
    loop (ImportAlt a _) = loop a
    loop (Note _ b) = loop b
    loop (Embed x) = absurd x
