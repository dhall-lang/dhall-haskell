{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TypeFamilies       #-}

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

> $ dhall-to-nix <<< "[] : Optional Integer"
> null

> $ dhall-to-nix <<< "[1] : Optional Integer"
> 1

    Unions are Church-encoded:

> $ dhall-to-nix <<< "< Left = True | Right : Natural >"
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
import Data.Foldable (toList)
import Data.Fix (Fix(..))
import Data.Traversable (for)
import Data.Typeable (Typeable)
import Dhall.Core (Chunks(..), Const(..), Expr(..), Var(..))
import Dhall.TypeCheck (X(..))
import Nix.Atoms (NAtom(..))
import Nix.Expr
    ( Antiquoted(..)
    , Binding(..)
    , NBinaryOp(..)
    , NExprF(..)
    , NKeyName(..)
    , NString(..)
    , Params(..)
    , (@@)
    , (==>)
    , ($+)
    )

import qualified Data.Text
import qualified Dhall.Core
import qualified Dhall.Map
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
dhallToNix :: Expr s X -> Either CompileError (Fix NExprF)
dhallToNix e = loop (Dhall.Core.normalize e)
  where
    loop (Const _) = return (Fix (NSet []))
    loop (Var (V a 0)) = return (Fix (NSym a))
    loop (Var  a     ) = Left (CannotReferenceShadowedVariable a)
    loop (Lam a _ c) = do
        c' <- loop c
        return (Fix (NAbs (Param a) c'))
    loop (Pi _ _ _) = return (Fix (NSet []))
    -- None needs a type to convert to an Optional
    loop (App None _) = do
      return (Fix (NConstant NNull))
    loop (App (Field (Union kts) k) v) = do
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
    loop (Let as b) = do
        as' <- for as $ \a -> do
          val <- loop $ Dhall.Core.value a
          pure $ NamedVar [StaticKey $ Dhall.Core.variable a] val Nix.nullPos
        b' <- loop b
        return (Fix (NLet (toList as') b'))
    loop (Annot a _) = loop a
    loop Bool = return (Fix (NSet []))
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
    loop Natural = return (Fix (NSet []))
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
        let e1 = Fix (NBinary NApp (Fix (NBinary NApp "k" (Fix (NSet [])))) (Fix (NAbs "n" e0)))
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
    loop NaturalShow = do
        return "toString"
    loop NaturalToInteger = do
        return (Fix (NAbs "n" "n"))
    loop (NaturalPlus a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NPlus a' b'))
    loop (NaturalTimes a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NMult a' b'))
    loop Integer = return (Fix (NSet []))
    loop (IntegerLit n) = return (Fix (NConstant (NInt (fromIntegral n))))
    loop IntegerShow = do
        let e0 = Fix (NBinary NApp "toString" "x")
        let e1 = Fix (NBinary NPlus (Fix (NStr "+")) e0)
        let e2 = Fix (NBinary NLte (Fix (NConstant (NInt 0))) "x")
        let e3 = Fix (NAbs "x" (Fix (NIf e2 e1 e0)))
        return e3
    loop IntegerToDouble = do
        return (Fix (NAbs "x" "x"))
    loop Double = return (Fix (NSet []))
    loop (DoubleLit n) = return (Fix (NConstant (NFloat (realToFrac n))))
    loop DoubleShow = do
        return "toString"
    loop Text = return (Fix (NSet []))
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

        let replaced = "builtins.replaceStrings" @@ from @@ to @@ "t"

        let quoted = Nix.mkStr "\"" $+ replaced $+ Nix.mkStr "\""

        return ("t" ==> quoted)
    loop List = return (Fix (NAbs "t" (Fix (NSet []))))
    loop (ListAppend a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NConcat a' b'))
    loop (ListLit _ bs) = do
        bs' <- mapM loop (toList bs)
        return (Fix (NList bs'))
    loop ListBuild = do
        let e0 = Fix (NBinary NApp "k" (Fix (NSet [])))
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
        let e3 = Fix (NBinary NApp "builtins.genList" (Fix (NAbs "i" (Fix (NSet e2)))))
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
    loop Optional = return (Fix (NAbs "t" (Fix (NSet []))))
    loop (Some a) = loop a
    loop None = return (Fix (NConstant NNull))
    loop OptionalFold = do
        let e0 = Fix (NBinary NEq "x" (Fix (NConstant NNull)))
        let e1 = Fix (NIf e0 "nothing" (Fix (NBinary NApp "just" "x")))
        let e2 = Fix (NAbs "t" (Fix (NAbs "just" (Fix (NAbs "nothing" e1)))))
        return (Fix (NAbs "t" (Fix (NAbs "x" e2))))
    loop OptionalBuild = do
        let e0 = Pi "nothing" "optional" "optional"
        let e1 = Pi "just" (Pi "_" "a" "optional") e0
        let e2 = Pi "optional" (Const Type) e1
        let e3 = App None "a"
        let e4 = Lam "x" "a" (Some "x")
        let e5 = App (App (App "f" (App Optional "a")) e4) e3
        loop (Lam "a" (Const Type) (Lam "f" e2 e5))
    loop (Record _) = return (Fix (NSet []))
    loop (RecordLit a) = do
        a' <- traverse loop a
        let a'' = do
                (k, v) <- Dhall.Map.toList a'
                return (NamedVar [StaticKey k] v Nix.nullPos)
        return (Fix (NSet a''))
    loop (Union _) = return (Fix (NSet []))
    loop (UnionLit k v kts) = do
        v' <- loop v
        let e0 = do
                k' <- k : Dhall.Map.keys kts
                return (k', Nothing)
        let e2 = Fix (NBinary NApp (Fix (NSym k)) v')
        return (Fix (NAbs (ParamSet e0 False Nothing) e2))
    loop (Combine a b) = do
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
                in  Fix (NList [Fix (NSet bindings)])

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
    loop (CombineTypes _ _) = return (Fix (NSet []))
    loop (Merge a b _) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NApp b' a'))
    loop (Prefer a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NUpdate a' b'))
    loop (Field (Union kts) k) =
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
    loop (Field a b) = do
        a' <- loop a
        return (Fix (NSelect a' [StaticKey b] Nothing))
    loop (Project a (Left b)) = do
        a' <- loop a
        let b' = fmap StaticKey (toList b)
        return (Fix (NSet [Inherit (Just a') b' Nix.nullPos]))
    loop (Project _ (Right _)) = do
        Left CannotProjectByType
    loop (ImportAlt a _) = loop a
    loop (Note _ b) = loop b
    loop (Embed (X x)) = x
