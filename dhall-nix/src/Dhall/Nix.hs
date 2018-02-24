{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}

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

    `Dhall.Core.Double`s cannot be translated to Nix at all since Nix does not
    support floating point values.

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
import Data.Scientific (Scientific)
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
    , ParamSet(..)
    )

import qualified Data.HashMap.Strict.InsOrd
import qualified Data.Map
import qualified Data.Text
import qualified Data.Text.Buildable
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector
import qualified Dhall.Core
import qualified NeatInterpolation

{-| This is the exception type for all possible errors that might arise when
    translating the Dhall syntax tree to the Nix syntax tree
-}
data CompileError
    = CannotReferenceShadowedVariable Var
    -- ^ Nix does not provide a way to reference a shadowed variable
    | NoDoubles Scientific
    -- ^ Nix does not provide a way to represent floating point values
    | UnexpectedConstructorsKeyword
    -- ^ The @constructors@ keyword is not yet supported
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
        builder = Data.Text.Buildable.build v

        txt =
            Data.Text.Lazy.toStrict (Data.Text.Lazy.Builder.toLazyText builder)

    show (NoDoubles n) =
        Data.Text.unpack [NeatInterpolation.text|
$_ERROR: No doubles

Explanation: Dhall values of type ❰Double❱ cannot be converted to Nix
expressions because the Nix language provides no way to represent floating point
values

You provided the following value:

↳ $txt

... which cannot be translated to Nix
|]
      where
        txt = Data.Text.pack (show n)

    show UnexpectedConstructorsKeyword =
        Data.Text.unpack [NeatInterpolation.text|
$_ERROR: Unexpected ❰constructors❱ keyword

Explanation: The dhallToNix Haskell API function has a precondition that the
Dhall expression to translate to Nix must have already been type-checked.  This
precondition ensures that the normalized expression will have no remaining
❰constructors❱ keywords.

However, the dhallToNix Haskell API function was called with a Dhall expression
that still had a ❰constructors❱ keyword, which indicates that the expression had
not yet been type-checked.
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
    loop (Var (V a 0)) = return (Fix (NSym (Data.Text.Lazy.toStrict a)))
    loop (Var  a     ) = Left (CannotReferenceShadowedVariable a)
    loop (Lam a _ c) = do
        let a' = Data.Text.Lazy.toStrict a
        c' <- loop c
        return (Fix (NAbs (Param a') c'))
    loop (Pi _ _ _) = return (Fix (NSet []))
    loop (App a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NApp a' b'))
    loop (Let a _ c d) = do
        let a' = Data.Text.Lazy.toStrict a
        c' <- loop c
        d' <- loop d
        return (Fix (NLet [NamedVar [StaticKey a'] c'] d'))
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
        let e1 = Fix (NApp (Fix (NApp (Fix (NApp "naturalFold" e0)) "t")) "succ")
        let e2 = Fix (NApp "succ" (Fix (NApp e1 "zero")))
        let e3 = Fix (NBinary NLte "n" (Fix (NConstant (NInt 0))))
        let e4 = Fix (NAbs "succ" (Fix (NAbs "zero" (Fix (NIf e3 "zero" e2)))))
        let e5 = Fix (NAbs "n" (Fix (NAbs "t" e4)))
        return (Fix (NLet [NamedVar ["naturalFold"] e5] "naturalFold"))
    loop NaturalBuild = do
        let e0 = Fix (NBinary NPlus "n" (Fix (NConstant (NInt 1))))
        let e1 = Fix (NApp (Fix (NApp "k" (Fix (NSet [])))) (Fix (NAbs "n" e0)))
        return (Fix (NAbs "k" (Fix (NApp e1 (Fix (NConstant (NInt 0)))))))
    loop NaturalIsZero = do
        let e0 = Fix (NBinary NEq "n" (Fix (NConstant (NInt 0))))
        return (Fix (NAbs "n" e0))
    loop NaturalEven = do
        let e0 = Fix (NBinary NMinus "n" (Fix (NConstant (NInt 2))))
        let e1 = Fix (NApp "naturalEven" e0)
        let e2 = Fix (NBinary NNEq "n" (Fix (NConstant (NInt 1))))
        let e3 = Fix (NBinary NEq "n" (Fix (NConstant (NInt 0))))
        let e4 = Fix (NBinary NOr e3 (Fix (NBinary NAnd e2 e1)))
        let e5 = NamedVar ["naturalEven"] (Fix (NAbs "n" e4))
        let e6 = Fix (NBinary NMinus (Fix (NConstant (NInt 0))) "n")
        let e7 = Fix (NBinary NLte "n" (Fix (NConstant (NInt 0))))
        let e8 = Fix (NAbs "n" (Fix (NApp "naturalEven" (Fix (NIf e7 e6 "n")))))
        return (Fix (NLet [e5] e8))
    loop NaturalOdd = do
        let e0 = Fix (NBinary NMinus "n" (Fix (NConstant (NInt 2))))
        let e1 = Fix (NApp "naturalOdd" e0)
        let e2 = Fix (NBinary NNEq "n" (Fix (NConstant (NInt 0))))
        let e3 = Fix (NBinary NEq "n" (Fix (NConstant (NInt 1))))
        let e4 = Fix (NBinary NOr e3 (Fix (NBinary NAnd e2 e1)))
        let e5 = NamedVar ["naturalOdd"] (Fix (NAbs "n" e4))
        let e6 = Fix (NBinary NMinus (Fix (NConstant (NInt 0))) "n")
        let e7 = Fix (NBinary NLte "n" (Fix (NConstant (NInt 0))))
        let e8 = Fix (NAbs "n" (Fix (NApp "naturalOdd" (Fix (NIf e7 e6 "n")))))
        return (Fix (NLet [e5] e8))
    loop NaturalShow = do
        let e0 = Fix (NApp "toString" "x")
        return (Fix (NAbs "x" (Fix (NBinary NPlus (Fix (NStr "+")) e0))))
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
        return "toString"
    loop Double = return (Fix (NSet []))
    loop (DoubleLit n) = Left (NoDoubles n)
    loop DoubleShow = do
        return "toString"
    loop Text = return (Fix (NSet []))
    loop (TextLit (Chunks abs_ c)) = do
        let process (a, b) = do
                let text = Data.Text.Lazy.Builder.toLazyText a
                let a'   = Data.Text.Lazy.toStrict text
                b' <- loop b
                return [Plain a', Antiquoted b']
        abs' <- mapM process abs_

        let text = Data.Text.Lazy.Builder.toLazyText c
        let c'   = Data.Text.Lazy.toStrict text

        let chunks = concat abs' ++ [Plain c']
        return (Fix (NStr (DoubleQuoted chunks)))
    loop (TextAppend a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NPlus a' b'))
    loop List = return (Fix (NAbs "t" (Fix (NSet []))))
    loop (ListAppend a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NConcat a' b'))
    loop (ListLit _ bs) = do
        bs' <- mapM loop (toList bs)
        return (Fix (NList bs'))
    loop ListBuild = do
        let e0 = Fix (NApp "k" (Fix (NSet [])))
        let e1 = Fix (NBinary NConcat (Fix (NList ["x"])) "xs")
        let e2 = Fix (NApp e0 (Fix (NAbs "x" (Fix (NAbs "xs" e1)))))
        let e3 = Fix (NAbs "k" (Fix (NApp e2 (Fix (NList [])))))
        return (Fix (NAbs "t" e3))
    loop ListFold = do
        let e0 = Fix (NApp "f" (Fix (NApp (Fix (NApp "cons" "y")) "ys")))
        let e1 = Fix (NAbs "f" (Fix (NAbs "y" (Fix (NAbs "ys" e0)))))
        let e2 = Fix (NApp "builtins.foldl'" e1)
        let e3 = Fix (NApp (Fix (NApp e2 (Fix (NAbs "ys" "ys")))) "xs")
        let e4 = Fix (NAbs "xs" (Fix (NAbs "t" (Fix (NAbs "cons" e3)))))
        return (Fix (NAbs "t" e4))
    loop ListLength = return (Fix (NAbs "t" "builtins.length"))
    loop ListHead = do
        let e0 = Fix (NApp "builtins.head" "xs")
        let e1 = Fix (NBinary NEq "xs" (Fix (NList [])))
        let e2 = Fix (NAbs "xs" (Fix (NIf e1 (Fix (NConstant NNull)) e0)))
        return (Fix (NAbs "t" e2))
    loop ListLast = do
        let e0 = Fix (NApp "builtins.length" "xs")
        let e1 = Fix (NBinary NMinus e0 (Fix (NConstant (NInt 1))))
        let e2 = Fix (NApp (Fix (NApp "builtins.elemAt" "xs")) e1)
        let e3 = Fix (NBinary NEq "xs" (Fix (NList [])))
        let e4 = Fix (NAbs "xs" (Fix (NIf e3 (Fix (NConstant NNull)) e2)))
        return (Fix (NAbs "t" e4))
    loop ListIndexed = do
        let e0 = Fix (NApp "builtins.length" "xs")
        let e1 = Fix (NApp (Fix (NApp "builtins.elemAt" "xs")) "i")
        let e2 =
                [ NamedVar ["index"] "i"
                , NamedVar ["value"] e1
                ]
        let e3 = Fix (NApp "builtins.genList" (Fix (NAbs "i" (Fix (NSet e2)))))
        return (Fix (NAbs "t" (Fix (NAbs "xs" (Fix (NApp e3 e0))))))
    loop ListReverse = do
        let e0 = Fix (NBinary NMinus "n" "i")
        let e1 = Fix (NBinary NMinus e0 (Fix (NConstant (NInt 1))))
        let e2 = Fix (NApp (Fix (NApp "builtins.elemAt" "xs")) e1)
        let e3 = Fix (NApp "builtins.genList" (Fix (NAbs "i" e2)))
        let e4 = Fix (NApp e3 "n")
        let e5 = Fix (NApp "builtins.length" "xs")
        let e6 = Fix (NAbs "xs" (Fix (NLet [NamedVar ["n"] e5] e4)))
        return (Fix (NAbs "t" e6))
    loop Optional = return (Fix (NAbs "t" (Fix (NSet []))))
    loop (OptionalLit _ b) =
        if Data.Vector.null b
            then return (Fix (NConstant NNull))
            else loop (Data.Vector.head b)
    loop OptionalFold = do
        let e0 = Fix (NBinary NEq "x" (Fix (NConstant NNull)))
        let e1 = Fix (NIf e0 "nothing" (Fix (NApp "just" "x")))
        let e2 = Fix (NAbs "t" (Fix (NAbs "just" (Fix (NAbs "nothing" e1)))))
        return (Fix (NAbs "t" (Fix (NAbs "x" e2))))
    loop OptionalBuild = do
        let e0 = Pi "nothing" "optional" "optional"
        let e1 = Pi "just" (Pi "_" "a" "optional") e0
        let e2 = Pi "optional" (Const Type) e1
        let e3 = OptionalLit "a" Data.Vector.empty
        let e4 = Lam "x" "a" (OptionalLit "a" (Data.Vector.singleton "x"))
        let e5 = App (App (App "f" (App Optional "a")) e4) e3
        loop (Lam "a" (Const Type) (Lam "f" e2 e5))
    loop (Record _) = return (Fix (NSet []))
    loop (RecordLit a) = do
        a' <- traverse loop a
        let a'' = do
                (k, v) <- Data.HashMap.Strict.InsOrd.toList a'
                let k' = Data.Text.Lazy.toStrict k
                return (NamedVar [StaticKey k'] v)
        return (Fix (NSet a''))
    loop (Union _) = return (Fix (NSet []))
    loop (UnionLit k v kts) = do
        v' <- loop v
        let k'   = Data.Text.Lazy.toStrict k
        let e0   = do
                k'' <- k : Data.HashMap.Strict.InsOrd.keys kts
                return (Data.Text.Lazy.toStrict k'', Nothing)
        let e1   = Data.Map.fromAscList e0
        let e2   = Fix (NApp (Fix (NSym k')) v')
        return (Fix (NAbs (ParamSet (FixedParamSet e1) Nothing) e2))
    loop (Combine a b) = do
        a' <- loop a
        b' <- loop b
        let e0 = Fix (NApp (Fix (NApp "map" "toKeyVals")) "ks")
        let e1 = Fix (NApp "builtins.concatLists" e0)
        let e2 = Fix (NApp "builtins.listToAttrs" e1)

        let defL = Fix (NApp (Fix (NApp "builtins.hasAttr" "k")) "kvsL")
        let defR = Fix (NApp (Fix (NApp "builtins.hasAttr" "k")) "kvsR")
        let valL = Fix (NApp (Fix (NApp "builtins.getAttr" "k")) "kvsL")
        let valR = Fix (NApp (Fix (NApp "builtins.getAttr" "k")) "kvsR")

        let empty = Fix (NList [])
        let toNameValue v =
                let bindings =
                        [ NamedVar ["name" ] "k"
                        , NamedVar ["value"] v
                        ]
                in  Fix (NList [Fix (NSet bindings)])

        let e3 = Fix (NApp (Fix (NApp "combine" valL)) valR)
        let e4 = Fix (NApp "builtins.isAttrs" valL)
        let e5 = Fix (NApp "builtins.isAttrs" valR)
        let e6 = Fix (NBinary NAnd e4 e5)
        let e7 = Fix (NIf e6 (toNameValue e3) (toNameValue valR))
        let e8 = Fix (NIf defR e7 (toNameValue valL))
        let e9 = Fix (NIf defR (toNameValue valR) empty)
        let toKeyVals = Fix (NAbs "k" (Fix (NIf defL e8 e9)))

        let ksL = Fix (NApp "builtins.attrNames" "kvsL")
        let ksR = Fix (NApp "builtins.attrNames" "kvsR")
        let ks  = Fix (NBinary NConcat ksL ksR)

        let e10 =
                [ NamedVar ["ks"       ] ks
                , NamedVar ["toKeyVals"] toKeyVals
                ]
        let combine = Fix (NAbs "kvsL" (Fix (NAbs "kvsR" (Fix (NLet e10 e2)))))

        let e11 = Fix (NApp (Fix (NApp "combine" a')) b')
        return (Fix (NLet [NamedVar ["combine"] combine] e11))
    loop (Merge a b _) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NApp b' a'))
    loop (Constructors _) = do
        Left UnexpectedConstructorsKeyword
    loop (Prefer a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NUpdate a' b'))
    loop (Field a b) = do
        a' <- loop a
        let b' = Data.Text.Lazy.toStrict b
        return (Fix (NSelect a' [StaticKey b'] Nothing))
    loop (Note _ b) = loop b
    loop (Embed (X x)) = x
