{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}

module Dhall.Nix where

import Control.Exception (Exception)
import Data.Foldable (toList)
import Data.Fix (Fix(..))
import Data.Typeable (Typeable)
import Dhall.Core (Expr(..), Var(..))
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

import qualified Data.Map
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector
import qualified Dhall.Core
import qualified NeatInterpolation

data CompileError
    = CannotReferenceShadowedVariable Var
    -- ^ Nix does not provide a way to reference a shadowed variable
    | NoDoubles Double
    -- ^ Nix does not provide a way to represent floating point values
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
        txt = Data.Text.Lazy.toStrict (Dhall.Core.pretty v)
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

_ERROR :: Data.Text.Text
_ERROR = "\ESC[1;31mError\ESC[0m"

instance Exception CompileError

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
        -- n - 1
        let e0 = Fix (NBinary NMinus "n" (Fix (NConstant (NInt 1))))
        -- naturalFold e0 succ
        let e1 = Fix (NApp (Fix (NApp "naturalFold" e0)) "succ")
        -- succ (e1 zero)
        let e2 = Fix (NApp "succ" (Fix (NApp e1 "zero")))
        -- n == 0
        let e3 = Fix (NBinary NLte "n" (Fix (NConstant (NInt 0))))
        -- succ : zero : if e3 then zero else e2
        let e4 = Fix (NAbs "succ" (Fix (NAbs "zero" (Fix (NIf e3 "zero" e2)))))
        -- n : t : e4
        let e5 = Fix (NAbs "n" (Fix (NAbs "t" e4)))
        -- let naturalFold = e5 in naturalFold
        return (Fix (NLet [NamedVar [StaticKey "naturalFold"] e5] "naturalFold"))
    loop NaturalBuild = do
        -- n + 1
        let e0 = Fix (NBinary NPlus "n" (Fix (NConstant (NInt 1))))
        -- k {} (n : e0)
        let e1 = Fix (NApp (Fix (NApp "k" (Fix (NSet [])))) (Fix (NAbs "n" e0)))
        -- k : e1 0
        return (Fix (NAbs "k" (Fix (NApp e1 (Fix (NConstant (NInt 0)))))))
    loop NaturalIsZero = do
        -- n == 0
        let e0 = Fix (NBinary NEq "n" (Fix (NConstant (NInt 0))))
        -- n : e0
        return (Fix (NAbs "n" e0))
    loop NaturalEven = do
        -- n - 2
        let e0 = Fix (NBinary NMinus "n" (Fix (NConstant (NInt 2))))
        -- naturalEven e0
        let e1 = Fix (NApp "naturalEven" e0)
        -- n != 1
        let e2 = Fix (NBinary NNEq "n" (Fix (NConstant (NInt 1))))
        -- n == 0
        let e3 = Fix (NBinary NEq "n" (Fix (NConstant (NInt 0))))
        -- e3 || (e2 && e1)
        let e4 = Fix (NBinary NOr e3 (Fix (NBinary NAnd e2 e1)))
        -- naturalEvent = n : e4
        let e5 = NamedVar [StaticKey "naturalEven"] (Fix (NAbs "n" e4))
        -- 0 - n
        let e6 = Fix (NBinary NMinus (Fix (NConstant (NInt 0))) "n")
        -- n <= 0
        let e7 = Fix (NBinary NLte "n" (Fix (NConstant (NInt 0))))
        -- n : naturalEven (if e7 then e6 else n)
        let e8 = Fix (NAbs "n" (Fix (NApp "naturalEven" (Fix (NIf e7 e6 "n")))))
        -- let e5 in e8
        return (Fix (NLet [e5] e8))
    loop NaturalOdd = do
        -- n - 2
        let e0 = Fix (NBinary NMinus "n" (Fix (NConstant (NInt 2))))
        -- naturalOdd e0
        let e1 = Fix (NApp "naturalOdd" e0)
        -- n != 0
        let e2 = Fix (NBinary NNEq "n" (Fix (NConstant (NInt 0))))
        -- n == 1
        let e3 = Fix (NBinary NEq "n" (Fix (NConstant (NInt 1))))
        -- e3 || (e2 && e1)
        let e4 = Fix (NBinary NOr e3 (Fix (NBinary NAnd e2 e1)))
        -- naturalOddt = n : e4
        let e5 = NamedVar [StaticKey "naturalOdd"] (Fix (NAbs "n" e4))
        -- 0 - n
        let e6 = Fix (NBinary NMinus (Fix (NConstant (NInt 0))) "n")
        -- n <= 0
        let e7 = Fix (NBinary NLte "n" (Fix (NConstant (NInt 0))))
        -- n : naturalOdd (if e7 then e6 else n)
        let e8 = Fix (NAbs "n" (Fix (NApp "naturalOdd" (Fix (NIf e7 e6 "n")))))
        -- let e5 in e8
        return (Fix (NLet [e5] e8))
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
    loop Double = return (Fix (NSet []))
    loop (DoubleLit n) = Left (NoDoubles n)
    loop Text = return (Fix (NSet []))
    loop (TextLit a) = do
        let a' = Data.Text.Lazy.toStrict (Data.Text.Lazy.Builder.toLazyText a)
        return (Fix (NStr (DoubleQuoted [Plain a'])))
    loop (TextAppend a b) = do
        a' <- loop a
        b' <- loop b
        return (Fix (NBinary NPlus a' b'))
    loop List = return (Fix (NAbs "t" (Fix (NSet []))))
    loop (ListLit _ bs) = do
        bs' <- mapM loop (toList bs)
        return (Fix (NList bs'))
    loop ListBuild = do
        -- k {}
        let e0 = Fix (NApp "k" (Fix (NSet [])))
        -- [x] ++ xs
        let e1 = Fix (NBinary NConcat (Fix (NList ["x"])) "xs")
        -- e0 (x : xs : e1)
        let e2 = Fix (NApp e0 (Fix (NAbs "x" (Fix (NAbs "xs" e1)))))
        -- k : e2 []
        let e3 = Fix (NAbs "k" (Fix (NApp e2 (Fix (NList [])))))
        -- t : e3
        return (Fix (NAbs "t" e3))
    loop ListFold = do
        -- f (cons y ys)
        let e0 = Fix (NApp "f" (Fix (NApp (Fix (NApp "cons" "y")) "ys")))
        -- f : y : ys : e0
        let e1 = Fix (NAbs "f" (Fix (NAbs "y" (Fix (NAbs "ys" e0)))))
        -- "builtins.foldl" e1
        let e2 = Fix (NApp "builtins.foldl'" e1)
        -- e2 (ys : ys) xs
        let e3 = Fix (NApp (Fix (NApp e2 (Fix (NAbs "ys" "ys")))) "xs")
        -- xs : t : cons : e3
        let e4 = Fix (NAbs "xs" (Fix (NAbs "t" (Fix (NAbs "cons" e3)))))
        -- t : e4
        return (Fix (NAbs "t" e4))
    loop ListLength = return (Fix (NAbs "t" "builtins.length"))
    loop ListHead = do
        -- builtins.head xs
        let e0 = Fix (NApp "builtins.head" "xs")
        -- xs == []
        let e1 = Fix (NBinary NEq "xs" (Fix (NList [])))
        -- xs : if e1 then null else e0
        let e2 = Fix (NAbs "xs" (Fix (NIf e1 (Fix (NConstant NNull)) e0)))
        -- t : e2
        return (Fix (NAbs "t" e2))
    loop ListLast = do
        -- builtins.length xs
        let e0 = Fix (NApp "builtins.length" "xs")
        -- e0 - 1
        let e1 = Fix (NBinary NMinus e0 (Fix (NConstant (NInt 1))))
        -- builtins.elemAt xs e1
        let e2 = Fix (NApp (Fix (NApp "builtins.elemAt" "xs")) e1)
        -- x == []
        let e3 = Fix (NBinary NEq "xs" (Fix (NList [])))
        -- xs : if e3 then null else e2
        let e4 = Fix (NAbs "xs" (Fix (NIf e3 (Fix (NConstant NNull)) e2)))
        -- t : e4
        return (Fix (NAbs "t" e4))
    loop ListIndexed = do
        -- builtins.length xs
        let e0 = Fix (NApp "builtins.length" "xs")
        -- builtins.elemAt xs i
        let e1 = Fix (NApp (Fix (NApp "builtins.elemAt" "xs")) "i")
        -- { index = i; value = e1; }
        let e2 =
                [ NamedVar [StaticKey "index"] "i"
                , NamedVar [StaticKey "value"] e1
                ]
        -- builtins.genList (i : e2)
        let e3 = Fix (NApp "builtins.genList" (Fix (NAbs "i" (Fix (NSet e2)))))
        -- t : xs : e3
        return (Fix (NAbs "t" (Fix (NAbs "xs" (Fix (NApp e3 e0))))))
    loop ListReverse = do
        -- n - i
        let e0 = Fix (NBinary NMinus "n" "i")
        -- n - 1
        let e1 = Fix (NBinary NMinus e0 (Fix (NConstant (NInt 1))))
        -- builtins.elemAt xs e1
        let e2 = Fix (NApp (Fix (NApp "builtins.elemAt" "xs")) e1)
        -- builtins.genList (i : e2)
        let e3 = Fix (NApp "builtins.genList" (Fix (NAbs "i" e2)))
        -- e3 n
        let e4 = Fix (NApp e3 "n")
        -- builtins.length xs
        let e5 = Fix (NApp "builtins.length" "xs")
        -- xs : let n = e5 in e4
        let e6 = Fix (NAbs "xs" (Fix (NLet [NamedVar [StaticKey "n"] e5] e4)))
        -- t : e6
        return (Fix (NAbs "t" e6))
    loop Optional = return (Fix (NAbs "t" (Fix (NSet []))))
    loop (OptionalLit _ b) =
        if Data.Vector.null b
            then return (Fix (NConstant NNull))
            else dhallToNix (Data.Vector.head b)
    loop OptionalFold = do
        -- x == null
        let e0 = Fix (NBinary NEq "x" (Fix (NConstant NNull)))
        -- if e0 then nothing else just x
        let e1 = Fix (NIf e0 "nothing" (Fix (NApp "just" "x")))
        -- t : just : nothing : e1
        let e2 = Fix (NAbs "t" (Fix (NAbs "just" (Fix (NAbs "nothing" e1)))))
        -- t : x : e2
        return (Fix (NAbs "t" (Fix (NAbs "x" e2))))
    loop (Record _) = return (Fix (NSet []))
    loop (RecordLit a) = do
        a' <- traverse dhallToNix a
        let a'' = do
                (k, v) <- Data.Map.toAscList a'
                let k' = Data.Text.Lazy.toStrict k
                return (NamedVar [StaticKey k'] v)
        return (Fix (NSet a''))
    loop (Union _) = return (Fix (NSet []))
    loop (UnionLit k v kts) = do
        v' <- dhallToNix v
        let k'   = Data.Text.Lazy.toStrict k
        let e0   = do
                k'' <- k : toList (Data.Map.keysSet kts)
                return (Data.Text.Lazy.toStrict k'', Nothing)
        let e1   = Data.Map.fromAscList e0
        let e2   = Fix (NApp (Fix (NSym k')) v')
        return (Fix (NAbs (ParamSet (FixedParamSet e1) Nothing) e2))
    loop (Combine a b) = do
        a' <- dhallToNix a
        b' <- dhallToNix b
        -- TODO: Do recursive update
        return (Fix (NBinary NUpdate a' b'))
    loop (Merge a b _) = do
        a' <- dhallToNix a
        b' <- dhallToNix b
        return (Fix (NApp b' a'))
    loop (Field a b) = do
        a' <- dhallToNix a
        let b' = Data.Text.Lazy.toStrict b
        return (Fix (NSelect a' [StaticKey b'] Nothing))
    loop (Note _ b) = loop b
    loop (Embed (X x)) = x
