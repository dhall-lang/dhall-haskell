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
import Data.Traversable (for)
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
    , MultiLet (..)
    , PreferAnnotation (..)
    , Var (..)
    , WithComponent (..)
    )

import Nix.Expr
    ( Antiquoted (..)
    , NExpr
    , NExprF (NStr, NSet)
    , Recursivity (NonRecursive)
    , Binding (NamedVar)
    , NKeyName (..)
    , NString (..)
    , Params (Param)
    , VarName(..)
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
import qualified Dhall.Core
import qualified Dhall.Map
import qualified Dhall.Optics
import qualified Dhall.Pretty
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
    | BytesUnsupported
    -- ^ The Nix language does not support arbitrary bytes (most notably: null
    --   bytes)
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

    show BytesUnsupported =
        Data.Text.unpack [NeatInterpolation.text|
$_ERROR: Cannot translate ❰Bytes❱ to Nix

Explanation: The Nix language does not support bytes literals
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
dhallToNix :: Expr s Void -> Either CompileError NExpr
dhallToNix e =
    loop (rewriteShadowed (Dhall.Core.normalize e))
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

    -- Even higher-level utility that renames all shadowed references
    rewriteShadowed =
        Dhall.Optics.rewriteOf Dhall.Core.subExpressions renameShadowed

    loop (Const _) = return untranslatable
    loop (Var (V a 0)) = return (Nix.mkSym a)
    loop (Var  a     ) = Left (CannotReferenceShadowedVariable a)
    loop (Lam _ FunctionBinding { functionBindingVariable = a } c) = do
        c' <- loop c
        return (Param (VarName a) ==> c')
    loop (Pi _ _ _ _) = return untranslatable
    loop (App None _) =
      return Nix.mkNull
    loop (App (Field (Union kts) (Dhall.Core.fieldSelectionLabel -> k)) v) = do
        v' <- loop v
        let e0 = do
                k' <- Dhall.Map.keys kts
                return (k', Nothing)
        let e2 = Nix.mkSym k @@ v'
        return (Nix.mkParamset e0 False ==> e2)
    loop (App a b) = do
        a' <- loop a
        b' <- loop b
        return (a' @@ b')
    loop (Let a0 b0) = do
        let MultiLet bindings b = Dhall.Core.multiLet a0 b0
        bindings' <- for bindings $ \Binding{ variable, value } -> do
          value' <- loop value
          pure (variable, value')
        b' <- loop b
        return (Nix.letsE (toList bindings') b')
    loop (Annot a _) = loop a
    loop Bool = return untranslatable
    loop (BoolLit b) = return (Nix.mkBool b)
    loop (BoolAnd a b) = do
        a' <- loop a
        b' <- loop b
        return (a' $&& b')
    loop (BoolOr a b) = do
        a' <- loop a
        b' <- loop b
        return (a' $|| b')
    loop (BoolEQ a b) = do
        a' <- loop a
        b' <- loop b
        return (a' $== b')
    loop (BoolNE a b) = do
        a' <- loop a
        b' <- loop b
        return (a' $!= b')
    loop (BoolIf a b c) = do
        a' <- loop a
        b' <- loop b
        c' <- loop c
        return (Nix.mkIf a' b' c')
    loop Bytes = return untranslatable
    loop (BytesLit _) = do
        Left BytesUnsupported
    loop Natural = return untranslatable
    loop (NaturalLit n) = return (Nix.mkInt (fromIntegral n))
    loop NaturalFold = do
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
    loop NaturalBuild = do
        return
            (   "k"
            ==> (   "k"
                @@  untranslatable
                @@  ("n" ==> ("n" $+ Nix.mkInt 1))
                @@  Nix.mkInt 0
                )
            )
    loop NaturalIsZero = do
        return ("n" ==> ("n" $== Nix.mkInt 0))
    loop NaturalEven = do
        return ("n" ==> ("n" $/ Nix.mkInt 2) $* Nix.mkInt 2 $== "n")
    loop NaturalOdd = do
        return ("n" ==> ("n" $/ Nix.mkInt 2) $* Nix.mkInt 2 $!= "n")
    loop NaturalShow =
        return "toString"
    loop NaturalSubtract = do
        return
            (   "x"
            ==> "y"
            ==> Nix.letE "z" ("y" $- "x")
                    (Nix.mkIf ("z" $< Nix.mkInt 0) (Nix.mkInt 0) "z")
            )
    loop NaturalToInteger =
        return ("n" ==> "n")
    loop (NaturalPlus a b) = do
        a' <- loop a
        b' <- loop b
        return (a' $+ b')
    loop (NaturalTimes a b) = do
        a' <- loop a
        b' <- loop b
        return (a' $* b')
    loop Integer = return untranslatable
    loop (IntegerLit n) = return (Nix.mkInt n)
    loop IntegerClamp = do
        return ("x" ==> Nix.mkIf (Nix.mkInt 0 $<= "x") "x" (Nix.mkInt 0))
    loop IntegerNegate = do
        return ("x" ==> (Nix.mkInt 0 $- "x"))
    loop IntegerShow = do
        let e0 = "toString" @@ "x"
        return ("x" ==> Nix.mkIf (Nix.mkInt 0 $<= "x") (Nix.mkStr "+" $+ e0) e0)
    loop IntegerToDouble =
        return ("x" ==> "x")
    loop Double = return untranslatable
    loop (DoubleLit (DhallDouble n)) = return (Nix.mkFloat (realToFrac n))
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
        return (a' $+ b')
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
    loop Date = return untranslatable
    loop Time = return untranslatable
    loop TimeZone = return untranslatable
    loop List = return ("t" ==> untranslatable)
    loop (ListAppend a b) = do
        a' <- loop a
        b' <- loop b
        return (a' $++ b')
    loop (ListLit _ bs) = do
        bs' <- mapM loop (toList bs)
        return (Nix.mkList bs')
    loop ListBuild = do
        return
            (   "t"
            ==> "k"
            ==> (   "k"
                @@  untranslatable
                @@  ("x" ==> "xs" ==> (Nix.mkList ["x"] $++ "xs"))
                @@  Nix.mkList []
                )
            )
    loop ListFold = do
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
    loop ListLength = return ("t" ==> "builtins.length")
    loop ListHead = do
        return
            (   "t"
            ==> "xs"
            ==> Nix.mkIf ("xs" $== Nix.mkList [])
                    Nix.mkNull
                    ("builtins.head" @@ "xs")
            )
    loop ListLast = do
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
    loop ListIndexed = do
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
    loop ListReverse = do
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
    loop Optional = return ("t" ==> untranslatable)
    loop (Some a) = loop a
    loop None = return ("t" ==> Nix.mkNull)
    loop t
        | Just text <- Dhall.Pretty.temporalToText t = do
            loop (Dhall.Core.TextLit (Dhall.Core.Chunks [] text))
    -- The next three cases are not necessary, because they are handled by the
    -- previous case
    loop DateLiteral{} = undefined
    loop TimeLiteral{} = undefined
    loop TimeZoneLiteral{} = undefined
    loop (Record _) = return untranslatable
    loop (RecordLit a) = do
        a' <- traverse (loop . Dhall.Core.recordFieldValue) a
        return (nixAttrs (Dhall.Map.toList a'))
      where
        -- nonrecursive attrset that uses correctly quoted keys
        -- see https://github.com/dhall-lang/dhall-haskell/issues/2414
        nixAttrs pairs =
          Fix $ NSet NonRecursive $
          (\(key, val) -> NamedVar (DynamicKey (Plain (DoubleQuoted [Plain key])) :| []) val Nix.nullPos)
          <$> pairs
    loop (Union _) = return untranslatable
    loop (Combine _ _ a b) = do
        a' <- loop a
        b' <- loop b

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
    loop (CombineTypes _ _ _) = return untranslatable
    loop (Merge a b _) = do
        a' <- loop a
        b' <- loop b
        return (b' @@ a')
    loop (ToMap a _) = do
        a' <- loop a
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
    loop (ShowConstructor _) = do
        Left CannotShowConstructor
    loop (Prefer _ _ b c) = do
        b' <- loop b
        c' <- loop c
        return (b' $// c')
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
    loop (Field a (Dhall.Core.fieldSelectionLabel -> b)) = do
        a' <- loop a
        return (a' @. b)
    loop (Project a (Left b)) = do
        a' <- loop a
        return (Nix.mkNonRecSet [ Nix.inheritFrom a' (fmap VarName b) ])
    loop (Project _ (Right _)) =
        Left CannotProjectByType
    loop (Assert _) =
        return untranslatable
    loop (Equivalent _ _ _) =
        return untranslatable
    loop (With a (WithLabel k :| []) b) = do
        a' <- loop a
        b' <- loop b

        return (a' $// Nix.attrsE [(k, b')])
    loop (With a (WithLabel k :| k' : ks) b) = do
        a' <- loop a
        b' <- loop (With (Field "_" (FieldSelection Nothing k Nothing)) (k' :| ks) (Dhall.Core.shift 1 "_" b))

        return (Nix.letE "_" a' ("_" $// Nix.attrsE [(k, b')]))
    loop (With a (WithQuestion :| []) b) = do
        a' <- loop a
        b' <- loop b
        return (Nix.mkIf (a' $== Nix.mkNull) Nix.mkNull b')
    loop (With a (WithQuestion :| k : ks) b) = do
        a' <- loop a
        b' <- loop (With "_" (k :| ks) (Dhall.Core.shift 1 "_" b))
        return (Nix.letE "_" a' (Nix.mkIf (a' $== Nix.mkNull) Nix.mkNull b'))
    loop (ImportAlt a _) = loop a
    loop (Note _ b) = loop b
    loop (Embed x) = absurd x
