{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RankNTypes         #-}
{-# OPTIONS_GHC -Wall #-}

module Dhall.TypeCheck (
    -- * Type-checking
      typeWith
    , typeOf

    -- * Types
    , X(..)
    , TypeError(..)
    , TypeMessage(..)
    ) where

import Control.Exception (Exception)
import Control.Monad.Trans.State.Strict (State)
import Data.Foldable (forM_)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Text.Buildable (Buildable(..))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Typeable (Typeable)
import Dhall.Core (Const(..), Expr(..), Var(..))
import Dhall.Context (Context)

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Lazy                   as Text
import qualified Data.Text.Lazy.Builder           as Builder
import qualified Data.Vector
import qualified Dhall.Context
import qualified Dhall.Core
import qualified NeatInterpolation

axiom :: Const -> Either TypeError Const
axiom Type = return Kind
axiom Kind = Left (TypeError Dhall.Context.empty (Const Kind) (Untyped Kind))

rule :: Const -> Const -> Either () Const
rule Type Kind = Left ()
rule Type Type = return Type
rule Kind Kind = return Kind
rule Kind Type = return Type

match :: Var -> Var -> [(Text, Text)] -> Bool
match (V xL nL) (V xR nR)             []  =
    xL == xR  && nL == nR
match (V xL 0 ) (V xR 0 ) ((xL', xR'):_ )
    | xL == xL' && xR == xR' = True
match (V xL nL) (V xR nR) ((xL', xR'):xs) =
    match (V xL nL') (V xR nR') xs
  where
    nL' = if xL == xL' then nL - 1 else nL
    nR' = if xR == xR' then nR - 1 else nR

propEqual :: Expr X -> Expr X -> Bool
propEqual eL0 eR0 =
    State.evalState
        (go (Dhall.Core.normalize eL0) (Dhall.Core.normalize eR0))
        []
  where
    go :: Expr X -> Expr X -> State [(Text, Text)] Bool
    go (Const Type) (Const Type) = return True
    go (Const Kind) (Const Kind) = return True
    go (Var vL) (Var vR) = do
        ctx <- State.get
        return (match vL vR ctx)
    go (Pi xL tL bL) (Pi xR tR bR) = do
        ctx <- State.get
        eq1 <- go tL tR
        if eq1
            then do
                State.put ((xL, xR):ctx)
                eq2 <- go bL bR
                State.put ctx
                return eq2
            else return False
    go (App fL aL) (App fR aR) = do
        b1 <- go fL fR
        if b1 then go aL aR else return False
    go Bool Bool = return True
    go Natural Natural = return True
    go Integer Integer = return True
    go Double Double = return True
    go Text Text = return True
    go List List = return True
    go Maybe Maybe = return True
    go (Record ktsL0) (Record ktsR0) = do
        let loop ((kL, tL):ktsL) ((kR, tR):ktsR)
                | kL == kR = do
                    b <- go tL tR
                    if b
                        then loop ktsL ktsR
                        else return False
            loop [] [] = return True
            loop _  _  = return False
        loop (Data.Map.toList ktsL0) (Data.Map.toList ktsR0)
    go (Union ktsL0) (Union ktsR0) = do
        let loop ((kL, tL):ktsL) ((kR, tR):ktsR)
                | kL == kR = do
                    b <- go tL tR
                    if b
                        then loop ktsL ktsR
                        else return False
            loop [] [] = return True
            loop _  _  = return False
        loop (Data.Map.toList ktsL0) (Data.Map.toList ktsR0)
    go _ _ = return False

{-| Type-check an expression and return the expression's type if type-checking
    suceeds or an error if type-checking fails

    `typeWith` does not necessarily normalize the type since full normalization
    is not necessary for just type-checking.  If you actually care about the
    returned type then you may want to `normalize` it afterwards.
-}
typeWith :: Context (Expr X) -> Expr X -> Either TypeError (Expr X)
typeWith _     (Const c         ) = do
    fmap Const (axiom c)
typeWith ctx e@(Var (V x n)     ) = do
    case Dhall.Context.lookup x n ctx of
        Nothing -> Left (TypeError ctx e UnboundVariable)
        Just a  -> return a
typeWith ctx   (Lam x _A  b     ) = do
    let ctx' = fmap (Dhall.Core.shift 1 (V x 0)) (Dhall.Context.insert x _A ctx)
    _B <- typeWith ctx' b
    let p = Pi x _A _B
    _t <- typeWith ctx p
    return p
typeWith ctx e@(Pi  x _A _B     ) = do
    tA <- fmap Dhall.Core.normalize (typeWith ctx _A)
    kA <- case tA of
        Const k -> return k
        _       -> Left (TypeError ctx e (InvalidInputType _A))

    let ctx' = fmap (Dhall.Core.shift 1 (V x 0)) (Dhall.Context.insert x _A ctx)
    tB <- fmap Dhall.Core.normalize (typeWith ctx' _B)
    kB <- case tB of
        Const k -> return k
        _       -> Left (TypeError ctx' e (InvalidOutputType _B))

    case rule kA kB of
        Left () -> Left (TypeError ctx e (NoDependentTypes _A _B))
        Right k -> Right (Const k)
typeWith ctx e@(App f a         ) = do
    tf <- fmap Dhall.Core.normalize (typeWith ctx f)
    (x, _A, _B) <- case tf of
        Pi x _A _B -> return (x, _A, _B)
        _          -> Left (TypeError ctx e (NotAFunction f))
    _A' <- typeWith ctx a
    if propEqual _A _A'
        then do
            let a'   = Dhall.Core.shift   1  (V x 0) a
            let _B'  = Dhall.Core.subst (V x 0) a' _B
            let _B'' = Dhall.Core.shift (-1) (V x 0) _B'
            return _B''
        else do
            let nf_A  = Dhall.Core.normalize _A
            let nf_A' = Dhall.Core.normalize _A'
            Left (TypeError ctx e (TypeMismatch nf_A nf_A'))
typeWith ctx e@(Let f mt r b ) = do
    tr <- typeWith ctx r
    case mt of
        Nothing ->
            return ()
        Just t  ->
            if propEqual tr t
                then return ()
                else Left (TypeError ctx e (AnnotMismatch r t tr))
    let ctx' = Dhall.Context.insert f tr ctx
    typeWith ctx' b
typeWith ctx e@(Annot x t       ) = do
    t' <- typeWith ctx x
    if propEqual t t'
        then do
            return t
        else do
            let nf_t  = Dhall.Core.normalize t
            let nf_t' = Dhall.Core.normalize t'
            Left (TypeError ctx e (AnnotMismatch x nf_t nf_t'))
typeWith _      Bool              = do
    return (Const Type)
typeWith _     (BoolLit _       ) = do
    return Bool
typeWith ctx e@(BoolAnd l r     ) = do
    tl <- fmap Dhall.Core.normalize (typeWith ctx l)
    case tl of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantAnd True l tl))

    tr <- fmap Dhall.Core.normalize (typeWith ctx r)
    case tr of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantAnd False r tr))

    return Bool
typeWith ctx e@(BoolOr  l r     ) = do
    tl <- fmap Dhall.Core.normalize (typeWith ctx l)
    case tl of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantOr True l tl))

    tr <- fmap Dhall.Core.normalize (typeWith ctx r)
    case tr of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantOr False r tr))

    return Bool
typeWith ctx e@(BoolEQ  l r     ) = do
    tl <- fmap Dhall.Core.normalize (typeWith ctx l)
    case tl of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantEQ True l tl))

    tr <- fmap Dhall.Core.normalize (typeWith ctx r)
    case tr of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantEQ False r tr))

    return Bool
typeWith ctx e@(BoolNE  l r     ) = do
    tl <- fmap Dhall.Core.normalize (typeWith ctx l)
    case tl of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantNE True l tl))

    tr <- fmap Dhall.Core.normalize (typeWith ctx r)
    case tr of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantNE False r tr))

    return Bool
typeWith ctx e@(BoolIf x y z    ) = do
    tx <- fmap Dhall.Core.normalize (typeWith ctx x)
    case tx of
        Bool -> return ()
        _    -> Left (TypeError ctx e (InvalidPredicate x tx))
    ty <- fmap Dhall.Core.normalize (typeWith ctx y)
    tz <- fmap Dhall.Core.normalize (typeWith ctx z)
    if propEqual ty tz
        then return ()
        else Left (TypeError ctx e (IfBranchMismatch y z ty tz))
    return ty
typeWith _      Natural           = do
    return (Const Type)
typeWith _     (NaturalLit _    ) = do
    return Natural
typeWith _      NaturalFold       = do
    return
        (Pi "_" Natural
            (Pi "natural" (Const Type)
                (Pi "succ" (Pi "_" "natural" "natural")
                    (Pi "zero" "natural" "natural") ) ) )
typeWith _      NaturalBuild      = do
    return
        (Pi "_"
            (Pi "natural" (Const Type)
                (Pi "succ" (Pi "_" "natural" "natural")
                    (Pi "zero" "natural" "natural") ) )
            Natural )
typeWith _      NaturalIsZero     = do
    return (Pi "_" Natural Bool)
typeWith _      NaturalEven       = do
    return (Pi "_" Natural Bool)
typeWith _      NaturalOdd        = do
    return (Pi "_" Natural Bool)
typeWith ctx e@(NaturalPlus  l r) = do
    tl <- fmap Dhall.Core.normalize (typeWith ctx l)
    case tl of
        Natural -> return ()
        _       -> Left (TypeError ctx e (CantAdd True l tl))

    tr <- fmap Dhall.Core.normalize (typeWith ctx r)
    case tr of
        Natural -> return ()
        _       -> Left (TypeError ctx e (CantAdd False r tr))
    return Natural
typeWith ctx e@(NaturalTimes l r) = do
    tl <- fmap Dhall.Core.normalize (typeWith ctx l)
    case tl of
        Natural -> return ()
        _       -> Left (TypeError ctx e (CantMultiply True l tl))

    tr <- fmap Dhall.Core.normalize (typeWith ctx r)
    case tr of
        Natural -> return ()
        _       -> Left (TypeError ctx e (CantMultiply False r tr))
    return Natural
typeWith _      Integer           = do
    return (Const Type)
typeWith _     (IntegerLit _    ) = do
    return Integer
typeWith _      Double            = do
    return (Const Type)
typeWith _     (DoubleLit _     ) = do
    return Double
typeWith _      Text              = do
    return (Const Type)
typeWith _     (TextLit _       ) = do
    return Text
typeWith ctx e@(TextAppend l r  ) = do
    tl <- fmap Dhall.Core.normalize (typeWith ctx l)
    case tl of
        Text -> return ()
        _    -> Left (TypeError ctx e (CantTextAppend True l tl))

    tr <- fmap Dhall.Core.normalize (typeWith ctx r)
    case tr of
        Text -> return ()
        _    -> Left (TypeError ctx e (CantTextAppend False r tr))
    return Text
typeWith _      List              = do
    return (Pi "_" (Const Type) (Const Type))
typeWith ctx e@(ListLit t xs    ) = do
    s <- fmap Dhall.Core.normalize (typeWith ctx t)
    case s of
        Const Type -> return ()
        _ -> Left (TypeError ctx e (InvalidListType (Data.Vector.null xs) t))
    let n = Data.Vector.length xs
    flip Data.Vector.imapM_ xs (\i x -> do
        t' <- typeWith ctx x
        if propEqual t t'
            then return ()
            else do
                let nf_t  = Dhall.Core.normalize t
                let nf_t' = Dhall.Core.normalize t'
                Left (TypeError ctx e (InvalidListElement i n x nf_t nf_t')) )
    return (App List t)
typeWith _      ListBuild         = do
    return
        (Pi "a" (Const Type)
            (Pi "_"
                (Pi "list" (Const Type)
                    (Pi "cons" (Pi "_" "a" (Pi "_" "list" "list"))
                        (Pi "nil" "list" "list") ) )
                (App List "a") ) )
typeWith _      ListFold          = do
    return
        (Pi "a" (Const Type)
            (Pi "_" (App List "a")
                (Pi "list" (Const Type)
                    (Pi "cons" (Pi "_" "a" (Pi "_" "list" "list"))
                        (Pi "nil" "list" "list")) ) ) )
typeWith _      ListLength        = do
    return (Pi "a" (Const Type) (Pi "_" (App List "a") Natural))
typeWith _      ListHead          = do
    return (Pi "a" (Const Type) (Pi "_" (App List "a") (App Maybe "a")))
typeWith _      ListLast          = do
    return (Pi "a" (Const Type) (Pi "_" (App List "a") (App Maybe "a")))
typeWith _      ListIndexed       = do
    let kts = [("index", Natural), ("value", "a")]
    return
        (Pi "a" (Const Type)
            (Pi "_" (App List "a")
                (App List (Record (Data.Map.fromList kts))) ) )
typeWith _      ListReverse       = do
    return (Pi "a" (Const Type) (Pi "_" (App List "a") (App List "a")))
typeWith _      Maybe             = do
    return (Pi "_" (Const Type) (Const Type))
typeWith ctx e@(MaybeLit t xs   ) = do
    s <- fmap Dhall.Core.normalize (typeWith ctx t)
    case s of
        Const Type -> return ()
        _ -> Left (TypeError ctx e (InvalidMaybeType t))
    let n = Data.Vector.length xs
    if 2 <= n
        then Left (TypeError ctx e (InvalidMaybeLiteral n))
        else return ()
    forM_ xs (\x -> do
        t' <- typeWith ctx x
        if propEqual t t'
            then return ()
            else do
                let nf_t  = Dhall.Core.normalize t
                let nf_t' = Dhall.Core.normalize t'
                Left (TypeError ctx e (InvalidMaybeElement x nf_t nf_t')) )
    return (App Maybe t)
typeWith _      MaybeFold         = do
    return
        (Pi "a" (Const Type)
            (Pi "_" (App Maybe "a")
                (Pi "maybe" (Const Type)
                    (Pi "just" (Pi "_" "a" "maybe")
                        (Pi "nothing" "maybe" "maybe") ) ) ) )
typeWith ctx e@(Record    kts   ) = do
    let process (k, t) = do
            s <- fmap Dhall.Core.normalize (typeWith ctx t)
            case s of
                Const Type -> return ()
                _          -> Left (TypeError ctx e (InvalidFieldType k t))
    mapM_ process (Data.Map.toList kts)
    return (Const Type)
typeWith ctx   (RecordLit kvs   ) = do
    let process (k, v) = do
            t <- typeWith ctx v
            return (k, t)
    kts <- mapM process (Data.Map.toAscList kvs)
    return (Record (Data.Map.fromAscList kts))
typeWith ctx e@(Union     kts   ) = do
    let process (k, t) = do
            s <- fmap Dhall.Core.normalize (typeWith ctx t)
            case s of
                Const Type -> return ()
                _          -> Left (TypeError ctx e (InvalidAlternativeType k t))
    mapM_ process (Data.Map.toList kts)
    return (Const Type)
typeWith ctx e@(UnionLit k v kts) = do
    case Data.Map.lookup k kts of
        Just _  -> Left (TypeError ctx e (DuplicateField k))
        Nothing -> return ()
    t   <- typeWith ctx v
    return (Union (Data.Map.insert k t kts))
typeWith ctx e@(Apply kvsX kvsY t) = do
    tKvsX <- fmap Dhall.Core.normalize (typeWith ctx kvsX)
    ktsX  <- case tKvsX of
        Record kts -> return kts
        _          -> Left (TypeError ctx e (MustApplyARecord kvsX tKvsX))
    let ksX = Data.Map.keysSet ktsX

    tKvsY <- fmap Dhall.Core.normalize (typeWith ctx kvsY)
    ktsY  <- case tKvsY of
        Union kts -> return kts
        _         -> Left (TypeError ctx e (MustApplyToUnion tKvsY))
    let ksY = Data.Map.keysSet ktsY

    let diffX = Data.Set.difference ksX ksY
    let diffY = Data.Set.difference ksY ksX

    if Data.Set.null diffX
        then return ()
        else Left (TypeError ctx e (UnusedHandler diffX))

    let process (kY, tY) = do
            case Data.Map.lookup kY ktsX of
                Nothing  -> Left (TypeError ctx e (MissingHandler diffY))
                Just tX  ->
                    case tX of
                        Pi _ tY' t' -> do
                            if propEqual tY tY'
                                then return ()
                                else Left (TypeError ctx e (HandlerInputTypeMismatch kY tY tY'))
                            if propEqual t t'
                                then return ()
                                else Left (TypeError ctx e (HandlerOutputTypeMismatch kY t t'))
                        _ -> Left (TypeError ctx e (HandlerNotAFunction kY tX))
    mapM_ process (Data.Map.toList ktsY)
    return t
typeWith ctx e@(Field r x       ) = do
    t <- fmap Dhall.Core.normalize (typeWith ctx r)
    case t of
        Record kts ->
            case Data.Map.lookup x kts of
                Just t' -> return t'
                Nothing -> Left (TypeError ctx e (MissingField x t))
        _          -> Left (TypeError ctx e (NotARecord x r t))
typeWith _     (Embed p         ) = do
    absurd p

{-| `typeOf` is the same as `typeWith` with an empty context, meaning that the
    expression must be closed (i.e. no free variables), otherwise type-checking
    will fail.
-}
typeOf :: Expr X -> Either TypeError (Expr X)
typeOf = typeWith Dhall.Context.empty

-- | Like `Data.Void.Void`, except with a shorter inferred type
newtype X = X { absurd :: forall a . a }

instance Show X where
    show = absurd

instance Buildable X where
    build = absurd

-- | The specific type error
data TypeMessage
    = UnboundVariable
    | InvalidInputType (Expr X)
    | InvalidOutputType (Expr X)
    | NotAFunction (Expr X)
    | TypeMismatch (Expr X) (Expr X)
    | AnnotMismatch (Expr X) (Expr X) (Expr X)
    | Untyped Const
    | InvalidListElement Int Int (Expr X) (Expr X) (Expr X)
    | InvalidListType Bool (Expr X)
    | InvalidMaybeElement (Expr X) (Expr X) (Expr X)
    | InvalidMaybeLiteral Int
    | InvalidMaybeType (Expr X)
    | InvalidPredicate (Expr X) (Expr X)
    | IfBranchMismatch (Expr X) (Expr X) (Expr X) (Expr X)
    | InvalidFieldType Text (Expr X)
    | InvalidAlternativeType Text (Expr X)
    | DuplicateField Text
    | MustApplyARecord (Expr X) (Expr X)
    | MustApplyToUnion (Expr X)
    | UnusedHandler (Set Text)
    | MissingHandler (Set Text)
    | HandlerInputTypeMismatch Text (Expr X) (Expr X)
    | HandlerOutputTypeMismatch Text (Expr X) (Expr X)
    | HandlerNotAFunction Text (Expr X)
    | NotARecord Text (Expr X) (Expr X)
    | MissingField Text (Expr X)
    | CantAnd Bool (Expr X) (Expr X)
    | CantOr Bool (Expr X) (Expr X)
    | CantEQ Bool (Expr X) (Expr X)
    | CantNE Bool (Expr X) (Expr X)
    | CantTextAppend Bool (Expr X) (Expr X)
    | CantAdd Bool (Expr X) (Expr X)
    | CantMultiply Bool (Expr X) (Expr X)
    | NoDependentTypes (Expr X) (Expr X)
    deriving (Show)

instance Buildable TypeMessage where
    build UnboundVariable =
        Builder.fromText [NeatInterpolation.text|
Error: Unbound variable

Explanation: Expressions can only reference previously introduced (i.e. "bound")
variables that are still "in scope".  For example, these are valid expressions:

    λ(x : Bool) → x       -- Anonymous functions introduce "bound" variables

    let x = 1 in x        -- `let` definitions introduce "bound" variables

    let f (x : Bool) = x  -- Function arguments are "bound" variables
    in  f True

... but these are not valid expressions:

    λ(x : Bool) → y           -- The variable `y` hasn't been introduced yet

    (let x = True in x) && x  -- `x` is undefined outside the parentheses

    let x = x in x            -- The definition for `x` cannot reference itself
|]

    build (InvalidInputType expr) =
        Builder.fromText [NeatInterpolation.text|
Error: Invalid input annotation for a function

Explanation: A function can accept an input term of a given "type", like this:

    ∀(x : Text) → Bool  -- This function accepts any term of type `Text`.
                        -- `x` is the term's name and `Text` is the type

    Bool → Integer  -- This function accepts any term of type `Bool`.
                    -- The input term's name is omitted

... or accept an input "type" of a given "kind", like this:

    ∀(a : Type) → Type  -- This accepts any type `a` of kind `Type`

Other input annotations are *not* valid, like this:

    ∀(x : 1) → x  -- `1` is a term and not a "type" nor a "kind"

This input annotation you gave is neither a type nor a kind:
↳ $txt
|]
      where
        txt = Text.toStrict (Dhall.Core.pretty expr)

    build (InvalidOutputType expr) =
        Builder.fromText [NeatInterpolation.text|
Error: Invalid output annotation for a function

Explanation: A function can emit an output term of a given "type", like this:

    ∀(x : Text) → Bool  -- This function emits a term of type `Bool`.

    Bool → Int          -- This function emits a term of type `Int`.

... or emit an output "type" of a given "kind", like this:

    ∀(a : Type) → Type  -- This emits a type of kind `Type`

Other outputs are *not* valid, like this:

    ∀(x : Text) → 1     -- `1` is a term and not a "type" nor a "kind"

This function output you specified is neither a type nor a kind:
↳ $txt
|]
      where
        txt = Text.toStrict (Dhall.Core.pretty expr)

    build (NotAFunction expr) =
        Builder.fromText [NeatInterpolation.text|
Error: Only functions may be applied to arguments

Explanation: Expressions separated by whitespace denote function application.
For example:

    f x  -- This denotes the function `f` applied to an argument `x`

However, not everything is a valid function.  For example:

    1                         -- Primitive terms are not functions
    Text                      -- Primitive types are not functions
    Type                      -- Primitive kinds are not functions
    { foo = 1, bar = "ABC" }  -- Records are not functions

You tried to apply an expression that was not a function to an argument

This is the expression that you incorrectly invoked as a function:
↳ $txt
|]
      where
        txt = Text.toStrict (Dhall.Core.pretty expr)

    build (TypeMismatch expr0 expr1) =
        Builder.fromText [NeatInterpolation.text|
Error: Function applied to the wrong type or kind of argument

Explanation: Every function declares what type or kind of argument to accept

    λ(x : Bool) → x    -- Anonymous function which only accepts `Bool` arguments

    let f (x : Bool) = x   -- Named function which only accepts `Bool` arguments
    in  f True

    λ(a : Type) → a    -- Anonymous function which only accepts `Type` arguments

You *cannot* apply a function to the wrong type or kind of argument:

    (λ(x : Bool) → x) "A"  -- "A" is `Text`, but the function expects a `Bool`

You tried to invoke a function which expects an argument of type or kind:
↳ $txt0
... on an argument of type or kind:
↳ $txt1
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)

    build (AnnotMismatch expr0 expr1 expr2) =
        Builder.fromText [NeatInterpolation.text|
Error: Expression's inferred type does not match annotated type

Explanation: You can annotate the type or kind of an expression like this:

    x : t  -- `x` is the expression and `t` is the annotated type or kind of `x`

Annotations are introduced in one of two ways:

* You can manually annotate expressions to declare the type or kind you expect
* The interpreter also implicitly inserts a top-level type annotation

Annotations are optional because the compiler can infer the type of all
expressions.  However, if you or the interpreter inserts an annotation and the
inferred type or kind does not match the annotation then type-checking fails.

You or the interpreter annotated this expression:
↳ $txt0
... with this type or kind:
↳ $txt1
... but the inferred type of the expression is actually this type or kind:
↳ $txt2
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)
        txt2 = Text.toStrict (Dhall.Core.pretty expr2)

    build (Untyped c) =
        Builder.fromText [NeatInterpolation.text|
Error: `$txt` has no type, kind, or sort

Explanation: There are four levels of expressions that form a heirarchy:

* terms
* types
* kinds
* sorts

The following annotations illustrate this heirarchy:

    "ABC" : Text : Type : Kind

Every term has a type.  For example, the term `"ABC"` has type `Text`
Every type has a kind.  For example, the type `Text` has kind `Type`
Every kind has a sort.  For example, the kind `Type` has sort `Kind`

However, there is nothing above sorts in this the hierarchy.  So if you ever
type-check an expression which includes `Kind` then you get this error because
the compiler cannot infer what `Kind` belongs to
|]
      where
        txt = Text.toStrict (Builder.toLazyText (Dhall.Core.buildConst c))

    build (InvalidPredicate expr0 expr1) =
        Builder.fromText [NeatInterpolation.text|
Error: Invalid predicate for `if`

    if $txt0 then ...
    -- ^ Your `if` expression's predicate has the wrong type

Your `if` expression begins with a predicate that has type:
↳ $txt1
... but the predicate must have type `Bool`
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)

    build (IfBranchMismatch expr0 expr1 expr2 expr3) =
        Builder.fromText [NeatInterpolation.text|
Error: The `then` and `else` branches must have matching types

    if ... then $txt0
           else $txt1
    --          ^ The above two expressions need to have the same type

Your `if` expression has two branches with different types

The type of the `then` branch is:
↳ $txt2
The type of the `else` branch is:
↳ $txt3

Fix the two branches to have matching types
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)
        txt2 = Text.toStrict (Dhall.Core.pretty expr2)
        txt3 = Text.toStrict (Dhall.Core.pretty expr3)

    build (InvalidListType isEmpty expr0) =
        Builder.fromText [NeatInterpolation.text|
Error: Invalid type for list elements

Explanation: Every list ends with a type annotation for the elements of the list

This annotation must be a type, but the annotation you gave is not a type:

$insert

You can fix the problem by changing the annotation to a type
|]
      where
        txt0 = Text.toStrict (Builder.toLazyText (Dhall.Core.buildExpr6 expr0))
        insert = indent $
            if isEmpty
            then [NeatInterpolation.text|
    [ ] : List $txt0
    --         ^ This needs to be a type
|]
            else [NeatInterpolation.text|
    [ ... ] : List $txt0
    --             ^ This needs to be a type
|]

    build (InvalidListElement i n expr0 expr1 expr2) =
        Builder.fromText [NeatInterpolation.text|
Error: List with an element of the wrong type

Explanation: Every element in the list must have a type matching the type
annotation at the end of the list

However, your list has an element of the wrong type:

$insert

The element you provided actually has this type:
↳ $txt2

You can fix the problem by either changing the list element or changing the
declared element type
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Builder.toLazyText (Dhall.Core.buildExpr6 expr1))
        txt2 = Text.toStrict (Dhall.Core.pretty expr2)
        txt3 = Text.toStrict (Dhall.Core.pretty i    )
        insert = indent $
            if n == 1
            then [NeatInterpolation.text|
    [  $txt0
    -- ^ This value ...
    ] : List $txt1
    --       ^ ... needs to match this type
|]
            else if i == 0
            then [NeatInterpolation.text|
    [  $txt0
    -- ^ This value ...
    ,  ...
    ] : List $txt1
    --       ^ ... needs to match this type
    ]
|]
            else if i + 1 == n
            then [NeatInterpolation.text|
    [  ...
    ,  $txt0
    -- ^ This value ...
    ] : List $txt1
    --       ^ ... needs to match this type
|]
            else [NeatInterpolation.text|
    [ ...
    ,  $txt0
    -- ^ This value at index #$txt3 ...
    ,  ...
    ] : List $txt1
    --       ^ ... needs to match this type
|]

    build (InvalidMaybeType expr0) =
        Builder.fromText [NeatInterpolation.text|
Error: Invalid type for `Maybe`

Explanation: Every optional value ends with a type annotation for the element
that might be stored inside.  For example, these are valid expressions:

    [1] : Maybe Integer  -- An optional value that's present
    []  : Maybe Integer  -- An optional value that's absent

The type following the `Maybe` is the "type parameter", and must be a type:

    Maybe Integer -- This is valid, because `Integer` is a type
    Maybe Text    -- This is also valid, because `Text` is a type

... but the type parameter must *not* be a term or kind:

    Maybe 1       -- This is invalid, because `1` is a term
    Maybe Type    -- This is invalid, because `Type` is a kind

You provided a type parameter for the `Maybe` that is not a valid type:

$insert
|]
      where
        txt0 = Text.toStrict (Builder.toLazyText (Dhall.Core.buildExpr6 expr0))
        insert = indent [NeatInterpolation.text|
    [ ... ] : Maybe $txt0
    --              ^ This needs to be a type
|]

    build (InvalidMaybeElement expr0 expr1 expr2) =
        Builder.fromText [NeatInterpolation.text|
Error: Optional expression with an element of the wrong type

Explanation: An optional value that is present must have a type matching the
corresponding type annotation.  For example, this is a valid optional value:

    [1] : Maybe Integer  -- The type of `1` is `Integer`, which matches

... but this is *not* a valid optional value:

    [1] : Maybe Text     -- Invalid, because the type of `1` is not `Text`

Your optional value has a type which does not match the type annotation:

$insert

The element you provided actually has this type:
↳ $txt2
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Builder.toLazyText (Dhall.Core.buildExpr6 expr1))
        txt2 = Text.toStrict (Dhall.Core.pretty expr2)
        insert = indent [NeatInterpolation.text|
    [  $txt0
    -- ^ This value ...
    ] : Maybe $txt1
    --        ^ ... needs to have a type matching this type parameter
|]

    build (InvalidMaybeLiteral n) =
        Builder.fromText [NeatInterpolation.text|
Error: More than one element for an optional value

Explanation: The syntax for an optional value resembles the syntax for `List`
literals:

    []  : Maybe Integer  -- A valid literal for an absent optional value
    [1] : Maybe Integer  -- A valid literal for a present optional value
    []  : List  Integer  -- A valid literal for an empty     (0-element) `List`
    [1] : List  Integer  -- A valid literal for a  singleton (1-element) `List`

However, an optional value can *not* have more than one element, whereas a
`List` can have multiple elements:

    [1, 2] : Maybe Integer  -- Invalid: multiple elements not allowed
    [1, 2] : List  Integer  -- Valid  : multiple elements allowed

Your optional value had $txt0 elements, which is not allowed.  Optional values
can only have at most one element
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty n)

    build (InvalidFieldType k expr0) =
        Builder.fromText [NeatInterpolation.text|
Error: Invalid type of field

Explanation: Every record type has an annotated type for each field

However, fields *cannot* be annotated with expressions other than types

You provided a record type with a key named:
↳ $txt0
... annotated with the following expression which is not a type:

    { ... : $txt1, ... }
    --      ^ This needs to be a type

You can fix the problem by changing the annotation to a type
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty k    )
        txt1 = Text.toStrict (Dhall.Core.pretty expr0)

    build (InvalidAlternativeType k expr0) =
        Builder.fromText [NeatInterpolation.text|
Error: Invalid type of alternative

Explanation: Every union type has an annotated type for each alternative

However, alternatives *cannot* be annotated with expressions other than types

You provided a union type with a alternative named:
↳ $txt0
... annotated with the following expression which is not a type:

    < ... : $txt1 , ... >
    --      ^ This needs to be a type

You can fix the problem by changing the annotation to a type
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty k    )
        txt1 = Text.toStrict (Dhall.Core.pretty expr0)

    build (DuplicateField k) =
        Builder.fromText [NeatInterpolation.text|
Error: Duplicate field

Explanation: Records and unions may not have two fields of the same name

    { foo = True, foo = 1 }        -- This is not valid

    < foo = True | foo : Integer >  -- This is also not valid

You have multiple fields named:
↳ $txt0
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty k)

    build (MustApplyARecord expr0 expr1) =
        Builder.fromText [NeatInterpolation.text|
Error: You can only `apply` a record of a handlers

Explanation: You can consume a union by `apply`ing a record of handlers, like
this:

        let handlers = { Left = Natural/even, Right = \(x : Bool) -> x }
    in  let union    = < Left = +2 | Right : Bool >
    in  apply handlers union : Bool

... but the first argument to `apply` must be a record and not another type.

For example, the following expression is *not* valid:

    let f (x : Bool) = x
    in  apply f < Foo = True > : True  -- Invalid: `f` is not a record

You provided the following handler:
↳ $txt0
... which is not a record, but is actually a value of type:
↳ $txt1
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)

    build (MustApplyToUnion expr0) =
        Builder.fromText [NeatInterpolation.text|
Error: You can only `apply` handlers to a union

Explanation: You can consume a union by `apply`ing a record of handlers, like
this:

        let handlers = { Left = Natural/even, Right = λ(x : Bool) → x }
    in  let union    = < Left = +2 | Right : Bool >
    in  apply handlers union : Bool

... but the second argument to `apply` must be a union and not another type.

For example, the following expression is *not* valid:

    let handlers = { Foo = λ(x : Bool) → x }
    in  apply handlers True  -- Invalid: `True` is not a union

You applied a record of handlers to this expression, which is not a union:
↳ $txt0
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)

    build (UnusedHandler ks) =
        Builder.fromText [NeatInterpolation.text|
Error: Unused handler

Explanation: You can consume a union by `apply`ing a record of handlers, like
this:

        let handlers = { Left = Natural/even, Right = λ(x : Bool) → x }
    in  let union    = < Left = +2 | Right : Bool >
    in  apply handlers union : Bool

... but there must be exactly one handler per alternative in the union.  You
cannot have extra handlers.

For example, the following expression is *not* valid:

        let handlers =
                { Left = Natural/even
                , Right = λ(x : Bool) → x  -- Invalid: This handler is not used
                }
    in  let union = < Left = +2 >  -- The `Right` alternative is missing
    in  apply handlers union : Bool  

The following handlers had no matching alternatives:
↳ $txt0
|]
      where
        txt0 = Text.toStrict (Text.intercalate ", " (Data.Set.toList ks))

    build (MissingHandler ks) =
        Builder.fromText [NeatInterpolation.text|
Error: Missing handler

Explanation: You can consume a union by `apply`ing a record of handlers, like
this:

        let handlers = { Left = Natural/even, Right = λ(x : Bool) → x }
    in  let union    = < Left = +2 | Right : Bool >
    in  apply handlers union : Bool

... but there must be exactly one handler per alternative in the union.  You
cannot have missing handlers.

For example, the following expression is *not* valid:

        let handlers = { Left = Natural/even }  -- Invalid: No `Right` handler
    in  let union = < Left = +2 | Right : Bool >
    in  apply handlers union : Bool

The following handlers are missing:
↳ $txt0
|]
      where
        txt0 = Text.toStrict (Text.intercalate ", " (Data.Set.toList ks))

    build (HandlerInputTypeMismatch expr0 expr1 expr2) =
        Builder.fromText [NeatInterpolation.text|
Error: Handler has the wrong input type

Explanation: You can consume a union by `apply`ing a record of handlers, like
this:

        let handlers = { Left = Natural/even, Right = λ(x : Bool) → x }
    in  let union    = < Left = +2 | Right : Bool >
    in  apply handlers union : Bool

... but the input type of each handler must match the type of the corresponding
alternative of the union:

        let handlers = { Left = Natural/even, Right = λ(x : Bool) → x }
                             -- ^ This function ...
    in  let union    = < Left = +2 | Right : Bool >
                             -- ^ ... must accept a value of this type
    in  apply handlers union : Bool

Your handler for the following alternative:
↳ $txt0
... was supposed to accept a value of type:
↳ $txt1
... but actually accepts a value of type:
↳ $txt2
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)
        txt2 = Text.toStrict (Dhall.Core.pretty expr2)

    build (HandlerOutputTypeMismatch expr0 expr1 expr2) =
        Builder.fromText [NeatInterpolation.text|
Error: Handler has the wrong output type

Explanation: You can consume a union by `apply`ing a record of handlers, like
this:

        let handlers = { Left = Natural/even, Right = λ(x : Bool) → x }
    in  let union    = < Left = +2 | Right : Bool >
    in  apply handlers union : Bool

... but the output type of each handler must match the declared final type:

        let handlers = { Left = Natural/even, Right = λ(x : Bool) → x }
                             -- ^                     ^
                             -- These two functions ...
    in  let union    = < Left = +2 | Right : Bool >
    in  apply handlers union : Bool  -- ... must return a `Bool`

Your handler for the following alternative:
↳ $txt0
... was supposed to return a value of type:
↳ $txt1
... but actually returns a value of type:
↳ $txt2
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)
        txt2 = Text.toStrict (Dhall.Core.pretty expr2)

    build (HandlerNotAFunction k expr0) =
        Builder.fromText [NeatInterpolation.text|
Error: Handler is not a function

Explanation: You can consume a union by `apply`ing a record of handlers, like
this:

        let handlers = { Left = Natural/even, Right = λ(x : Bool) → x }
    in  let union    = < Left = +2 | Right : Bool >
    in  apply handlers union : Bool

... but the type of each handler must be a function.

For example, the following expression is *not* valid:

    apply { Foo = 1 } < Foo = 1 > : Integer
               -- ^ Not a function

Your handler for:
↳ $txt0
... has the following type:
↳ $txt1
... which is not the type of a function
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty k)
        txt1 = Text.toStrict (Dhall.Core.pretty expr0)

    build (NotARecord k expr0 expr1) =
        Builder.fromText [NeatInterpolation.text|
Error: Invalid record access

Explanation: You can only access fields on records, like this:

    { foo = True, bar = "ABC" }.foo              -- This is valid ...

    λ(r : {{ foo : Bool, bar : Text }}) → r.foo  -- ... and so is this

... but you *cannot* access fields on non-record expressions, like this:

    1.foo                  -- `1` is not a valid record

    (λ(x : Bool) → x).foo  -- A function is not a valid record

You tried to access a field named:
↳ $txt0
... on the following expression which is not a record:
↳ $txt1
... but is actually an expression of type:
↳ $txt2
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty k    )
        txt1 = Text.toStrict (Dhall.Core.pretty expr0)
        txt2 = Text.toStrict (Dhall.Core.pretty expr1)

    build (MissingField k expr0) =
        Builder.fromText [NeatInterpolation.text|
Error: Missing record field

Explanation: You can only retrieve record fields if they are present

    { foo = True, bar = "ABC" }.foo              -- This is valid ...

    λ(r : {{ foo : Bool, bar : Text }}) → r.foo  -- ... and so is this

... but you *cannot* access fields missing from a record:

    { foo = True, bar = "ABC" }.qux  -- Not valid: the field `qux` is missing

You tried to access a field named:
↳ $txt0
... but the field is missing because the record only defines these fields:
↳ $txt1
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty k    )
        txt1 = Text.toStrict (Dhall.Core.pretty expr0)

    build (CantAnd b expr0 expr1) =
        buildBooleanOperator "&&" b expr0 expr1

    build (CantOr b expr0 expr1) =
        buildBooleanOperator "||" b expr0 expr1

    build (CantEQ b expr0 expr1) =
        buildBooleanOperator "==" b expr0 expr1

    build (CantNE b expr0 expr1) =
        buildBooleanOperator "/=" b expr0 expr1

    build (CantTextAppend b expr0 expr1) =
        Builder.fromText [NeatInterpolation.text|
Error: Cannot use `(++)` on a value that's not a `Text`

Explanation: The `(++)` operator expects two arguments of type `Text`

You provided this argument:

    $insert

... whose type is not `Text`.  The type is actually:
↳ $txt1
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)
        insert =
            if b
            then [NeatInterpolation.text|$txt0 ++ ...|]
            else [NeatInterpolation.text|... ++ $txt0|]

    build (CantAdd b expr0 expr1) =
        buildNaturalOperator "+" b expr0 expr1

    build (CantMultiply b expr0 expr1) =
        buildNaturalOperator "*" b expr0 expr1

    build (NoDependentTypes expr0 expr1) =
        Builder.fromText [NeatInterpolation.text|
Error: No dependent types

Explanation: This programming language does not allow functions from terms to
types.  For example, this is *not* a legal function type:

    Bool → Type

Your function type is invalid because the input is a term of type:
↳ $txt0
... and the output is a type of kind:
↳ $txt1
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)

indent :: Data.Text.Text -> Data.Text.Text
indent = Data.Text.unlines . fmap ("    " <>) . Data.Text.lines

buildBooleanOperator :: Text -> Bool -> Expr X -> Expr X -> Builder
buildBooleanOperator operator b expr0 expr1 =
    Builder.fromText [NeatInterpolation.text|
Error: Cannot use `($txt2)` on a value that's not a `Bool`

Explanation: The `($txt2)` operator expects two arguments of type `Bool`

You provided this argument:

    $insert

... whose type is not `Bool`.  The type is actually:
↳ $txt1
|]
  where
    txt0 = Text.toStrict (Dhall.Core.pretty expr0)
    txt1 = Text.toStrict (Dhall.Core.pretty expr1)
    txt2 = Text.toStrict operator
    insert =
        if b
        then [NeatInterpolation.text|$txt0 $txt2 ...|]
        else [NeatInterpolation.text|... $txt2 $txt0|]

buildNaturalOperator :: Text -> Bool -> Expr X -> Expr X -> Builder
buildNaturalOperator operator b expr0 expr1 =
    Builder.fromText [NeatInterpolation.text|
Error: Cannot use `($txt2)` on a value that's not a `Natural`

Explanation: The `($txt2)` operator expects two arguments of type `Natural`

You provided this argument:

    $insert0

... whose type is not `Natural`.  The type is actually:
↳ $txt1$hint0$hint1|]
  where
    txt0 = Text.toStrict (Dhall.Core.pretty expr0)
    txt1 = Text.toStrict (Dhall.Core.pretty expr1)
    txt2 = Text.toStrict operator
    insert0 =
        if b
        then [NeatInterpolation.text|$txt0 $txt2 ...|]
        else [NeatInterpolation.text|... $txt2 $txt0|]
    insert1 =
        if b
        then [NeatInterpolation.text|+$txt0 $txt2 ...|]
        else [NeatInterpolation.text|... $txt2 +$txt0|]
    hint0 =
        case expr1 of
            Integer -> "\n\n" <> [NeatInterpolation.text|
An `Integer` is not the same thing as a `Natural` number.  They are distinct
types: `Integer`s can be negative, but `Natural` numbers must be non-negative
|]
            _ -> mempty
    hint1 =
        case expr0 of
            IntegerLit _ -> "\n\n" <> [NeatInterpolation.text|
You can prefix an `Integer` literal with a `+` to create a `Natural` literal

Example:

    $insert1
|]
            _ -> mempty

-- | A structured type error that includes context
data TypeError = TypeError
    { context     :: Context (Expr X)
    , current     :: Expr X
    , typeMessage :: TypeMessage
    } deriving (Typeable)

instance Show TypeError where
    show = Text.unpack . Dhall.Core.pretty

instance Exception TypeError

instance Buildable TypeError where
    build (TypeError ctx expr msg)
        =   "\n"
        <>  (    if  Text.null (Builder.toLazyText (buildContext ctx))
                 then ""
                 else "Context:\n" <> buildContext ctx <> "\n"
            )
        <>  "Expression: " <> build expr <> "\n"
        <>  "\n"
        <>  build msg
      where
        buildKV (key, val) = build key <> " : " <> build val

        buildContext =
                build
            .   Text.unlines
            .   map (Builder.toLazyText . buildKV)
            .   reverse
            .   Dhall.Context.toList
