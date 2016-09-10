{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS_GHC -Wall #-}

-- | This module contains the core calculus for the Dhall language.

module Dhall.Core (
    -- * Syntax
    Const(..),
    Path(..),
    X(..),
    Let(..),
    Expr(..),
    Context,

    -- * Core functions
    typeWith,
    typeOf,
    normalize,

    -- * Utilities
    subst,
    pretty,

    -- * Errors
    TypeError(..),
    TypeMessage(..),
    ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative (Applicative(..), (<$>))
#endif
import Control.Exception (Exception)
import Data.Foldable
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text.Buildable (Buildable(..))
import Data.Text.Lazy (Text)
import Data.Traversable
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Dhall.Context (Context)
import Filesystem.Path.CurrentOS (FilePath)
import Numeric.Natural (Natural)
import Prelude hiding (FilePath)

import qualified Control.Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List
import qualified Data.Map
import qualified Data.Text.Lazy                   as Text
import qualified Data.Text.Lazy.Builder           as Builder
import qualified Data.Vector
import qualified Data.Vector.Mutable
import qualified Dhall.Context                    as Context
import qualified Filesystem.Path.CurrentOS        as Filesystem
import qualified NeatInterpolation

{-| Constants for the calculus of constructions

    The only axiom is:

> ⊦ * : □

    ... and all four rule pairs are valid:

> ⊦ * ↝ * : *
> ⊦ □ ↝ * : *
> ⊦ * ↝ □ : □
> ⊦ □ ↝ □ : □

-}
data Const = Star | Box deriving (Eq, Show, Bounded, Enum)

instance Buildable Const where
    build Star = "Type"
    build Box  = "Kind"

axiom :: Const -> Either TypeError Const
axiom Star = return Box
axiom Box  = Left (TypeError Context.empty (Const Box) (Untyped Box))

rule :: Const -> Const -> Either TypeError Const
rule Star Box  = return Box
rule Star Star = return Star
rule Box  Box  = return Box
rule Box  Star = return Star

-- | Path to an external resource
data Path
    = File FilePath
    | URL  Text
    deriving (Eq, Ord, Show)

instance Buildable Path where
    build (File file)
        |  Text.isPrefixOf  "./" txt
        || Text.isPrefixOf   "/" txt
        || Text.isPrefixOf "../" txt
        = build txt <> " "
        | otherwise
        = "./" <> build txt <> " "
      where
        txt = Text.fromStrict (either id id (Filesystem.toText file))
    build (URL  str ) = build str <> " "

-- | Like `Data.Void.Void`, except with a shorter inferred type
newtype X = X { absurd :: forall a . a }

instance Eq X where
    _ == _ = True

instance Show X where
    show = absurd

instance Buildable X where
    build = absurd

{-|
> Let f [(x1, t1), (x2, t2)] r  ~ let f (x1 : t1) (x2 : t2) = r
-}
data Let a = Let
    { letName :: Text
    , letArgs :: [(Text, Expr a)]
    , letRhs  :: Expr a
    } deriving (Functor, Foldable, Traversable, Show)

instance Buildable a => Buildable (Let a) where
    build (Let f as r) =
            "let "
        <>  build f
        <>  " "
        <>  foldMap (\(x, t) -> "(" <> build x <> " : " <> build t <> ")") as
        <>  "= "
        <>  build r

-- | Syntax tree for expressions
data Expr a
    -- | > Const c                         ~  c
    = Const Const
    -- | > Var x                           ~  x
    | Var Text
    -- | > Lam x     A b                   ~  λ(x : A) → b
    | Lam Text (Expr a) (Expr a)
    -- | > Pi x      A B                   ~  ∀(x : A) → B
    --   > Pi unused A B                   ~        A  → B
    | Pi  Text (Expr a) (Expr a)
    -- | > App f a                         ~  f a
    | App (Expr a) (Expr a)
    -- | > Lets [l1, l2] e                 ~  l1 l2 in e
    | Lets [Let a] (Expr a)
    -- | > Annot x t                       ~  x : t
    | Annot (Expr a) (Expr a)
    -- | > Bool                            ~  Bool
    | Bool
    -- | > BoolLit b                       ~  b
    | BoolLit Bool
    -- | > BoolAnd x y                     ~  x && y
    | BoolAnd (Expr a) (Expr a)
    -- | > BoolOr  x y                     ~  x || y
    | BoolOr  (Expr a) (Expr a)
    -- | > BoolIf                          ~  if
    | BoolIf (Expr a) (Expr a) (Expr a)
    -- | > Natural                         ~  Natural
    | Natural
    -- | > NaturalLit n                    ~  +n
    | NaturalLit Natural
    -- | > NaturalFold                     ~  Natural/fold
    | NaturalFold
    -- | > NaturalPlus x y                 ~  x + y
    | NaturalPlus (Expr a) (Expr a)
    -- | > NaturalTimes x y                ~  x * y
    | NaturalTimes (Expr a) (Expr a)
    -- | > Integer                         ~  Integer
    | Integer
    -- | > IntegerLit n                    ~  n
    | IntegerLit Integer
    -- | > Double                          ~  Double
    | Double
    -- | > DoubleLit n                     ~  n
    | DoubleLit Double
    -- | > Text                            ~  Text
    | Text
    -- | > TextLit t                       ~  t
    | TextLit Text
    -- | > TextAppend x y                  ~  x ++ y
    | TextAppend (Expr a) (Expr a)
    -- | > Maybe a                         ~  Maybe a
    | Maybe (Expr a)
    -- | > Nothing                         ~  Nothing
    | Nothing_
    -- | > Just_                           ~  Just
    | Just_
    -- | > List t                          ~  [ t ]
    | List (Expr a)
    -- | > ListLit t [x, y, z]             ~  [ x, y, z : t ]
    | ListLit (Expr a) (Vector (Expr a))
    -- | > ListBuild                       ~  List/build
    | ListBuild
    -- | > ListFold                        ~  List/fold
    | ListFold
    -- | > Record    [(k1, t1), (k2, t2)]  ~  { k1 : t1, k2 : t1 }
    | Record    (Map Text (Expr a))
    -- | > RecordLit [(k1, v1), (k2, v2)]  ~  { k1 = v1, k2 = v2 }
    | RecordLit (Map Text (Expr a))
    -- | > Field e x                       ~  e.x
    | Field (Expr a) Text
    -- | > Embed path                      ~  #path
    | Embed a
    deriving (Functor, Foldable, Traversable, Show)

instance Applicative Expr where
    pure = Embed

    (<*>) = Control.Monad.ap

instance Monad Expr where
    return = pure

    Const c          >>= _ = Const c
    Var x            >>= _ = Var x
    Lam x _A  b      >>= k = Lam x (_A >>= k) ( b >>= k)
    Pi  x _A _B      >>= k = Pi  x (_A >>= k) (_B >>= k)
    App f a          >>= k = App (f >>= k) (a >>= k)
    Lets ls e        >>= k = Lets ls' (e >>= k)
      where
        ls' = do
            Let f as r  <- ls
            let as' = do
                    (x, t) <- as
                    return (x, t >>= k)
            return (Let f as' (r >>= k))
    Annot x t        >>= k = Annot (x >>= k) (t >>= k)
    Bool             >>= _ = Bool
    BoolLit b        >>= _ = BoolLit b
    BoolAnd l r      >>= k = BoolAnd (l >>= k) (r >>= k)
    BoolOr  l r      >>= k = BoolOr  (l >>= k) (r >>= k)
    BoolIf x y z     >>= k = BoolIf (x >>= k) (y >>= k) (z >>= k)
    Natural          >>= _ = Natural
    NaturalLit n     >>= _ = NaturalLit n
    NaturalFold      >>= _ = NaturalFold
    NaturalPlus  l r >>= k = NaturalPlus  (l >>= k) (r >>= k)
    NaturalTimes l r >>= k = NaturalTimes (l >>= k) (r >>= k)
    Integer          >>= _ = Integer
    IntegerLit n     >>= _ = IntegerLit n
    Double           >>= _ = Double
    DoubleLit n      >>= _ = DoubleLit n
    Text             >>= _ = Text
    TextLit t        >>= _ = TextLit t
    TextAppend l r   >>= k = TextAppend (l >>= k) (r >>= k)
    Maybe t          >>= k = Maybe (t >>= k)
    Nothing_         >>= _ = Nothing_
    Just_            >>= _ = Just_
    List t           >>= k = List (t >>= k)
    ListLit t es     >>= k = ListLit (t >>= k) es'
      where
        es' = do
            e <- es
            return (e >>= k)
    ListBuild       >>= _ = ListBuild
    ListFold        >>= _ = ListFold
    Record    kts   >>= k = Record (Data.Map.fromAscList kts')
      where
        kts' = [ (k', t >>= k) | (k', t) <- Data.Map.toAscList kts ]
    RecordLit kvs   >>= k = RecordLit (Data.Map.fromAscList kvs')
      where
        kvs' = [ (k', v >>= k) | (k', v) <- Data.Map.toAscList kvs ]
    Field r x       >>= k = Field (r >>= k) x
    Embed r         >>= k = k r

match :: Text -> Text -> [(Text, Text)] -> Bool
match xL xR  []              = xL == xR
match xL xR ((xL', xR'):xs)
    | xL == xL' && xR == xR' = True
    | xL == xL'              = False
    |              xR == xR' = False
    | otherwise              = match xL xR xs

instance Eq a => Eq (Expr a) where
    eL0 == eR0 = State.evalState (go (normalize eL0) (normalize eR0)) []
      where
--      go :: Expr a -> Expr a -> State [(Text, Text)] Bool
        go (Const cL) (Const cR) = return (cL == cR)
        go (Var xL) (Var xR) = do
            ctx <- State.get
            return (match xL xR ctx)
        go (Lam xL tL bL) (Lam xR tR bR) = do
            ctx <- State.get
            eq1 <- go tL tR
            if eq1
                then do
                    State.put ((xL, xR):ctx)
                    eq2 <- go bL bR
                    State.put ctx
                    return eq2
                else return False
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
        go (BoolLit x) (BoolLit y) = return (x == y)
        go (BoolAnd xL yL) (BoolAnd xR yR) = do
            b <- go xL xR
            if b then go yL yR else return False
        go (BoolOr xL yL) (BoolOr xR yR) = do
            b <- go xL xR
            if b then go yL yR else return False
        go Natural Natural = return True
        go (NaturalLit x) (NaturalLit y) = return (x == y)
        go Integer Integer = return True
        go (IntegerLit x) (IntegerLit y) = return (x == y)
        go Double Double = return True
        go (DoubleLit x) (DoubleLit y) = return (x == y)
        go Text Text = return True
        go (TextLit x) (TextLit y) = return (x == y)
        go (TextAppend xL yL) (TextAppend xR yR) = do
            b1 <- go xL xR
            if b1 then go yL yR else return False
        go (Maybe tL) (Maybe tR) = go tL tR
        go Nothing_ Nothing_ = return True
        go Just_ Just_ = return True
        go (List tL) (List tR) = go tL tR
        go (ListLit tL esL) (ListLit tR esR) = do
            b1 <- go tL tR
            if b1
                then fmap and (Data.Vector.zipWithM go esL esR)
                else return False
        go ListBuild ListBuild = return True
        go ListFold ListFold = return True
        go NaturalFold NaturalFold = return True
        go (NaturalPlus xL yL) (NaturalPlus xR yR) = do
            b <- go xL xR
            if b then go yL yR else return False
        go (NaturalTimes xL yL) (NaturalTimes xR yR) = do
            b <- go xL xR
            if b then go yL yR else return False
        go (Record    ktsL0) (Record    ktsR0) = do
            let loop ((kL, tL):ktsL) ((kR, tR):ktsR)
                    | kL == kR = do
                        b <- go tL tR
                        if b
                            then loop ktsL ktsR
                            else return False
                loop [] [] = return True
                loop _  _  = return False
            loop (Data.Map.toList ktsL0) (Data.Map.toList ktsR0)
        go (RecordLit kvsL0) (RecordLit kvsR0) = do
            let loop ((kL, vL):kvsL) ((kR, vR):kvsR)
                    | kL == kR = do
                        b <- go vL vR
                        if b
                            then loop kvsL kvsR
                            else return False
                    | otherwise = return False
                loop [] [] = return True
                loop _  _  = return False
            loop (Data.Map.toList kvsL0) (Data.Map.toList kvsR0)
        go (Embed pL) (Embed pR) = return (pL == pR)
        go (Field rL xL) (Field rR xR)
            | xL == xR  = go rL rR
            | otherwise = return False
        go _ _ = return False

instance IsString (Expr a)
  where
    fromString str = Var (fromString str)

-- | Generates a syntactically valid Dhall program
instance Buildable a => Buildable (Expr a)
  where
    build = go False False
      where
        go parenBind parenApp e = case e of
            Const c          -> build c
            Var x            -> build x
            Lam x _A b       ->
                    (if parenBind then "(" else "")
                <>  "λ("
                <>  build x
                <>  " : "
                <>  go False False _A
                <>  ") → "
                <>  go False False b
                <>  (if parenBind then ")" else "")
            Pi  x _A b       ->
                    (if parenBind then "(" else "")
                <>  (if x /= "_"
                     then "∀(" <> build x <> " : " <> go False False _A <> ")"
                     else go True False _A )
                <>  " → "
                <>  go False False b
                <>  (if parenBind then ")" else "")
            App f a          ->
                    (if parenApp then "(" else "")
                <>  go True False f <> " " <> go True True a
                <>  (if parenApp then ")" else "")
            Lets ls e'       ->
                    (if parenBind then "(" else "")
                <>  foldMap (\l -> build l <> " ") ls
                <>  "in "
                <>  go False False e'
                <>  (if parenBind then ")" else "")
            Annot x t        ->
                    go True False x
                <>  " : "
                <>  go False False t
            Bool             -> "Bool"
            BoolLit b        -> build (show b)
            BoolAnd x y      -> build x <> " && " <> build y
            BoolOr  x y      -> build x <> " || " <> build y
            BoolIf x y z     ->
                    (if parenApp then "(" else "")
                <>  "if "
                <>  go False False x
                <>  " then "
                <>  go False False y
                <>  " else "
                <>  go False False z
                <>  (if parenApp then ")" else "")
            Natural          -> "Natural"
            NaturalLit n     -> "+" <> build (show n)
            NaturalFold      -> "Natural/fold"
            NaturalPlus  x y -> go True False x <> " + " <> go True False y
            NaturalTimes x y -> go True False x <> " * " <> go True False y
            Integer          -> "Integer"
            IntegerLit n     -> build (show n)
            Double           -> "Double"
            DoubleLit n      -> build (show n)
            Text             -> "Text"
            TextLit t        -> build (show t)
            TextAppend x y   -> go True False x <> " ++ " <> go True False y
            Maybe t          ->
                    (if parenApp then "(" else "")
                <>  "Maybe "
                <>  go True True t
                <>  (if parenApp then ")" else "")
            Nothing_         -> "Nothing"
            Just_            -> "Just"
            List t           -> "[ " <> go False False t <> " ]"
            ListLit t es     ->
                if null es
                then    "[ : " <> go False False t <> " ]"
                else    "[ "
                    <>  mconcat
                            (Data.List.intersperse ", "
                                (fmap (go False False) (toList es)) )
                    <>  " : "
                    <>  go False False t
                    <>  " ]"
            ListBuild        -> "List/build"
            ListFold         -> "List/fold"
            Record kts       ->
                if Data.Map.null kts
                then    "{{ }}"
                else    "{{ "
                    <>  mconcat
                            (Data.List.intersperse ", "
                                [ build k <> " : " <> go False False t
                                | (k, t) <- Data.Map.toList kts
                                ] )
                    <>  " }}"
            RecordLit kvs    ->
                if Data.Map.null kvs
                then    "{ }"
                else    "{ "
                    <>  mconcat
                            (Data.List.intersperse ", "
                                [ build k <> " = " <> go False False v
                                | (k, v) <- Data.Map.toList kvs
                                ] )
                    <>  " }"
            Field r x        -> go True True r <> "." <> build x
            Embed p          -> build p
 
-- | The specific type error
data TypeMessage
    = UnboundVariable
    | InvalidInputType (Expr X)
    | InvalidOutputType (Expr X)
    | NotAFunction (Expr X)
    | TypeMismatch (Expr X) (Expr X)
    | AnnotMismatch (Expr X) (Expr X)
    | Untyped Const
    | InvalidElement Int (Expr X) (Expr X) (Expr X)
    | InvalidMaybeTypeParam (Expr X) (Expr X)
    | InvalidListTypeParam (Expr X) (Expr X)
    | InvalidListType (Expr X) (Expr X)
    | InvalidPredicate (Expr X)
    | IfBranchMismatch (Expr X) (Expr X)
    | InvalidFieldType (Expr X)
    | NotARecord (Expr X)
    | MissingField Text
    | CantAnd (Expr X) (Expr X)
    | CantOr (Expr X) (Expr X)
    | CantAppend (Expr X) (Expr X)
    | CantAdd (Expr X) (Expr X)
    | CantMultiply (Expr X) (Expr X)
    deriving (Show)

instance Buildable TypeMessage where
    build  UnboundVariable           =
        Builder.fromText [NeatInterpolation.text|
Error: Unbound variable

Explanation: Expressions can only reference previously introduced (i.e. "bound")
variables that are still "in scope".  For example, these are valid expressions:

    λ(x : Bool) → x       -- Anonymous functions introduce "bound" variables

    let x = 1 in x        -- `let` definitions introduce "bound" variables

    let f (x : Bool) = x  -- Function arguments are "bound" variables
    in  f True

... but these are not valid expressions:

    λ(x : Bool) → y     -- The variable `y` hasn't been introduced yet

    (let x = 1 in x) x  -- `x` is undefined outside the parentheses

    let x = x in x      -- The definition for `x` cannot reference itself
|]
    build (InvalidInputType expr   ) =
        Builder.fromText [NeatInterpolation.text|
Error: Invalid input annotation for a function

Explanation: A function can accept an input value of a given "type", like this:

    ∀(x : Text) → Bool -- This function accepts any value of type `Text`.
                       -- `x` is the value's name and `Text` is the value's type

    Bool → Integer     -- This function accepts any value of type `Bool`.
                       -- The input value's name is omitted

... or accept an input "type" of a given "kind", like this:

    ∀(a : Type) → Type  -- This accepts any type `a` of kind `Type`

Other input annotations are *not* valid, like this:

    ∀(x : 1) → x        -- `1` is a value and not a "type" nor a "kind"

This input annotation you gave is neither a type nor a kind:
↳ $txt
|]
      where
        txt = Text.toStrict (pretty expr)
    build (InvalidOutputType expr  ) =
        Builder.fromText [NeatInterpolation.text|
Error: Invalid output annotation for a function

Explanation: A function can emit an output value of a given "type", like this:

    ∀(x : Text) → Bool  -- This function emits a value of type `Text`.

    Bool → Int          -- This function emits a value of type `Int`.

... or emit an output "type" of a given "kind", like this:

    ∀(a : Type) → Type  -- This emits a type of kind `Type`

Other outputs are *not* valid, like this:

    ∀(x : Text) → 1     -- `1` is a value and not a "type" nor a "kind"

This function output you specified is neither a type nor a kind:
↳ $txt
|]
      where
        txt = Text.toStrict (pretty expr)
    build (NotAFunction expr       ) =
        Builder.fromText [NeatInterpolation.text|
Error: Only functions may be applied to arguments

Explanation: Expressions separated by whitespace denote function application.
For example:

    f x  -- This denotes the function `f` applied to an argument `x`

However, not everything is a valid function.  For example:

    1                         -- Primitive values are not functions
    Text                      -- Primitive types are not functions
    Type                      -- Primitive kinds are not functions
    { foo = 1, bar = "ABC" }  -- Records are not functions

You tried to apply an expression that was not a function to an argument

This is the expression that you incorrectly treated as a function:
↳ $txt
|]
      where
        txt = Text.toStrict (pretty expr)
    build (TypeMismatch expr1 expr2) =
            "Error: Function applied to argument of the wrong type\n"
        <>  "\n"
        <>  "Expected type: " <> build expr1 <> "\n"
        <>  "Argument type: " <> build expr2 <> "\n"
    build (AnnotMismatch expr1 expr2) =
            "Error: Expression's inferred type does not match annotated type\n"
        <>  "\n"
        <>  "Annotated type: " <> build expr1 <> "\n"
        <>  "Inferred  type: " <> build expr2 <> "\n"
    build (Untyped c               ) =
            "Error: " <> build c <> " has no type\n"
    build (InvalidMaybeTypeParam t k     ) =
            "Error: Invalid optional type\n"
        <>  "\n"
            -- TODO: Wrap in parentheses if necessary
        <>  "Invalid type: Maybe " <> build t <> "\n"
        <>  "                    ^\n"
        <>  "Expected kind: *\n"
        <>  "Actual   kind: " <> build k <> "\n"
    build (InvalidListTypeParam t k     ) =
            "Error: Invalid list type\n"
        <>  "\n"
        <>  "Invalid type: [ " <> build t <> " ]\n"
        <>  "                ^\n"
        <>  "Expected kind: *\n"
        <>  "Actual   kind: " <> build k <> "\n"
    build (InvalidPredicate t      ) =
            "Error: Invalid predicate for `if`\n"
        <>  "\n"
        <>  "Expected predicate type: Bool\n"
        <>  "Inferred predicate type: " <> build t <> "\n"
    build (IfBranchMismatch l r    ) =
            "Error: The `then` and `else` branches have different types\n"
        <>  "\n"
        <>  "Type of `then` branch: " <> build l <> "\n"
        <>  "Type of `else` branch: " <> build r <> "\n"
    build (InvalidListType t k     ) =
            "Error: Type of the wrong kind for list elements\n"
        <>  "\n"
        <>  "Invalid type: [ ... : " <> build t <> " ]\n"
        <>  "                      ^\n"
        <>  "Expected kind: *\n"
        <>  "Actual   kind: " <> build k <> "\n"
    build (InvalidElement n x t t' ) =
            "Error: List with an element of the wrong type\n"
        <>  "\n"
        <>  "Element index  : " <> build n <> "\n"
        <>  "Invalid element: " <> build x <> "\n"
        <>  "\n"
        <>  "Expected type: " <> build t  <> "\n"
        <>  "Actual   type: " <> build t' <> "\n"
    build (InvalidFieldType  expr  ) =
            "Error: Invalid field type\n"
        <>  "\n"
        <>  "Type: " <> build expr <> "\n"
    build (NotARecord expr         ) =
            "Error: Not a record\n"
        <>  "\n"
        <>  "Value: " <> build expr <> "\n"
    build (MissingField t          ) =
            "Error: Missing field\n"
        <>  "\n"
        <>  "Field: " <> build t <> "\n"
    build (CantAnd e t             ) =
            "Error: Can't use `(&&)` on a value that's not a `Bool`\n"
        <>  "\n"
        <>  "Value: " <> build e <> "\n"
        <>  "Type : " <> build t <> "\n"
    build (CantOr  e t             ) =
            "Error: Can't use `(||)` on a value that's not a `Bool`\n"
        <>  "\n"
        <>  "Value: " <> build e <> "\n"
        <>  "Type : " <> build t <> "\n"
    build (CantAppend e t          ) =
            "Error: Can't append a value that's not `Text`\n"
        <>  "\n"
        <>  "Value: " <> build e <> "\n"
        <>  "Type : " <> build t <> "\n"
    build (CantAdd e t             ) =
            "Error: Can't add a value that's not a `Natural` number\n"
        <>  (case t of
                Integer -> "Hint : You're not allowed to add `Integer`s\n"
                _       -> mempty )
        <>  (case e of
                IntegerLit n ->
                    "Hint : Replace `" <> build n <> "` with `+" <> build n <> "` to provide a `Natural` number\n"
                _            ->
                    mempty )
        <>  "\n"
        <>  "Value: " <> build e <> "\n"
        <>  "Type : " <> build t <> "\n"
    build (CantMultiply e t        ) =
            "Error: Can't multiply a value that's not a `Natural` number\n"
        <>  (case t of
                Integer -> "Hint : You're not allowed to multiply `Integer`s\n"
                _       -> mempty )
        <>  (case e of
                IntegerLit n ->
                    "Hint : Replace `" <> build n <> "` with `+" <> build n <> "` to provide a `Natural` number\n"
                _            ->
                    mempty )
        <>  "\n"
        <>  "Value: " <> build e <> "\n"
        <>  "Type : " <> build t <> "\n"

-- | A structured type error that includes context
data TypeError = TypeError
    { context     :: Context (Expr X)
    , current     :: Expr X
    , typeMessage :: TypeMessage
    } deriving (Typeable)

instance Show TypeError where
    show = Text.unpack . pretty

instance Exception TypeError

instance Buildable TypeError where
    build (TypeError ctx expr msg)
        =   "\n"
        <>  (    if Text.null (Builder.toLazyText (buildContext ctx))
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
            .   Context.toList

{-| Substitute all occurrences of a variable with an expression

> subst x C B  ~  B[x := C]
-}
subst :: Text -> Expr a -> Expr a -> Expr a
subst x e (Lam x' _A  b    ) = Lam x' (subst x e _A)  b'
  where
    b'  = if x == x' then  b else subst x e  b
subst x e (Pi  x' _A _B    ) = Pi  x' (subst x e _A) _B'
  where
    _B' = if x == x' then _B else subst x e _B
subst x e (App f a         ) = App (subst x e f) (subst x e a)
subst x e (Lets ls0 s0     ) = Lets ls0' s0''
  where
    ~(ls0', s0'') = go0 True ls0

    go0 !b              []  = (              [] , s0')
      where
        s0' = if b then subst x e s0 else s0
    go0 !b (Let f as0 r:ls) = (Let f as0' r'':ls', s0')
      where
        ~(ls', s0') = go0 (b && x /= f) ls

        ~(as0', r'') = go1 True as0
          where
            go1 !b'         []  = (        [] , r')
              where
                r' = if b' then subst x e r else r
            go1 !b' ((y, t):as) = ((y, t'):as', r')
              where
                ~(as', r') = go1 (b' && x /= y) as

                t' = if b' then subst x e t else t
subst x e (Annot x' t      ) = Annot (subst x e x') (subst x e t)
subst x e (Var x'          ) = if x == x' then e else Var x'
subst x e (BoolAnd      l r) = BoolAnd      (subst x e l) (subst x e r)
subst x e (BoolOr       l r) = BoolOr       (subst x e l) (subst x e r)
subst x e (BoolIf x' y z   ) = BoolIf (subst x e x') (subst x e y) (subst x e z)
subst x e (NaturalPlus  l r) = NaturalPlus  (subst x e l) (subst x e r)
subst x e (NaturalTimes l r) = NaturalTimes (subst x e l) (subst x e r)
subst x e (TextAppend   l r) = TextAppend   (subst x e l) (subst x e r)
subst x e (Maybe   t       ) = Maybe (subst x e t)
subst x e (List    t       ) = List (subst x e t)
subst x e (ListLit t es    ) = ListLit (subst x e t) es'
  where
    es' = do
        e' <- es
        return (subst x e e')
subst x e (Record    kts   ) = Record (Data.Map.fromAscList kts')
  where
    kts' = [ (k, subst x e t) | (k, t) <- Data.Map.toList kts ]
subst x e (RecordLit kvs   ) = RecordLit (Data.Map.fromAscList kvs')
  where
    kvs' = [ (k, subst x e v) | (k, v) <- Data.Map.toList kvs ]
subst x e (Field r x'      ) = Field (subst x e r) x'
-- The Dhall compiler enforces that all embedded values are closed expressions
subst _ _ (Embed p         ) = Embed p
subst _ _  e                 = e

{-| Type-check an expression and return the expression's type if type-checking
    suceeds or an error if type-checking fails

    `typeWith` does not necessarily normalize the type since full normalization
    is not necessary for just type-checking.  If you actually care about the
    returned type then you may want to `normalize` it afterwards.
-}
typeWith :: Context (Expr X) -> Expr X -> Either TypeError (Expr X)
typeWith _     (Const c         ) = do
    fmap Const (axiom c)
typeWith ctx e@(Var x           ) = do
    case Context.lookup x ctx of
        Nothing -> Left (TypeError ctx e UnboundVariable)
        Just a  -> return a
typeWith ctx   (Lam x _A  b     ) = do
    _B <- typeWith (Context.insert x _A ctx) b
    let p = Pi x _A _B
    _t <- typeWith ctx p
    return p
typeWith ctx e@(Pi  x _A _B     ) = do
    tA <- fmap normalize (typeWith ctx _A)
    kA <- case tA of
        Const k -> return k
        _       -> Left (TypeError ctx e (InvalidInputType _A))

    let ctx' = Context.insert x _A ctx
    tB <- fmap normalize (typeWith ctx' _B)
    kB <- case tB of
        Const k -> return k
        _       -> Left (TypeError ctx' e (InvalidOutputType _B))

    fmap Const (rule kA kB)
typeWith ctx e@(App f a         ) = do
    tf <- fmap normalize (typeWith ctx f)
    (x, _A, _B) <- case tf of
        Pi x _A _B -> return (x, _A, _B)
        _          -> Left (TypeError ctx e (NotAFunction f))
    _A' <- typeWith ctx a
    if _A == _A'
        then do
            return (subst x a _B)
        else do
            let nf_A  = normalize _A
            let nf_A' = normalize _A'
            Left (TypeError ctx e (TypeMismatch nf_A nf_A'))
typeWith ctx   (Lets ls0 e'     ) = do
    let go c             []  = typeWith c e'
        go c (Let f as r:ls) = do
            let r' = foldr (\(x, _A) b -> Lam x _A b) r as
            tr <- typeWith c r'
            go (Context.insert f tr c) ls
    go ctx ls0
typeWith ctx e@(Annot x t       ) = do
    t' <- typeWith ctx x
    if t == t'
        then do
            return t
        else do
            let nf_t  = normalize t
            let nf_t' = normalize t'
            Left (TypeError ctx e (AnnotMismatch nf_t nf_t'))
typeWith _      Bool              = do
    return (Const Star)
typeWith _     (BoolLit _       ) = do
    return Bool
typeWith ctx e@(BoolAnd l r     ) = do
    tl <- fmap normalize (typeWith ctx l)
    case tl of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantAnd l tl))

    tr <- fmap normalize (typeWith ctx r)
    case tr of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantAnd r tr))

    return Bool
typeWith ctx e@(BoolOr  l r     ) = do
    tl <- fmap normalize (typeWith ctx l)
    case tl of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantOr l tl))

    tr <- fmap normalize (typeWith ctx r)
    case tr of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantOr r tr))

    return Bool
typeWith ctx e@(BoolIf x y z    ) = do
    tx <- fmap normalize (typeWith ctx x)
    case tx of
        Bool -> return ()
        _    -> Left (TypeError ctx e (InvalidPredicate tx))
    ty <- fmap normalize (typeWith ctx y)
    tz <- fmap normalize (typeWith ctx z)
    if ty == tz
        then return ()
        else Left (TypeError ctx e (IfBranchMismatch ty tz))
    return ty
typeWith _      Natural           = do
    return (Const Star)
typeWith _     (NaturalLit _    ) = do
    return Natural
typeWith _      NaturalFold       = do
    return
        (Pi "_" Natural
            (Pi "natural" (Const Star)
                (Pi "succ" (Pi "pred" "natural" "natural")
                    (Pi "zero" "natural" "natural") ) ) )
typeWith ctx e@(NaturalPlus  l r) = do
    tl <- fmap normalize (typeWith ctx l)
    case tl of
        Natural -> return ()
        _       -> Left (TypeError ctx e (CantAdd l tl))

    tr <- fmap normalize (typeWith ctx r)
    case tr of
        Natural -> return ()
        _       -> Left (TypeError ctx e (CantAdd r tr))
    return Natural
typeWith ctx e@(NaturalTimes l r) = do
    tl <- fmap normalize (typeWith ctx l)
    case tl of
        Natural -> return ()
        _       -> Left (TypeError ctx e (CantMultiply l tl))

    tr <- fmap normalize (typeWith ctx r)
    case tr of
        Natural -> return ()
        _       -> Left (TypeError ctx e (CantMultiply r tr))
    return Natural
typeWith _      Integer           = do
    return (Const Star)
typeWith _     (IntegerLit _    ) = do
    return Integer
typeWith _      Double            = do
    return (Const Star)
typeWith _     (DoubleLit _     ) = do
    return Double
typeWith _      Text              = do
    return (Const Star)
typeWith _     (TextLit _       ) = do
    return Text
typeWith ctx e@(TextAppend l r  ) = do
    tl <- fmap normalize (typeWith ctx l)
    case tl of
        Text -> return ()
        _    -> Left (TypeError ctx e (CantAppend l tl))

    tr <- fmap normalize (typeWith ctx r)
    case tr of
        Text -> return ()
        _    -> Left (TypeError ctx e (CantAppend r tr))
    return Text
typeWith ctx e@(Maybe t         ) = do
    s <- fmap normalize (typeWith ctx t)
    case s of
        Const Star -> return ()
        _          -> Left (TypeError ctx e (InvalidMaybeTypeParam t s))
    return (Const Star)
typeWith _      Nothing_          = do
    return (Pi "a" (Const Star) (Maybe "a"))
typeWith _      Just_             = do
    return (Pi "a" (Const Star) (Pi "_" "a" (Maybe "a")))
typeWith ctx e@(List t          ) = do
    s <- fmap normalize (typeWith ctx t)
    case s of
        Const Star -> return ()
        _          -> Left (TypeError ctx e (InvalidListTypeParam t s))
    return (Const Star)
typeWith ctx e@(ListLit t xs    ) = do
    s <- fmap normalize (typeWith ctx t)
    if s == Const Star
        then return ()
        else Left (TypeError ctx e (InvalidListType t s))
    flip Data.Vector.imapM_ xs (\n x -> do
        t' <- typeWith ctx x
        if t == t'
            then return ()
            else do
                let nf_t  = normalize t
                let nf_t' = normalize t'
                Left (TypeError ctx e (InvalidElement n x nf_t nf_t')) )
    return (List t)
typeWith _      ListBuild         = do
    return
        (Pi "a" (Const Star)
            (Pi "_"
                (Pi "list" (Const Star)
                    (Pi "cons" (Pi "head" "a" (Pi "tail" "list" "list"))
                        (Pi "nil" "list" "list") ) )
                (List "a") ) )
typeWith _      ListFold          = do
    return
        (Pi "a" (Const Star)
            (Pi "_" (List "a")
                (Pi "list" (Const Star)
                    (Pi "cons" (Pi "head" "a" (Pi "tail" "list" "list"))
                        (Pi "nil" "list" "list")) ) ) )
typeWith ctx e@(Record    kts   ) = do
    let process (_, t) = do
            s <- fmap normalize (typeWith ctx t)
            case normalize s of
                Const Star -> return ()
                _          -> Left (TypeError ctx e (InvalidFieldType t))
    mapM_ process (Data.Map.toList kts)
    return (Const Star)
typeWith ctx   (RecordLit kvs   ) = do
    let process (k, v) = do
            t <- typeWith ctx v
            return (k, t)
    kts <- mapM process (Data.Map.toAscList kvs)
    return (Record (Data.Map.fromAscList kts))
typeWith ctx e@(Field r x       ) = do
    t <- fmap normalize (typeWith ctx r)
    case t of
        Record kts ->
            case Data.Map.lookup x kts of
                Just t' -> return t'
                Nothing -> Left (TypeError ctx e (MissingField x))
        _          -> Left (TypeError ctx e (NotARecord r))
typeWith _     (Embed p         ) = do
    absurd p

{-| `typeOf` is the same as `typeWith` with an empty context, meaning that the
    expression must be closed (i.e. no free variables), otherwise type-checking
    will fail.
-}
typeOf :: Expr X -> Either TypeError (Expr X)
typeOf = typeWith Context.empty

{-| Reduce an expression to its normal form, performing both beta reduction and
    eta reduction

    `normalize` does not type-check the expression.  You may want to type-check
    expressions before normalizing them since normalization can convert an
    ill-typed expression into a well-typed expression.
-}
normalize :: Expr a -> Expr a
normalize e = case e of
    Lam x _A  b -> Lam x (normalize _A) (normalize  b)
    Pi  x _A _B -> Pi  x (normalize _A) (normalize _B)
    App f a -> case normalize f of
        Lam x _A b -> normalize (subst x (normalize a) b)  -- Beta reduce
        f' -> case App f' a' of
            App (App (App (App NaturalFold (NaturalLit n0)) _) succ') zero ->
                normalize (go n0)
              where
                go !0 = zero
                go !n = App succ' (go (n - 1))
            App (App ListBuild t) k
                | check     -> ListLit t (buildVector k')
                | otherwise -> App f' a'
              where
                labeled = normalize (App (App (App k (List t)) "Cons") "Nil")

                k' cons nil = go labeled
                  where
                    go (App (App (Var "Cons") x) e') = cons x (go e')
                    go (Var "Nil")                   = nil
                    go  _                            =
                        error "normalize: Malformed `build`"

                check = go labeled
                  where
                    go (App (App (Var "Cons") _) e') = go e'
                    go (Var "Nil")                   = True
                    go  _                            = False
            App (App (App (App (App ListFold _) (ListLit _ xs)) _) cons) nil ->
                normalize (Data.Vector.foldr cons' nil xs)
              where
                cons' y ys = App (App cons y) ys
            _ -> App f' a'
          where
            a' = normalize a
    Lets ls i0 -> normalize (foldr cons i0 ls)
      where
        cons (Let f as r) i = subst f r' i
          where
            r' = foldr (\(x, _A) b -> Lam x _A b) r as
    Annot x _ -> normalize x
    BoolAnd x y ->
        case x' of
            BoolLit xn ->
                case y' of
                    BoolLit yn -> BoolLit (xn && yn)
                    _ -> BoolAnd x' y'
            _ -> BoolAnd x' y'
      where
        x' = normalize x
        y' = normalize y
    BoolOr x y ->
        case x' of
            BoolLit xn ->
                case y' of
                    BoolLit yn -> BoolLit (xn || yn)
                    _ -> BoolOr x' y'
            _ -> BoolOr x' y'
      where
        x' = normalize x
        y' = normalize y
    BoolIf (BoolLit True) true _ ->
        normalize true
    BoolIf (BoolLit False) _ false ->
        normalize false
    NaturalPlus  x y ->
        case x' of
            NaturalLit xn ->
                case y' of
                    NaturalLit yn -> NaturalLit (xn + yn)
                    _ -> NaturalPlus x' y'
            _ -> NaturalPlus x' y'
      where
        x' = normalize x
        y' = normalize y
    NaturalTimes x y ->
        case x' of
            NaturalLit xn ->
                case y' of
                    NaturalLit yn -> NaturalLit (xn * yn)
                    _ -> NaturalTimes x' y'
            _ -> NaturalTimes x' y'
      where
        x' = normalize x
        y' = normalize y
    TextAppend x y   ->
        case x' of
            TextLit xt ->
                case y' of
                    TextLit yt -> TextLit (xt <> yt)
                    _ -> TextAppend x' y'
            _ -> TextAppend x' y'
      where
        x' = normalize x
        y' = normalize y
    Maybe t          -> Maybe (normalize t)
    List t           -> List (normalize t)
    ListLit t es     -> ListLit (normalize t) (fmap normalize es)
    RecordLit kvs    -> RecordLit (fmap normalize kvs)
    Record    kts    -> Record    (fmap normalize kts)
    Field r x        ->
        case normalize r of
            RecordLit kvs ->
                case Data.Map.lookup x kvs of
                    Just v  -> normalize v
                    Nothing -> Field (RecordLit (fmap normalize kvs)) x
            r' -> Field r' x
    _ -> e

buildVector :: (forall x . (a -> x -> x) -> x -> x) -> Vector a
buildVector f = Data.Vector.reverse (Data.Vector.create (do
    let cons a st = do
            (len, cap, mv) <- st
            if len < cap
                then do
                    Data.Vector.Mutable.write mv len a
                    return (len + 1, cap, mv)
                else do
                    let cap' = 2 * cap
                    mv' <- Data.Vector.Mutable.unsafeGrow mv cap'
                    Data.Vector.Mutable.write mv' len a
                    return (len + 1, cap', mv')
    let nil = do
            mv <- Data.Vector.Mutable.unsafeNew 1
            return (0, 1, mv)
    (len, _, mv) <- f cons nil
    return (Data.Vector.Mutable.slice 0 len mv) ))

-- | Pretty-print a value
pretty :: Buildable a => a -> Text
pretty = Builder.toLazyText . build
