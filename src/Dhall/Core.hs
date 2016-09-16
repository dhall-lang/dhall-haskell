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
import Control.Monad.Trans.State.Strict (State)
import Data.Foldable
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text.Buildable (Buildable(..))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Traversable
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Dhall.Context (Context)
import Filesystem.Path.CurrentOS (FilePath)
import Numeric.Natural (Natural)
import Prelude hiding (FilePath)

import qualified Control.Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
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

    ... and the valid rule pairs are:

> ⊦ * ↝ * : *
> ⊦ □ ↝ * : *
> ⊦ □ ↝ □ : □

-}
data Const = Type | Kind deriving (Show, Bounded, Enum)

instance Buildable Const where
    build Type = "Type"
    build Kind = "Kind"

axiom :: Const -> Either TypeError Const
axiom Type = return Kind
axiom Kind = Left (TypeError Context.empty (Const Kind) (Untyped Kind))

rule :: Const -> Const -> Either () Const
rule Type Kind = Left ()
rule Type Type = return Type
rule Kind Kind = return Kind
rule Kind Type = return Type

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
    -- | > Lam x     A b                   ~  \(x : A) -> b
    | Lam Text (Expr a) (Expr a)
    -- | > Pi x      A B                   ~  forall (x : A) -> B
    --   > Pi unused A B                   ~              A  -> B
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
    -- | > TextAppend x y                  ~  x <> y
    | TextAppend (Expr a) (Expr a)
    -- | > List                            ~  List
    | List
    -- | > ListLit t [x, y, z]             ~  [x, y, z] : List t
    | ListLit (Expr a) (Vector (Expr a))
    -- | > ListBuild                       ~  List/build
    | ListBuild
    -- | > ListFold                        ~  List/fold
    | ListFold
    -- | > ListFirst                       ~  List/first
    | ListFirst
    -- | > ListLast                        ~  List/last
    | ListLast
    -- | > ListIndexed                     ~  List/indexed
    | ListIndexed
    -- | > ListConcat x y                  ~  x ++ y
    | ListConcat (Expr a) (Expr a)
    -- | > Maybe                           ~  Maybe
    | Maybe
    -- | > MaybeLit t [e]                  ~  [e] : Maybe t
    -- | > MaybeLit t []                   ~  []  : Maybe t
    | MaybeLit (Expr a) (Vector (Expr a))
    -- | > MaybeFold                       ~  Maybe/fold
    | MaybeFold
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
    List             >>= _ = List
    ListLit t es     >>= k = ListLit (t >>= k) (fmap (>>= k) es)
    ListBuild        >>= _ = ListBuild
    ListFold         >>= _ = ListFold
    ListFirst        >>= _ = ListFirst
    ListLast         >>= _ = ListLast
    ListIndexed      >>= _ = ListIndexed
    ListConcat l r   >>= k = ListConcat (l >>= k) (r >>= k)
    Maybe            >>= _ = Maybe
    MaybeLit t es    >>= k = MaybeLit (t >>= k) (fmap (>>= k) es)
    MaybeFold        >>= _ = MaybeFold
    Record    kts    >>= k = Record (Data.Map.fromAscList kts')
      where
        kts' = [ (k', t >>= k) | (k', t) <- Data.Map.toAscList kts ]
    RecordLit kvs   >>= k = RecordLit (Data.Map.fromAscList kvs')
      where
        kvs' = [ (k', v >>= k) | (k', v) <- Data.Map.toAscList kvs ]
    Field r x        >>= k = Field (r >>= k) x
    Embed r          >>= k = k r

match :: Text -> Text -> [(Text, Text)] -> Bool
match xL xR  []              = xL == xR
match xL xR ((xL', xR'):xs)
    | xL == xL' && xR == xR' = True
    | xL == xL'              = False
    |              xR == xR' = False
    | otherwise              = match xL xR xs

propEqual :: Expr X -> Expr X -> Bool
propEqual eL0 eR0 = State.evalState (go (normalize eL0) (normalize eR0)) []
  where
    go :: Expr X -> Expr X -> State [(Text, Text)] Bool
    go (Const Type) (Const Type) = return True
    go (Const Kind) (Const Kind) = return True
    go (Var xL) (Var xR) = do
        ctx <- State.get
        return (match xL xR ctx)
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
    go _ _ = return False

instance IsString (Expr a)
  where
    fromString str = Var (fromString str)

buildExpr0 :: Buildable a => Expr a -> Builder
buildExpr0 (Annot a b) =
    buildExpr1 a <> " : " <> buildExpr0 b
buildExpr0 a =
    buildExpr1 a

buildExpr1 :: Buildable a => Expr a -> Builder
buildExpr1 (Lam a b c) =
        "\\("
    <>  build a
    <> " : "
    <> buildExpr0 b
    <> ") -> "
    <> buildExpr1 c
buildExpr1 (BoolIf a b c) =
        "if "
    <>  buildExpr0 a
    <>  " then "
    <>  buildExpr1 b
    <>  " else "
    <> buildExpr1 c
buildExpr1 (Pi "_" b c) =
        buildExpr2 b
    <>  " -> "
    <>  buildExpr1 c
buildExpr1 (Pi a b c) =
        "forall ("
    <>  build a
    <>  " : "
    <>  buildExpr0 b
    <>  ") -> "
    <>  buildExpr1 c
buildExpr1 (Lets a b) =
        buildLets a
    <>  " in "
    <>  buildExpr1 b
buildExpr1 (ListLit a b) =
    "[" <> buildElems (Data.Vector.toList b) <> "] : List "  <> buildExpr5 a
buildExpr1 (MaybeLit a b) =
    "[" <> buildElems (Data.Vector.toList b) <> "] : Maybe "  <> buildExpr5 a
buildExpr1 a =
    buildExpr2 a

buildExpr2 :: Buildable a => Expr a -> Builder
buildExpr2 (BoolOr      a b) = buildExpr2 a <> " || " <> buildExpr2 b
buildExpr2 (NaturalPlus a b) = buildExpr2 a <> " + "  <> buildExpr2 b
buildExpr2 (TextAppend  a b) = buildExpr2 a <> " <> " <> buildExpr2 b
buildExpr2 (ListConcat  a b) = buildExpr2 a <> " ++ " <> buildExpr2 b
buildExpr2  a                = buildExpr3 a

buildExpr3 :: Buildable a => Expr a -> Builder
buildExpr3 (BoolAnd      a b) = buildExpr3 a <> " && " <> buildExpr3 b
buildExpr3 (NaturalTimes a b) = buildExpr3 a <> " * "  <> buildExpr3 b
buildExpr3  a                 = buildExpr4 a

buildExpr4 :: Buildable a => Expr a -> Builder
buildExpr4 (App a b) = buildExpr4 a <> " " <> buildExpr5 b
buildExpr4  a        = buildExpr5 a

buildExpr5 :: Buildable a => Expr a -> Builder
buildExpr5 (Var a) =
    build a
buildExpr5 (Const Type) =
    "Type"
buildExpr5 (Const Kind) =
    "Kind"
buildExpr5 Bool =
    "Bool"
buildExpr5 Natural =
    "Natural"
buildExpr5 NaturalFold =
    "Natural/fold"
buildExpr5 Integer =
    "Integer"
buildExpr5 Double =
    "Double"
buildExpr5 Text =
    "Text"
buildExpr5 ListBuild =
    "List/build"
buildExpr5 ListFold =
    "List/fold"
buildExpr5 ListFirst =
    "List/first"
buildExpr5 ListLast =
    "List/last"
buildExpr5 ListIndexed =
    "List/indexed"
buildExpr5 List =
    "List"
buildExpr5 Maybe =
    "Maybe"
buildExpr5 MaybeFold =
    "Maybe/fold"
buildExpr5 (BoolLit True) =
    "True"
buildExpr5 (BoolLit False) =
    "False"
buildExpr5 (IntegerLit a) =
    build (show a)
buildExpr5 (NaturalLit a) =
    "+" <> build (show a)
buildExpr5 (DoubleLit a) =
    build (show a)
buildExpr5 (TextLit a) =
    build (show a)
buildExpr5 (RecordLit a) =
    buildRecordLit a
buildExpr5 (Record a) =
    buildRecord a
buildExpr5 (Embed a) =
    build a
buildExpr5 (Field a b) =
    buildExpr5 a <> "." <> build b
buildExpr5 a =
    "(" <> buildExpr0 a <> ")"

buildLets :: Buildable a => [Let a] -> Builder
buildLets (a:bs) = buildLet a <> buildLets bs
buildLets    []  = ""

buildLet :: Buildable a => Let a -> Builder
buildLet (Let a b c) =
        "let "
    <>  build a
    <>  " "
    <>  buildArgs b
    <>  "= "
    <>  buildExpr0 c
    <>  " "

buildArgs :: Buildable a => [(Text, Expr a)] -> Builder
buildArgs (a:bs) = buildArg a <> buildArgs bs
buildArgs    []  = ""

buildArg :: Buildable a => (Text, Expr a) -> Builder
buildArg (a, b) = "(" <> build a <> " : " <> buildExpr0 b <> ") "

buildElems :: Buildable a => [Expr a] -> Builder
buildElems   []   = ""
buildElems   [a]  = buildExpr0 a
buildElems (a:bs) = buildExpr0 a <> ", " <> buildElems bs

buildRecordLit :: Buildable a => Map Text (Expr a) -> Builder
buildRecordLit a = "{" <> buildFieldValues (Data.Map.toList a) <> "}"

buildFieldValues :: Buildable a => [(Text, Expr a)] -> Builder
buildFieldValues    []  = ""
buildFieldValues   [a]  = buildFieldValue a
buildFieldValues (a:bs) = buildFieldValue a <> ", " <> buildFieldValues bs

buildFieldValue :: Buildable a => (Text, Expr a) -> Builder
buildFieldValue (a, b) = build a <> " = " <> buildExpr0 b

buildRecord :: Buildable a => Map Text (Expr a) -> Builder
buildRecord a | Data.Map.null a =
    "{1}"
buildRecord a =
    "{" <> buildFieldTypes (Data.Map.toList a) <> "}"

buildFieldTypes :: Buildable a => [(Text, Expr a)] -> Builder
buildFieldTypes    []  = ""
buildFieldTypes   [a]  = buildFieldType a
buildFieldTypes (a:bs) = buildFieldType a <> ", " <> buildFieldTypes bs

buildFieldType :: Buildable a => (Text, Expr a) -> Builder
buildFieldType (a, b) = build a <> " : " <> build b

-- | Generates a syntactically valid Dhall program
instance Buildable a => Buildable (Expr a)
  where
    build = buildExpr0

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
    | NotARecord Text (Expr X) (Expr X)
    | MissingField Text (Expr X)
    | CantAnd Bool (Expr X) (Expr X)
    | CantOr Bool (Expr X) (Expr X)
    | CantAppend Bool (Expr X) (Expr X)
    | CantConcat Bool (Expr X) (Expr X)
    | ElementMismatch (Expr X) (Expr X)
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

    \(x : Bool) -> x      -- Anonymous functions introduce "bound" variables

    let x = 1 in x        -- `let` definitions introduce "bound" variables

    let f (x : Bool) = x  -- Function arguments are "bound" variables
    in  f True

... but these are not valid expressions:

    \(x : Bool) -> y          -- The variable `y` hasn't been introduced yet

    (let x = True in x) && x  -- `x` is undefined outside the parentheses

    let x = x in x            -- The definition for `x` cannot reference itself
|]

    build (InvalidInputType expr) =
        Builder.fromText [NeatInterpolation.text|
Error: Invalid input annotation for a function

Explanation: A function can accept an input term of a given "type", like this:

    forall (x : Text) -> Bool  -- This function accepts any term of type `Text`.
                               -- `x` is the term's name and `Text` is the type

    Bool -> Integer  -- This function accepts any term of type `Bool`.
                     -- The input term's name is omitted

... or accept an input "type" of a given "kind", like this:

    forall (a : Type) -> Type  -- This accepts any type `a` of kind `Type`

Other input annotations are *not* valid, like this:

    forall (x : 1) -> x  -- `1` is a term and not a "type" nor a "kind"

This input annotation you gave is neither a type nor a kind:
↳ $txt
|]
      where
        txt = Text.toStrict (pretty expr)

    build (InvalidOutputType expr) =
        Builder.fromText [NeatInterpolation.text|
Error: Invalid output annotation for a function

Explanation: A function can emit an output term of a given "type", like this:

    forall (x : Text) -> Bool  -- This function emits a term of type `Bool`.

    Bool -> Int                -- This function emits a term of type `Int`.

... or emit an output "type" of a given "kind", like this:

    forall (a : Type) -> Type  -- This emits a type of kind `Type`

Other outputs are *not* valid, like this:

    forall (x : Text) -> 1     -- `1` is a term and not a "type" nor a "kind"

This function output you specified is neither a type nor a kind:
↳ $txt
|]
      where
        txt = Text.toStrict (pretty expr)

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
        txt = Text.toStrict (pretty expr)

    build (TypeMismatch expr0 expr1) =
        Builder.fromText [NeatInterpolation.text|
Error: Function applied to the wrong type or kind of argument

Explanation: Every function declares what type or kind of argument to accept

    \(x : Bool) -> x   -- Anonymous function which only accepts `Bool` arguments

    let f (x : Bool) = x   -- Named function which only accepts `Bool` arguments
    in  f True

    \(a : Type) -> a   -- Anonymous function which only accepts `Type` arguments

You *cannot* apply a function to the wrong type or kind of argument:

    (\(x : Bool) -> x) "A"  -- "A" is `Text`, but the function expects a `Bool`

You tried to invoke a function which expects an argument of type or kind:
↳ $txt0
... on an argument of type or kind:
↳ $txt1
|]
      where
        txt0 = Text.toStrict (pretty expr0)
        txt1 = Text.toStrict (pretty expr1)

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
        txt0 = Text.toStrict (pretty expr0)
        txt1 = Text.toStrict (pretty expr1)
        txt2 = Text.toStrict (pretty expr2)

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
        txt = Text.toStrict (pretty c)

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
        txt0 = Text.toStrict (pretty expr0)
        txt1 = Text.toStrict (pretty expr1)

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
        txt0 = Text.toStrict (pretty expr0)
        txt1 = Text.toStrict (pretty expr1)
        txt2 = Text.toStrict (pretty expr2)
        txt3 = Text.toStrict (pretty expr3)

    build (InvalidListType isEmpty expr0) =
        Builder.fromText [NeatInterpolation.text|
Error: Invalid type for list elements

Explanation: Every list ends with a type annotation for the elements of the list

This annotation must be a type, but the annotation you gave is not a type:

$insert

You can fix the problem by changing the annotation to a type
|]
      where
        txt0 = Text.toStrict (Builder.toLazyText (buildExpr5 expr0))
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
        txt0 = Text.toStrict (pretty expr0)
        txt1 = Text.toStrict (Builder.toLazyText (buildExpr5 expr1))
        txt2 = Text.toStrict (pretty expr2)
        txt3 = Text.toStrict (pretty i    )
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
        txt0 = Text.toStrict (Builder.toLazyText (buildExpr5 expr0))
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
        txt0 = Text.toStrict (pretty expr0)
        txt1 = Text.toStrict (Builder.toLazyText (buildExpr5 expr1))
        txt2 = Text.toStrict (pretty expr2)
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
        txt0 = Text.toStrict (pretty n)

    build (InvalidFieldType k expr0) =
        Builder.fromText [NeatInterpolation.text|
Error: Invalid type of field

Explanation: Every record type has an annotated type for each field

However, fields *cannot* be annotated with expressions other than types

You provided a record type with a key named:
↳ $txt0
... annotated with the following expression which is not a type:

    {{ ... : $txt1, ... }}
    --       ^ This needs to be a type

You can fix the problem by changing the annotation to a type
|]
      where
        txt0 = Text.toStrict (pretty k    )
        txt1 = Text.toStrict (pretty expr0)

    build (NotARecord k expr0 expr1) =
        Builder.fromText [NeatInterpolation.text|
Error: Invalid record access

Explanation: You can only access fields on records, like this:

    { foo = True, bar = "ABC" }.foo               -- This is valid ...

    \(r : {{ foo : Bool, bar : Text }}) -> r.foo  -- ... and so is this

... but you *cannot* access fields on non-record expressions, like this:

    1.foo                   -- `1` is not a valid record

    (\(x : Bool) -> x).foo  -- A function is not a valid record

You tried to access a field named:
↳ $txt0
... on the following expression which is not a record:
↳ $txt1
... but is actually an expression of type:
↳ $txt2
|]
      where
        txt0 = Text.toStrict (pretty k    )
        txt1 = Text.toStrict (pretty expr0)
        txt2 = Text.toStrict (pretty expr1)

    build (MissingField k expr0) =
        Builder.fromText [NeatInterpolation.text|
Error: Missing record field

Explanation: You can only retrieve record fields if they are present

    { foo = True, bar = "ABC" }.foo               -- This is valid ...

    \(r : {{ foo : Bool, bar : Text }}) -> r.foo  -- ... and so is this

... but you *cannot* access fields missing from a record:

    { foo = True, bar = "ABC" }.qux  -- Not valid: the field `qux` is missing

You tried to access a field named:
↳ $txt0
... but the field is missing because the record only defines these fields:
↳ $txt1
|]
      where
        txt0 = Text.toStrict (pretty k    )
        txt1 = Text.toStrict (pretty expr0)

    build (CantAnd b expr0 expr1) =
        buildBooleanOperator "&&" b expr0 expr1

    build (CantOr b expr0 expr1) =
        buildBooleanOperator "||" b expr0 expr1

    build (CantAppend b expr0 expr1) =
        Builder.fromText [NeatInterpolation.text|
Error: Cannot use `(<>)` on a value that's not a `Text`

Explanation: The `(<>)` operator expects two arguments of type `Text`

You provided this argument:

    $insert

... whose type is not `Text`.  The type is actually:
↳ $txt1
|]
      where
        txt0 = Text.toStrict (pretty expr0)
        txt1 = Text.toStrict (pretty expr1)
        insert =
            if b
            then [NeatInterpolation.text|$txt0 <> ...|]
            else [NeatInterpolation.text|... <> $txt0|]

    build (CantConcat b expr0 expr1) =
        Builder.fromText [NeatInterpolation.text|
Error: Cannot use `(++)` on a value that's not a list

Explanation: The `(++)` operator expects two list arguments of type `[ a ]` for
some element type `a`

You provided this argument:

    $insert

... whose type is not a list at all.  The type is actually:
↳ $txt1
|]
      where
        txt0 = Text.toStrict (pretty expr0)
        txt1 = Text.toStrict (pretty expr1)
        insert =
            if b
            then [NeatInterpolation.text|$txt0 ++ ...|]
            else [NeatInterpolation.text|... ++ $txt0|]

    build (ElementMismatch expr0 expr1) =
        Builder.fromText [NeatInterpolation.text|
Error: Can't concatenate lists of different element types

Explanation: You can only combine two lists if they have matching element types.
For example, this is legal:

    [ 1, 2 : Integer] ++ [ 3, 4 : Integer ]  -- The element types match

... but this is *not* legal:

    [ 1, 2 : Integer ] ++ [ True : Bool ] -- The element types do not match

The left list has elements of type:
↳ $txt0
... while the right list has elements of type:
↳ $txt1
... and those two types do not match
|]
      where
        txt0 = Text.toStrict (pretty expr0)
        txt1 = Text.toStrict (pretty expr1)

    build (CantAdd b expr0 expr1) =
        buildNaturalOperator "+" b expr0 expr1

    build (CantMultiply b expr0 expr1) =
        buildNaturalOperator "*" b expr0 expr1

    build (NoDependentTypes expr0 expr1) =
        Builder.fromText [NeatInterpolation.text|
Error: No dependent types

Explanation: This programming language does not allow functions from terms to
types.  For example, this is *not* a legal function type:

    Bool -> Type

Your function type is invalid because the input is a term of type:
↳ $txt0
... and the output is a type of kind:
↳ $txt1
|]
      where
        txt0 = Text.toStrict (pretty expr0)
        txt1 = Text.toStrict (pretty expr1)

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
    txt0 = Text.toStrict (pretty expr0)
    txt1 = Text.toStrict (pretty expr1)
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
    txt0 = Text.toStrict (pretty expr0)
    txt1 = Text.toStrict (pretty expr1)
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
subst x e (ListLit t es    ) = ListLit (subst x e t) (fmap (subst x e) es)
subst x e (ListConcat   l r) = ListConcat (subst x e l) (subst x e r)
subst x e (MaybeLit t es   ) = MaybeLit (subst x e t) (fmap (subst x e) es)
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

    case rule kA kB of
        Left () -> Left (TypeError ctx e (NoDependentTypes _A _B))
        Right k -> Right (Const k)
typeWith ctx e@(App f a         ) = do
    tf <- fmap normalize (typeWith ctx f)
    (x, _A, _B) <- case tf of
        Pi x _A _B -> return (x, _A, _B)
        _          -> Left (TypeError ctx e (NotAFunction f))
    _A' <- typeWith ctx a
    if propEqual _A _A'
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
    if propEqual t t'
        then do
            return t
        else do
            let nf_t  = normalize t
            let nf_t' = normalize t'
            Left (TypeError ctx e (AnnotMismatch x nf_t nf_t'))
typeWith _      Bool              = do
    return (Const Type)
typeWith _     (BoolLit _       ) = do
    return Bool
typeWith ctx e@(BoolAnd l r     ) = do
    tl <- fmap normalize (typeWith ctx l)
    case tl of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantAnd True l tl))

    tr <- fmap normalize (typeWith ctx r)
    case tr of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantAnd False r tr))

    return Bool
typeWith ctx e@(BoolOr  l r     ) = do
    tl <- fmap normalize (typeWith ctx l)
    case tl of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantOr True l tl))

    tr <- fmap normalize (typeWith ctx r)
    case tr of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantOr False r tr))

    return Bool
typeWith ctx e@(BoolIf x y z    ) = do
    tx <- fmap normalize (typeWith ctx x)
    case tx of
        Bool -> return ()
        _    -> Left (TypeError ctx e (InvalidPredicate x tx))
    ty <- fmap normalize (typeWith ctx y)
    tz <- fmap normalize (typeWith ctx z)
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
                (Pi "succ" (Pi "pred" "natural" "natural")
                    (Pi "zero" "natural" "natural") ) ) )
typeWith ctx e@(NaturalPlus  l r) = do
    tl <- fmap normalize (typeWith ctx l)
    case tl of
        Natural -> return ()
        _       -> Left (TypeError ctx e (CantAdd True l tl))

    tr <- fmap normalize (typeWith ctx r)
    case tr of
        Natural -> return ()
        _       -> Left (TypeError ctx e (CantAdd False r tr))
    return Natural
typeWith ctx e@(NaturalTimes l r) = do
    tl <- fmap normalize (typeWith ctx l)
    case tl of
        Natural -> return ()
        _       -> Left (TypeError ctx e (CantMultiply True l tl))

    tr <- fmap normalize (typeWith ctx r)
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
    tl <- fmap normalize (typeWith ctx l)
    case tl of
        Text -> return ()
        _    -> Left (TypeError ctx e (CantAppend True l tl))

    tr <- fmap normalize (typeWith ctx r)
    case tr of
        Text -> return ()
        _    -> Left (TypeError ctx e (CantAppend False r tr))
    return Text
typeWith _      List              = do
    return (Pi "_" (Const Type) (Const Type))
typeWith ctx e@(ListLit t xs    ) = do
    s <- fmap normalize (typeWith ctx t)
    case s of
        Const Type -> return ()
        _ -> Left (TypeError ctx e (InvalidListType (Data.Vector.null xs) t))
    let n = Data.Vector.length xs
    flip Data.Vector.imapM_ xs (\i x -> do
        t' <- typeWith ctx x
        if propEqual t t'
            then return ()
            else do
                let nf_t  = normalize t
                let nf_t' = normalize t'
                Left (TypeError ctx e (InvalidListElement i n x nf_t nf_t')) )
    return (App List t)
typeWith _      ListBuild         = do
    return
        (Pi "a" (Const Type)
            (Pi "_"
                (Pi "list" (Const Type)
                    (Pi "cons" (Pi "head" "a" (Pi "tail" "list" "list"))
                        (Pi "nil" "list" "list") ) )
                (App List "a") ) )
typeWith _      ListFold          = do
    return
        (Pi "a" (Const Type)
            (Pi "_" (App List "a")
                (Pi "list" (Const Type)
                    (Pi "cons" (Pi "head" "a" (Pi "tail" "list" "list"))
                        (Pi "nil" "list" "list")) ) ) )
typeWith _      ListFirst         = do
    return (Pi "a" (Const Type) (Pi "_" (App List "a") (App Maybe "a")))
typeWith _      ListLast          = do
    return (Pi "a" (Const Type) (Pi "_" (App List "a") (App Maybe "a")))
typeWith _      ListIndexed       = do
    let kts = [("_1", Natural), ("_2", "a")]
    return
        (Pi "a" (Const Type)
            (Pi "_" (App List "a")
                (App List (Record (Data.Map.fromList kts))) ) )
typeWith ctx e@(ListConcat l r  ) = do
    tl <- fmap normalize (typeWith ctx l)
    el <- case tl of
        App List el -> return el
        _           -> Left (TypeError ctx e (CantConcat True l tl))

    tr <- fmap normalize (typeWith ctx r)
    er <- case tr of
        App List er -> return er
        _           -> Left (TypeError ctx e (CantConcat False r tr))
    if propEqual el er
        then return ()
        else Left (TypeError ctx e (ElementMismatch el er))
    return (App List el)
typeWith _      Maybe             = do
    return (Pi "_" (Const Type) (Const Type))
typeWith ctx e@(MaybeLit t xs   ) = do
    s <- fmap normalize (typeWith ctx t)
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
                let nf_t  = normalize t
                let nf_t' = normalize t'
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
            s <- fmap normalize (typeWith ctx t)
            case normalize s of
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
typeWith ctx e@(Field r x       ) = do
    t <- fmap normalize (typeWith ctx r)
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
                labeled =
                    normalize (App (App (App k (App List t)) "Cons") "Nil")

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
            App (App ListFirst _) (ListLit t ys) ->
                normalize (MaybeLit t (Data.Vector.take 1 ys))
            App (App ListLast _) (ListLit t ys) ->
                normalize (MaybeLit t y)
              where
                y =
                    if Data.Vector.null ys
                    then Data.Vector.empty
                    else Data.Vector.singleton (Data.Vector.last ys)
            App (App ListIndexed _) (ListLit t xs) ->
                normalize (ListLit t' (fmap adapt (Data.Vector.indexed xs)))
              where
                t' = Record (Data.Map.fromList kts)
                  where
                    kts = [("_1", Natural), ("_2", t)]
                adapt (n, x) = RecordLit (Data.Map.fromList kvs)
                  where
                    kvs = [ ("_1", NaturalLit (fromIntegral n))
                          , ("_2", x)
                          ]
            App (App (App (App (App MaybeFold _) (MaybeLit _ xs)) _) just) nothing ->
                normalize (maybe nothing just' (toMaybe xs))
              where
                just' y = App just y
                toMaybe = Data.Maybe.listToMaybe . Data.Vector.toList
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
    BoolIf b true false -> case normalize b of
        BoolLit True  -> true'
        BoolLit False -> false'
        b'            -> BoolIf b' true' false'
      where
        true'  = normalize true
        false' = normalize false
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
    ListLit t es     -> ListLit (normalize t) (fmap normalize es)
    ListConcat x y   ->
        case x' of
            ListLit t xt ->
                case y' of
                    ListLit _ yt -> ListLit t (xt Data.Vector.++ yt)
                    _ -> ListConcat x' y'
            _ -> ListConcat x' y'
      where
        x' = normalize x
        y' = normalize y
    MaybeLit t es    -> MaybeLit (normalize t) (fmap normalize es)
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
