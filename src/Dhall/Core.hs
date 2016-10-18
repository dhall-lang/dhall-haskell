{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS_GHC -Wall #-}

-- | This module contains the core calculus for the Dhall language.

module Dhall.Core (
    -- * Syntax
    Const(..),
    Path(..),
    Var(..),
    Expr(..),

    -- * Normalization
    normalize,
    shift,
    subst,

    -- * Builders
    buildExpr0,
    buildExpr1,
    buildExpr2,
    buildExpr3,
    buildExpr4,
    buildExpr5,
    buildExpr6,
    buildConst,
    buildVar,
    buildElems,
    buildRecordLit,
    buildFieldValues,
    buildFieldValue,
    buildRecord,
    buildFieldTypes,
    buildFieldType,
    buildUnion,
    buildTagTypes,
    buildTagType,
    buildUnionLit,
    pretty,
    ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative (Applicative(..), (<$>))
#endif
import Data.Foldable
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text.Buildable (Buildable(..))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Traversable
import Data.Vector (Vector)
import Filesystem.Path.CurrentOS (FilePath)
import Numeric.Natural (Natural)
import Prelude hiding (FilePath)

import qualified Control.Monad
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text.Lazy                   as Text
import qualified Data.Text.Lazy.Builder           as Builder
import qualified Data.Vector
import qualified Data.Vector.Mutable
import qualified Filesystem.Path.CurrentOS        as Filesystem

{-| Constants for a pure type system

    The only axiom is:

> ⊦ Type : Kind

    ... and the valid rule pairs are:

> ⊦ Type ↝ Type : Type  -- Functions from terms to terms (ordinary functions)
> ⊦ Kind ↝ Type : Type  -- Functions from types to terms (polymorphic functions)
> ⊦ Kind ↝ Kind : Kind  -- Functions from types to types (type constructors)

    These are the same rule pairs as System Fω

    Note that Dhall does not support functions from terms to types and therefore
    Dhall is not a dependently typed language
-}
data Const = Type | Kind deriving (Show, Bounded, Enum)

instance Buildable Const where
    build = buildConst

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

{-| Label for a bound variable

    The `Text` field is the variable's name (i.e. \"@x@\").

    The `Int` field disambiguates variables with the same name if there are
    multiple bound variables of the same name in scope.  Zero refers to the
    nearest bound variable and the index increases by one for each bound
    variable of the same name going outward.  The following diagram may help:

>                           +-refers to-+
>                           |           |
>                           v           |
> \(x : *) -> \(y : *) -> \(x : *) -> x@0
>
>   +-------------refers to-------------+
>   |                                   |
>   v                                   |
> \(x : *) -> \(y : *) -> \(x : *) -> x@1

    This `Int` behaves like a De Bruijn index in the special case where all
    variables have the same name.

    You can optionally omit the index if it is @0@:

>                           +refers to+
>                           |         |
>                           v         |
> \(x : *) -> \(y : *) -> \(x : *) -> x

    Zero indices are omitted when pretty-printing `Var`s and non-zero indices
    appear as a numeric suffix.
-}
data Var = V Text !Integer
    deriving (Eq, Show)

instance IsString Var where
    fromString str = V (fromString str) 0

-- | Syntax tree for expressions
data Expr a
    -- | > Const c                                  ~  c
    = Const Const
    -- | > Var (V x 0)                              ~  x
    --   > Var (V x n)                              ~  x@n
    | Var Var             
    -- | > Lam x     A b                            ~  λ(x : A) -> b
    | Lam Text (Expr a) (Expr a)
    -- | > Pi "_" A B                               ~        A  -> B
    --   > Pi x   A B                               ~  ∀(x : A) -> B
    | Pi  Text (Expr a) (Expr a)
    -- | > App f a                                  ~  f a
    | App (Expr a) (Expr a)
    -- | > Let x Nothing  r e  ~  let x     = r in e
    --   > Let x (Just t) r e  ~  let x : t = r in e
    | Let Text (Maybe (Expr a)) (Expr a) (Expr a)
    -- | > Annot x t                                ~  x : t
    | Annot (Expr a) (Expr a)
    -- | > Bool                                     ~  Bool
    | Bool
    -- | > BoolLit b                                ~  b
    | BoolLit Bool
    -- | > BoolAnd x y                              ~  x && y
    | BoolAnd (Expr a) (Expr a)
    -- | > BoolOr  x y                              ~  x || y
    | BoolOr  (Expr a) (Expr a)
    -- | > BoolEQ  x y                              ~  x == y
    | BoolEQ  (Expr a) (Expr a)
    -- | > BoolNE  x y                              ~  x /= y
    | BoolNE  (Expr a) (Expr a)
    -- | > BoolIf x y z                             ~  if x then y else z
    | BoolIf (Expr a) (Expr a) (Expr a)
    -- | > Natural                                  ~  Natural
    | Natural
    -- | > NaturalLit n                             ~  +n
    | NaturalLit Natural
    -- | > NaturalFold                              ~  Natural/fold
    | NaturalFold
    -- | > NaturalIsZero                            ~  Natural/isZero
    | NaturalIsZero
    -- | > NaturalEven                              ~  Natural/even
    | NaturalEven
    -- | > NaturalOdd                               ~  Natural/odd
    | NaturalOdd
    -- | > NaturalPlus x y                          ~  x + y
    | NaturalPlus (Expr a) (Expr a)
    -- | > NaturalTimes x y                         ~  x * y
    | NaturalTimes (Expr a) (Expr a)
    -- | > Integer                                  ~  Integer
    | Integer
    -- | > IntegerLit n                             ~  n
    | IntegerLit Integer
    -- | > Double                                   ~  Double
    | Double
    -- | > DoubleLit n                              ~  n
    | DoubleLit Double
    -- | > Text                                     ~  Text
    | Text
    -- | > TextLit t                                ~  t
    | TextLit Builder
    -- | > TextAppend x y                           ~  x ++ y
    | TextAppend (Expr a) (Expr a)
    -- | > List                                     ~  List
    | List
    -- | > ListLit t [x, y, z]                      ~  [x, y, z] : List t
    | ListLit (Expr a) (Vector (Expr a))
    -- | > ListBuild                                ~  List/build
    | ListBuild
    -- | > ListFold                                 ~  List/fold
    | ListFold
    -- | > ListLength                               ~  List/length
    | ListLength
    -- | > ListHead                                 ~  List/head
    | ListHead 
    -- | > ListLast                                 ~  List/last
    | ListLast
    -- | > ListIndexed                              ~  List/indexed
    | ListIndexed
    -- | > ListReverse                              ~  List/reverse
    | ListReverse
    -- | > Maybe                                    ~  Maybe
    | Maybe
    -- | > MaybeLit t [e]                           ~  [e] : Maybe t
    --   > MaybeLit t []                            ~  []  : Maybe t
    | MaybeLit (Expr a) (Vector (Expr a))
    -- | > MaybeFold                                ~  Maybe/fold
    | MaybeFold
    -- | > Record            [(k1, t1), (k2, t2)]   ~  { k1 : t1, k2 : t1 }
    | Record    (Map Text (Expr a))
    -- | > RecordLit         [(k1, v1), (k2, v2)]   ~  { k1 = v1, k2 = v2 }
    | RecordLit (Map Text (Expr a))
    -- | > Union             [(k1, t1), (k2, t2)]   ~  < k1 : t1, k2 : t2 >
    | Union     (Map Text (Expr a))
    -- | > UnionLit (k1, v1) [(k2, t2), (k3, t3)]   ~  < k1 = t1, k2 : t2, k3 : t3 > 
    | UnionLit Text (Expr a) (Map Text (Expr a))
    -- | > Apply x y t                              ~ apply x y : t
    | Apply (Expr a) (Expr a) (Expr a)
    -- | > Field e x                                ~  e.x
    | Field (Expr a) Text
    -- | > Embed path                               ~  path
    | Embed a
    deriving (Functor, Foldable, Traversable, Show)

instance Applicative Expr where
    pure = Embed

    (<*>) = Control.Monad.ap

instance Monad Expr where
    return = pure

    Const c           >>= _ = Const c
    Var v             >>= _ = Var v
    Lam x _A  b       >>= k = Lam x (_A >>= k) ( b >>= k)
    Pi  x _A _B       >>= k = Pi  x (_A >>= k) (_B >>= k)
    App f a           >>= k = App (f >>= k) (a >>= k)
    Let f mt r e      >>= k = Let f (fmap (>>= k) mt) (r >>= k) (e >>= k)
    Annot x t         >>= k = Annot (x >>= k) (t >>= k)
    Bool              >>= _ = Bool
    BoolLit b         >>= _ = BoolLit b
    BoolAnd l r       >>= k = BoolAnd (l >>= k) (r >>= k)
    BoolOr  l r       >>= k = BoolOr  (l >>= k) (r >>= k)
    BoolEQ  l r       >>= k = BoolEQ  (l >>= k) (r >>= k)
    BoolNE  l r       >>= k = BoolNE  (l >>= k) (r >>= k)
    BoolIf x y z      >>= k = BoolIf (x >>= k) (y >>= k) (z >>= k)
    Natural           >>= _ = Natural
    NaturalLit n      >>= _ = NaturalLit n
    NaturalFold       >>= _ = NaturalFold
    NaturalIsZero     >>= _ = NaturalIsZero
    NaturalEven       >>= _ = NaturalEven
    NaturalOdd        >>= _ = NaturalOdd
    NaturalPlus  l r  >>= k = NaturalPlus  (l >>= k) (r >>= k)
    NaturalTimes l r  >>= k = NaturalTimes (l >>= k) (r >>= k)
    Integer           >>= _ = Integer
    IntegerLit n      >>= _ = IntegerLit n
    Double            >>= _ = Double
    DoubleLit n       >>= _ = DoubleLit n
    Text              >>= _ = Text
    TextLit t         >>= _ = TextLit t
    TextAppend l r    >>= k = TextAppend (l >>= k) (r >>= k)
    List              >>= _ = List
    ListLit t es      >>= k = ListLit (t >>= k) (fmap (>>= k) es)
    ListBuild         >>= _ = ListBuild
    ListFold          >>= _ = ListFold
    ListLength        >>= _ = ListLength
    ListHead          >>= _ = ListHead
    ListLast          >>= _ = ListLast
    ListIndexed       >>= _ = ListIndexed
    ListReverse       >>= _ = ListReverse
    Maybe             >>= _ = Maybe
    MaybeLit t es     >>= k = MaybeLit (t >>= k) (fmap (>>= k) es)
    MaybeFold         >>= _ = MaybeFold
    Record    kts     >>= k = Record    (fmap (>>= k) kts)
    RecordLit kvs     >>= k = RecordLit (fmap (>>= k) kvs)
    Union     kts     >>= k = Union     (fmap (>>= k) kts)
    UnionLit k' v kts >>= k = UnionLit k' (v >>= k) (fmap (>>= k) kts)
    Apply x y t       >>= k = Apply (x >>= k) (y >>= k) (t >>= k)
    Field r x         >>= k = Field (r >>= k) x
    Embed r           >>= k = k r

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
        "λ("
    <>  build a
    <> " : "
    <> buildExpr0 b
    <> ") → "
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
    <>  " → "
    <>  buildExpr1 c
buildExpr1 (Pi a b c) =
        "∀("
    <>  build a
    <>  " : "
    <>  buildExpr0 b
    <>  ") → "
    <>  buildExpr1 c
buildExpr1 (Let a Nothing c d) =
        "let "
    <>  build a
    <>  " = "
    <>  buildExpr0 c
    <>  " in "
    <>  buildExpr1 d
buildExpr1 (Let a (Just b) c d) =
        "let "
    <>  build a
    <>  " : "
    <>  buildExpr0 b
    <>  " = "
    <>  buildExpr0 c
    <>  " in "
    <>  buildExpr1 d
buildExpr1 (ListLit a b) =
    "[" <> buildElems (Data.Vector.toList b) <> "] : List "  <> buildExpr6 a
buildExpr1 (MaybeLit a b) =
    "[" <> buildElems (Data.Vector.toList b) <> "] : Maybe "  <> buildExpr6 a
buildExpr1 (Apply a b c) =
    "apply " <> buildExpr6 a <> " " <> buildExpr6 b <> " : " <> buildExpr5 c
buildExpr1 a =
    buildExpr2 a

buildExpr2 :: Buildable a => Expr a -> Builder
buildExpr2 (BoolEQ      a b) = buildExpr2 a <> " == " <> buildExpr2 b
buildExpr2 (BoolNE      a b) = buildExpr2 a <> " /= " <> buildExpr2 b
buildExpr2  a                = buildExpr3 a

buildExpr3 :: Buildable a => Expr a -> Builder
buildExpr3 (BoolOr      a b) = buildExpr3 a <> " || " <> buildExpr3 b
buildExpr3 (NaturalPlus a b) = buildExpr3 a <> " + "  <> buildExpr3 b
buildExpr3 (TextAppend  a b) = buildExpr3 a <> " ++ " <> buildExpr3 b
buildExpr3  a                = buildExpr4 a

buildExpr4 :: Buildable a => Expr a -> Builder
buildExpr4 (BoolAnd      a b) = buildExpr4 a <> " && " <> buildExpr4 b
buildExpr4 (NaturalTimes a b) = buildExpr4 a <> " * "  <> buildExpr4 b
buildExpr4  a                 = buildExpr5 a

buildExpr5 :: Buildable a => Expr a -> Builder
buildExpr5 (App a b) = buildExpr5 a <> " " <> buildExpr6 b
buildExpr5  a        = buildExpr6 a

buildExpr6 :: Buildable a => Expr a -> Builder
buildExpr6 (Var a) =
    buildVar a
buildExpr6 (Const k) =
    buildConst k
buildExpr6 Bool =
    "Bool"
buildExpr6 Natural =
    "Natural"
buildExpr6 NaturalFold =
    "Natural/fold"
buildExpr6 NaturalIsZero =
    "Natural/isZero"
buildExpr6 NaturalEven =
    "Natural/even"
buildExpr6 NaturalOdd =
    "Natural/odd"
buildExpr6 Integer =
    "Integer"
buildExpr6 Double =
    "Double"
buildExpr6 Text =
    "Text"
buildExpr6 ListBuild =
    "List/build"
buildExpr6 ListFold =
    "List/fold"
buildExpr6 ListLength =
    "List/length"
buildExpr6 ListHead =
    "List/head"
buildExpr6 ListLast =
    "List/last"
buildExpr6 ListIndexed =
    "List/indexed"
buildExpr6 ListReverse =
    "List/reverse"
buildExpr6 List =
    "List"
buildExpr6 Maybe =
    "Maybe"
buildExpr6 MaybeFold =
    "Maybe/fold"
buildExpr6 (BoolLit True) =
    "True"
buildExpr6 (BoolLit False) =
    "False"
buildExpr6 (IntegerLit a) =
    build (show a)
buildExpr6 (NaturalLit a) =
    "+" <> build (show a)
buildExpr6 (DoubleLit a) =
    build (show a)
buildExpr6 (TextLit a) =
    build (show a)
buildExpr6 (RecordLit a) =
    buildRecordLit a
buildExpr6 (Record a) =
    buildRecord a
buildExpr6 (Union a) =
    buildUnion a
buildExpr6 (UnionLit a b c) =
    buildUnionLit a b c
buildExpr6 (Embed a) =
    build a
buildExpr6 (Field a b) =
    buildExpr6 a <> "." <> build b
buildExpr6 a =
    "(" <> buildExpr0 a <> ")"

buildConst :: Const -> Builder
buildConst Type = "Type"
buildConst Kind = "Kind"

buildVar :: Var -> Builder
buildVar (V x 0) = build x
buildVar (V x n) = build x <> "@" <> build (show n)

buildElems :: Buildable a => [Expr a] -> Builder
buildElems   []   = ""
buildElems   [a]  = buildExpr0 a
buildElems (a:bs) = buildExpr0 a <> ", " <> buildElems bs

buildRecordLit :: Buildable a => Map Text (Expr a) -> Builder
buildRecordLit a | Data.Map.null a =
    "{=}"
buildRecordLit a =
    "{ " <> buildFieldValues (Data.Map.toList a) <> " }"

buildFieldValues :: Buildable a => [(Text, Expr a)] -> Builder
buildFieldValues    []  = ""
buildFieldValues   [a]  = buildFieldValue a
buildFieldValues (a:bs) = buildFieldValue a <> ", " <> buildFieldValues bs

buildFieldValue :: Buildable a => (Text, Expr a) -> Builder
buildFieldValue (a, b) = build a <> " = " <> buildExpr0 b

buildRecord :: Buildable a => Map Text (Expr a) -> Builder
buildRecord a | Data.Map.null a =
    "{}"
buildRecord a =
    "{ " <> buildFieldTypes (Data.Map.toList a) <> " }"

buildFieldTypes :: Buildable a => [(Text, Expr a)] -> Builder
buildFieldTypes    []  = ""
buildFieldTypes   [a]  = buildFieldType a
buildFieldTypes (a:bs) = buildFieldType a <> ", " <> buildFieldTypes bs

buildFieldType :: Buildable a => (Text, Expr a) -> Builder
buildFieldType (a, b) = build a <> " : " <> buildExpr0 b

buildUnion :: Buildable a => Map Text (Expr a) -> Builder
buildUnion a | Data.Map.null a =
    "<>"
buildUnion a =
    "< " <> buildTagTypes (Data.Map.toList a) <> " >"

buildTagTypes :: Buildable a => [(Text, Expr a)] -> Builder
buildTagTypes    []  = ""
buildTagTypes   [a]  = buildTagType a
buildTagTypes (a:bs) = buildTagType a <> " | " <> buildTagTypes bs

buildTagType :: Buildable a => (Text, Expr a) -> Builder
buildTagType (a, b) = build a <> " : " <> buildExpr0 b

buildUnionLit :: Buildable a => Text -> Expr a -> Map Text (Expr a) -> Builder
buildUnionLit a b c
    | Data.Map.null c =
            "< "
        <>  build a
        <>  " = "
        <>  buildExpr0 b
        <>  " >"
    | otherwise =
            "< "
        <>  build a
        <>  " = "
        <>  buildExpr0 b
        <>  " | "
        <>  buildTagTypes (Data.Map.toList c)
        <>  " >"

-- | Generates a syntactically valid Dhall program
instance Buildable a => Buildable (Expr a)
  where
    build = buildExpr0

shift :: Integer -> Var -> Expr a -> Expr a
shift _ _ (Const k) = Const k
shift d (V x n) (Var (V x' n')) = Var (V x' n'')
  where
    n'' = if x == x' && n <= n' then n' + d else n'
shift d (V x n) (Lam x' _A b) = Lam x' _A' b'
  where
    _A' = shift d (V x n ) _A
    b'  = shift d (V x n') b
      where
        n' = if x == x' then n + 1 else n
shift d (V x n) (Pi x' _A _B) = Pi x' _A' _B'
  where
    _A' = shift d (V x n ) _A
    _B' = shift d (V x n') _B
      where
        n' = if x == x' then n + 1 else n
shift d v (App f a) = App f' a'
  where
    f' = shift d v f
    a' = shift d v a
shift d (V x n) (Let f mt r e) = Let f mt' r' e'
  where
    e' = shift d (V x n') e
      where
        n' = if x == f then n + 1 else n

    mt' = fmap (shift d (V x n)) mt
    r'  =       shift d (V x n)  r
shift d v (Annot a b) = Annot a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (BoolAnd a b) = BoolAnd a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (BoolOr a b) = BoolOr a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (BoolEQ a b) = BoolEQ a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (BoolNE a b) = BoolNE a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (BoolIf a b c) = BoolIf a' b' c'
  where
    a' = shift d v a
    b' = shift d v b
    c' = shift d v c
shift d v (NaturalPlus a b) = NaturalPlus a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (NaturalTimes a b) = NaturalTimes a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (TextAppend a b) = TextAppend a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (ListLit a b) = ListLit a' b'
  where
    a' =       shift d v  a
    b' = fmap (shift d v) b
shift d v (MaybeLit a b) = MaybeLit a' b'
  where
    a' =       shift d v  a
    b' = fmap (shift d v) b
shift d v (Record       kts) = Record                   (fmap (shift d v) kts)
shift d v (RecordLit    kvs) = RecordLit                (fmap (shift d v) kvs)
shift d v (Union        kts) = Union                    (fmap (shift d v) kts)
shift d v (UnionLit a b kts) = UnionLit a (shift d v b) (fmap (shift d v) kts)
shift d v (Apply a b c) = Apply a' b' c'
  where
    a' = shift d v a
    b' = shift d v b
    c' = shift d v c
shift d v (Field a b) = Field a' b
  where
    a' = shift d v a
-- The Dhall compiler enforces that all embedded values are closed expressions
shift _ _ (Embed p) = Embed p
shift _ _ e = e

{-| Substitute all occurrences of a variable with an expression

> subst x C B  ~  B[x := C]
-}
subst :: Var -> Expr a -> Expr a -> Expr a
subst (V x n) e (Lam y _A b) = Lam y _A' b'
  where
    _A' = subst (V x n )                  e  _A
    b'  = subst (V x n') (shift 1 (V y 0) e)  b
    n'  = if x == y then n + 1 else n
subst (V x n) e (Pi y _A _B) = Pi y _A' _B'
  where
    _A' = subst (V x n )                  e  _A
    _B' = subst (V x n') (shift 1 (V y 0) e) _B
    n'  = if x == y then n + 1 else n
subst v e (App f a) = App f' a'
  where
    f' = subst v e f
    a' = subst v e a
subst v e (Var v') = if v == v' then e else Var v'
subst (V x n) e (Let f mt r b) = Let f mt' r' b'
  where
    b' = subst (V x n') (shift 1 (V f 0) e) b
      where
        n' = if x == f then n + 1 else n

    mt' = fmap (subst (V x n) e) mt
    r'  =       subst (V x n) e  r
subst x e (Annot y t) = Annot y' t'
  where
    y' = subst x e y
    t' = subst x e t
subst x e (BoolAnd a b) = BoolAnd a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (BoolOr a b) = BoolOr a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (BoolEQ a b) = BoolEQ a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (BoolNE a b) = BoolNE a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (BoolIf a b c) = BoolIf a' b' c'
  where
    a' = subst x e a
    b' = subst x e b
    c' = subst x e c
subst x e (NaturalPlus a b) = NaturalPlus a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (NaturalTimes a b) = NaturalTimes a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (TextAppend a b) = TextAppend a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (ListLit a b) = ListLit a' b'
  where
    a' =       subst x e  a
    b' = fmap (subst x e) b
subst x e (MaybeLit a b) = MaybeLit a' b'
  where
    a' =       subst x e  a
    b' = fmap (subst x e) b
subst x e (Record       kts) = Record                   (fmap (subst x e) kts)
subst x e (RecordLit    kvs) = RecordLit                (fmap (subst x e) kvs)
subst x e (Union        kts) = Union                    (fmap (subst x e) kts)
subst x e (UnionLit a b kts) = UnionLit a (subst x e b) (fmap (subst x e) kts)
subst x e (Apply a b c) = Apply a' b' c'
  where
    a' = subst x e a
    b' = subst x e b
    c' = subst x e c
subst x e (Field a b) = Field a' b
  where
    a' = subst x e a
-- The Dhall compiler enforces that all embedded values are closed expressions
subst _ _ (Embed p) = Embed p
subst _ _  e        = e

{-| Reduce an expression to its normal form, performing beta reduction

    `normalize` does not type-check the expression.  You may want to type-check
    expressions before normalizing them since normalization can convert an
    ill-typed expression into a well-typed expression.
-}
normalize :: Expr a -> Expr a
normalize e = case e of
    Lam x _A  b -> Lam x (normalize _A) (normalize  b)
    Pi  x _A _B -> Pi  x (normalize _A) (normalize _B)
    App f a -> case normalize f of
        Lam x _A b -> normalize b''  -- Beta reduce
          where
            a'  = shift   1  (V x 0) a
            b'  = subst (V x 0) a' b
            b'' = shift (-1) (V x 0) b'
        f' -> case App f' a' of
            App (App (App (App NaturalFold (NaturalLit n0)) _) succ') zero ->
                normalize (go n0)
              where
                go !0 = zero
                go !n = App succ' (go (n - 1))
            App NaturalIsZero (NaturalLit n) -> BoolLit (n == 0)
            App NaturalEven (NaturalLit n) -> BoolLit (even n)
            App NaturalOdd (NaturalLit n) -> BoolLit (odd n)
            App (App ListBuild _) (App (App ListFold  _) e') -> normalize e'
            App (App ListFold  _) (App (App ListBuild _) e') -> normalize e'
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
            App (App ListLength _) (ListLit _ ys) ->
                NaturalLit (fromIntegral (Data.Vector.length ys))
            App (App ListHead _) (ListLit t ys) ->
                normalize (MaybeLit t (Data.Vector.take 1 ys))
            App (App ListLast _) (ListLit t ys) ->
                normalize (MaybeLit t y)
              where
                y = if Data.Vector.null ys
                    then Data.Vector.empty
                    else Data.Vector.singleton (Data.Vector.last ys)
            App (App ListIndexed _) (ListLit t xs) ->
                normalize (ListLit t' (fmap adapt (Data.Vector.indexed xs)))
              where
                t' = Record (Data.Map.fromList kts)
                  where
                    kts = [("index", Natural), ("value", t)]
                adapt (n, x) = RecordLit (Data.Map.fromList kvs)
                  where
                    kvs = [ ("index", NaturalLit (fromIntegral n))
                          , ("value", x)
                          ]
            App (App ListReverse _) (ListLit t xs) ->
                normalize (ListLit t (Data.Vector.reverse xs))
            App (App (App (App (App MaybeFold _) (MaybeLit _ xs)) _) just) nothing ->
                normalize (maybe nothing just' (toMaybe xs))
              where
                just' y = App just y
                toMaybe = Data.Maybe.listToMaybe . Data.Vector.toList
            _ -> App f' a'
          where
            a' = normalize a
    Let f _ r b -> normalize b''
      where
        r' = shift 1 (V f 0) r
        b'  = subst (V f 0) r' b
        b'' = shift (-1) (V f 0) b'
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
    BoolEQ x y ->
        case x' of
            BoolLit xn ->
                case y' of
                    BoolLit yn -> BoolLit (xn == yn)
                    _ -> BoolEQ x' y'
            _ -> BoolEQ x' y'
      where
        x' = normalize x
        y' = normalize y
    BoolNE x y ->
        case x' of
            BoolLit xn ->
                case y' of
                    BoolLit yn -> BoolLit (xn /= yn)
                    _ -> BoolNE x' y'
            _ -> BoolNE x' y'
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
    MaybeLit t es    -> MaybeLit (normalize t) (fmap normalize es)
    Record    kts    -> Record    (fmap normalize kts)
    RecordLit kvs    -> RecordLit (fmap normalize kvs)
    Union     kts    -> Union     (fmap normalize kts)
    Apply x y t      ->
        case x of
            RecordLit kvsX ->
                case y of
                    UnionLit kY vY _ ->
                        case Data.Map.lookup kY kvsX of
                            Just vX -> normalize (App vX vY)
                            Nothing -> Apply x' y' t'
                    _ -> Apply x' y' t'
            _ -> Apply x' y' t'
      where
        x' = normalize x
        y' = normalize y
        t' = normalize t
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
