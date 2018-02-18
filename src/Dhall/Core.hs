{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wall #-}

{-| This module contains the core calculus for the Dhall language.

    Dhall is essentially a fork of the @morte@ compiler but with more built-in
    functionality, better error messages, and Haskell integration
-}

module Dhall.Core (
    -- * Syntax
      Const(..)
    , HasHome(..)
    , PathType(..)
    , PathHashed(..)
    , PathMode(..)
    , Path(..)
    , Var(..)
    , Chunks(..)
    , Expr(..)

    -- * Normalization
    , normalize
    , normalizeWith
    , Normalizer
    , subst
    , shift
    , isNormalized
    , isNormalizedWith
    , denote

    -- * Pretty-printing
    , pretty

    -- * Miscellaneous
    , internalError
    , reservedIdentifiers
    , escapeText
    ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative (Applicative(..), (<$>))
#endif
import Control.Applicative (empty)
import Data.Bifunctor (Bifunctor(..))
import Data.Foldable
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashSet (HashSet)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Scientific (Scientific)
import Data.Text.Buildable (Buildable(..))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Prettyprint.Doc (Pretty)
import Data.Traversable
import Data.Vector (Vector)
import {-# SOURCE #-} Dhall.Pretty.Internal
import Numeric.Natural (Natural)
import Prelude hiding (succ)

import qualified Control.Monad
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Base16
import qualified Data.HashMap.Strict.InsOrd
import qualified Data.HashSet
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Lazy                        as Text
import qualified Data.Text.Lazy.Builder                as Builder
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Vector
import qualified Data.Vector.Mutable

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
data Const = Type | Kind deriving (Show, Eq, Bounded, Enum)

instance Buildable Const where
    build = buildConst

-- | Whether or not a path is relative to the user's home directory
data HasHome = Home | Homeless deriving (Eq, Ord, Show)

-- | The type of path to import (i.e. local vs. remote vs. environment)
data PathType
    = File HasHome FilePath
    -- ^ Local path
    | URL  Text (Maybe PathHashed)
    -- ^ URL of emote resource and optional headers stored in a path
    | Env  Text
    -- ^ Environment variable
    deriving (Eq, Ord, Show)

instance Buildable PathType where
    build (File Home     file)
        = "~/" <> build (Text.pack file)
    build (File Homeless file)
        |  Text.isPrefixOf  "./" txt
        || Text.isPrefixOf   "/" txt
        || Text.isPrefixOf "../" txt
        = build txt <> " "
        | otherwise
        = "./" <> build txt <> " "
      where
        txt = Text.pack file
    build (URL str  Nothing      ) = build str <> " "
    build (URL str (Just headers)) = build str <> " using " <> build headers <> " "
    build (Env env) = "env:" <> build env

-- | How to interpret the path's contents (i.e. as Dhall code or raw text)
data PathMode = Code | RawText deriving (Eq, Ord, Show)

-- | A `PathType` extended with an optional hash for semantic integrity checks
data PathHashed = PathHashed
    { hash     :: Maybe Data.ByteString.ByteString
    , pathType :: PathType
    } deriving (Eq, Ord, Show)

instance Buildable PathHashed where
    build (PathHashed  Nothing p) = build p
    build (PathHashed (Just h) p) = build p <> "sha256:" <> build string <> " "
      where
        bytes = Data.ByteString.Base16.encode h

        string = Data.ByteString.Char8.unpack bytes

-- | Path to an external resource
data Path = Path
    { pathHashed :: PathHashed
    , pathMode   :: PathMode
    } deriving (Eq, Ord, Show)

instance Buildable Path where
    build (Path {..}) = build pathHashed <> suffix
      where
        suffix = case pathMode of
            RawText -> "as Text"
            Code    -> ""

instance Pretty Path where
    pretty path = Pretty.pretty (Builder.toLazyText (build path))

{-| Label for a bound variable

    The `Text` field is the variable's name (i.e. \"@x@\").

    The `Int` field disambiguates variables with the same name if there are
    multiple bound variables of the same name in scope.  Zero refers to the
    nearest bound variable and the index increases by one for each bound
    variable of the same name going outward.  The following diagram may help:

>                               ┌──refers to──┐
>                               │             │
>                               v             │
> λ(x : Type) → λ(y : Type) → λ(x : Type) → x@0
>
> ┌─────────────────refers to─────────────────┐
> │                                           │
> v                                           │
> λ(x : Type) → λ(y : Type) → λ(x : Type) → x@1

    This `Int` behaves like a De Bruijn index in the special case where all
    variables have the same name.

    You can optionally omit the index if it is @0@:

>                               ┌─refers to─┐
>                               │           │
>                               v           │
> λ(x : Type) → λ(y : Type) → λ(x : Type) → x

    Zero indices are omitted when pretty-printing `Var`s and non-zero indices
    appear as a numeric suffix.
-}
data Var = V Text !Integer
    deriving (Eq, Show)

instance IsString Var where
    fromString str = V (fromString str) 0

instance Buildable Var where
    build = buildVar

-- | Syntax tree for expressions
data Expr s a
    -- | > Const c                                  ~  c
    = Const Const
    -- | > Var (V x 0)                              ~  x
    --   > Var (V x n)                              ~  x@n
    | Var Var
    -- | > Lam x     A b                            ~  λ(x : A) -> b
    | Lam Text (Expr s a) (Expr s a)
    -- | > Pi "_" A B                               ~        A  -> B
    --   > Pi x   A B                               ~  ∀(x : A) -> B
    | Pi  Text (Expr s a) (Expr s a)
    -- | > App f a                                  ~  f a
    | App (Expr s a) (Expr s a)
    -- | > Let x Nothing  r e                       ~  let x     = r in e
    --   > Let x (Just t) r e                       ~  let x : t = r in e
    | Let Text (Maybe (Expr s a)) (Expr s a) (Expr s a)
    -- | > Annot x t                                ~  x : t
    | Annot (Expr s a) (Expr s a)
    -- | > Bool                                     ~  Bool
    | Bool
    -- | > BoolLit b                                ~  b
    | BoolLit Bool
    -- | > BoolAnd x y                              ~  x && y
    | BoolAnd (Expr s a) (Expr s a)
    -- | > BoolOr  x y                              ~  x || y
    | BoolOr  (Expr s a) (Expr s a)
    -- | > BoolEQ  x y                              ~  x == y
    | BoolEQ  (Expr s a) (Expr s a)
    -- | > BoolNE  x y                              ~  x != y
    | BoolNE  (Expr s a) (Expr s a)
    -- | > BoolIf x y z                             ~  if x then y else z
    | BoolIf (Expr s a) (Expr s a) (Expr s a)
    -- | > Natural                                  ~  Natural
    | Natural
    -- | > NaturalLit n                             ~  +n
    | NaturalLit Natural
    -- | > NaturalFold                              ~  Natural/fold
    | NaturalFold
    -- | > NaturalBuild                             ~  Natural/build
    | NaturalBuild
    -- | > NaturalIsZero                            ~  Natural/isZero
    | NaturalIsZero
    -- | > NaturalEven                              ~  Natural/even
    | NaturalEven
    -- | > NaturalOdd                               ~  Natural/odd
    | NaturalOdd
    -- | > NaturalToInteger                         ~  Natural/toInteger
    | NaturalToInteger
    -- | > NaturalShow                              ~  Natural/show
    | NaturalShow
    -- | > NaturalPlus x y                          ~  x + y
    | NaturalPlus (Expr s a) (Expr s a)
    -- | > NaturalTimes x y                         ~  x * y
    | NaturalTimes (Expr s a) (Expr s a)
    -- | > Integer                                  ~  Integer
    | Integer
    -- | > IntegerLit n                             ~  n
    | IntegerLit Integer
    -- | > IntegerShow                              ~  Integer/show
    | IntegerShow
    -- | > Double                                   ~  Double
    | Double
    -- | > DoubleLit n                              ~  n
    | DoubleLit Scientific
    -- | > DoubleShow                               ~  Double/show
    | DoubleShow
    -- | > Text                                     ~  Text
    | Text
    -- | > TextLit (Chunks [(t1, e1), (t2, e2)] t3) ~  "t1${e1}t2${e2}t3"
    | TextLit (Chunks s a)
    -- | > TextAppend x y                           ~  x ++ y
    | TextAppend (Expr s a) (Expr s a)
    -- | > List                                     ~  List
    | List
    -- | > ListLit (Just t ) [x, y, z]              ~  [x, y, z] : List t
    --   > ListLit  Nothing  [x, y, z]              ~  [x, y, z]
    | ListLit (Maybe (Expr s a)) (Vector (Expr s a))
    -- | > ListAppend x y                           ~  x # y
    | ListAppend (Expr s a) (Expr s a)
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
    -- | > Optional                                 ~  Optional
    | Optional
    -- | > OptionalLit t [e]                        ~  [e] : Optional t
    --   > OptionalLit t []                         ~  []  : Optional t
    | OptionalLit (Expr s a) (Vector (Expr s a))
    -- | > OptionalFold                             ~  Optional/fold
    | OptionalFold
    -- | > OptionalBuild                            ~  Optional/build
    | OptionalBuild
    -- | > Record            [(k1, t1), (k2, t2)]   ~  { k1 : t1, k2 : t1 }
    | Record    (InsOrdHashMap Text (Expr s a))
    -- | > RecordLit         [(k1, v1), (k2, v2)]   ~  { k1 = v1, k2 = v2 }
    | RecordLit (InsOrdHashMap Text (Expr s a))
    -- | > Union             [(k1, t1), (k2, t2)]   ~  < k1 : t1 | k2 : t2 >
    | Union     (InsOrdHashMap Text (Expr s a))
    -- | > UnionLit (k1, v1) [(k2, t2), (k3, t3)]   ~  < k1 = t1 | k2 : t2 | k3 : t3 >
    | UnionLit Text (Expr s a) (InsOrdHashMap Text (Expr s a))
    -- | > Combine x y                              ~  x ∧ y
    | Combine (Expr s a) (Expr s a)
    -- | > CombineRight x y                         ~  x ⫽ y
    | Prefer (Expr s a) (Expr s a)
    -- | > Merge x y (Just t )                      ~  merge x y : t
    --   > Merge x y  Nothing                       ~  merge x y
    | Merge (Expr s a) (Expr s a) (Maybe (Expr s a))
    -- | > Constructors e                           ~  constructors e
    | Constructors (Expr s a)
    -- | > Field e x                                ~  e.x
    | Field (Expr s a) Text
    -- | > Note s x                                 ~  e
    | Note s (Expr s a)
    -- | > Embed path                               ~  path
    | Embed a
    deriving (Functor, Foldable, Traversable, Show, Eq)

instance Applicative (Expr s) where
    pure = Embed

    (<*>) = Control.Monad.ap

instance Monad (Expr s) where
    return = pure

    Const a              >>= _ = Const a
    Var a                >>= _ = Var a
    Lam a b c            >>= k = Lam a (b >>= k) (c >>= k)
    Pi  a b c            >>= k = Pi a (b >>= k) (c >>= k)
    App a b              >>= k = App (a >>= k) (b >>= k)
    Let a b c d          >>= k = Let a (fmap (>>= k) b) (c >>= k) (d >>= k)
    Annot a b            >>= k = Annot (a >>= k) (b >>= k)
    Bool                 >>= _ = Bool
    BoolLit a            >>= _ = BoolLit a
    BoolAnd a b          >>= k = BoolAnd (a >>= k) (b >>= k)
    BoolOr  a b          >>= k = BoolOr  (a >>= k) (b >>= k)
    BoolEQ  a b          >>= k = BoolEQ  (a >>= k) (b >>= k)
    BoolNE  a b          >>= k = BoolNE  (a >>= k) (b >>= k)
    BoolIf a b c         >>= k = BoolIf (a >>= k) (b >>= k) (c >>= k)
    Natural              >>= _ = Natural
    NaturalLit a         >>= _ = NaturalLit a
    NaturalFold          >>= _ = NaturalFold
    NaturalBuild         >>= _ = NaturalBuild
    NaturalIsZero        >>= _ = NaturalIsZero
    NaturalEven          >>= _ = NaturalEven
    NaturalOdd           >>= _ = NaturalOdd
    NaturalToInteger     >>= _ = NaturalToInteger
    NaturalShow          >>= _ = NaturalShow
    NaturalPlus  a b     >>= k = NaturalPlus  (a >>= k) (b >>= k)
    NaturalTimes a b     >>= k = NaturalTimes (a >>= k) (b >>= k)
    Integer              >>= _ = Integer
    IntegerLit a         >>= _ = IntegerLit a
    IntegerShow          >>= _ = IntegerShow
    Double               >>= _ = Double
    DoubleLit a          >>= _ = DoubleLit a
    DoubleShow           >>= _ = DoubleShow
    Text                 >>= _ = Text
    TextLit (Chunks a b) >>= k = TextLit (Chunks (fmap (fmap (>>= k)) a) b)
    TextAppend a b       >>= k = TextAppend (a >>= k) (b >>= k)
    List                 >>= _ = List
    ListLit a b          >>= k = ListLit (fmap (>>= k) a) (fmap (>>= k) b)
    ListAppend a b       >>= k = ListAppend (a >>= k) (b >>= k)
    ListBuild            >>= _ = ListBuild
    ListFold             >>= _ = ListFold
    ListLength           >>= _ = ListLength
    ListHead             >>= _ = ListHead
    ListLast             >>= _ = ListLast
    ListIndexed          >>= _ = ListIndexed
    ListReverse          >>= _ = ListReverse
    Optional             >>= _ = Optional
    OptionalLit a b      >>= k = OptionalLit (a >>= k) (fmap (>>= k) b)
    OptionalFold         >>= _ = OptionalFold
    OptionalBuild        >>= _ = OptionalBuild
    Record    a          >>= k = Record (fmap (>>= k) a)
    RecordLit a          >>= k = RecordLit (fmap (>>= k) a)
    Union     a          >>= k = Union (fmap (>>= k) a)
    UnionLit a b c       >>= k = UnionLit a (b >>= k) (fmap (>>= k) c)
    Combine a b          >>= k = Combine (a >>= k) (b >>= k)
    Prefer a b           >>= k = Prefer (a >>= k) (b >>= k)
    Merge a b c          >>= k = Merge (a >>= k) (b >>= k) (fmap (>>= k) c)
    Constructors a       >>= k = Constructors (a >>= k)
    Field a b            >>= k = Field (a >>= k) b
    Note a b             >>= k = Note a (b >>= k)
    Embed a              >>= k = k a

instance Bifunctor Expr where
    first _ (Const a             ) = Const a
    first _ (Var a               ) = Var a
    first k (Lam a b c           ) = Lam a (first k b) (first k c)
    first k (Pi a b c            ) = Pi a (first k b) (first k c)
    first k (App a b             ) = App (first k a) (first k b)
    first k (Let a b c d         ) = Let a (fmap (first k) b) (first k c) (first k d)
    first k (Annot a b           ) = Annot (first k a) (first k b)
    first _  Bool                  = Bool
    first _ (BoolLit a           ) = BoolLit a
    first k (BoolAnd a b         ) = BoolAnd (first k a) (first k b)
    first k (BoolOr a b          ) = BoolOr (first k a) (first k b)
    first k (BoolEQ a b          ) = BoolEQ (first k a) (first k b)
    first k (BoolNE a b          ) = BoolNE (first k a) (first k b)
    first k (BoolIf a b c        ) = BoolIf (first k a) (first k b) (first k c)
    first _  Natural               = Natural
    first _ (NaturalLit a        ) = NaturalLit a
    first _  NaturalFold           = NaturalFold
    first _  NaturalBuild          = NaturalBuild
    first _  NaturalIsZero         = NaturalIsZero
    first _  NaturalEven           = NaturalEven
    first _  NaturalOdd            = NaturalOdd
    first _  NaturalToInteger      = NaturalToInteger
    first _  NaturalShow           = NaturalShow
    first k (NaturalPlus a b     ) = NaturalPlus (first k a) (first k b)
    first k (NaturalTimes a b    ) = NaturalTimes (first k a) (first k b)
    first _  Integer               = Integer
    first _ (IntegerLit a        ) = IntegerLit a
    first _  IntegerShow           = IntegerShow
    first _  Double                = Double
    first _ (DoubleLit a         ) = DoubleLit a
    first _  DoubleShow            = DoubleShow
    first _  Text                  = Text
    first k (TextLit (Chunks a b)) = TextLit (Chunks (fmap (fmap (first k)) a) b)
    first k (TextAppend a b      ) = TextAppend (first k a) (first k b)
    first _  List                  = List
    first k (ListLit a b         ) = ListLit (fmap (first k) a) (fmap (first k) b)
    first k (ListAppend a b      ) = ListAppend (first k a) (first k b)
    first _  ListBuild             = ListBuild
    first _  ListFold              = ListFold
    first _  ListLength            = ListLength
    first _  ListHead              = ListHead
    first _  ListLast              = ListLast
    first _  ListIndexed           = ListIndexed
    first _  ListReverse           = ListReverse
    first _  Optional              = Optional
    first k (OptionalLit a b     ) = OptionalLit (first k a) (fmap (first k) b)
    first _  OptionalFold          = OptionalFold
    first _  OptionalBuild         = OptionalBuild
    first k (Record a            ) = Record (fmap (first k) a)
    first k (RecordLit a         ) = RecordLit (fmap (first k) a)
    first k (Union a             ) = Union (fmap (first k) a)
    first k (UnionLit a b c      ) = UnionLit a (first k b) (fmap (first k) c)
    first k (Combine a b         ) = Combine (first k a) (first k b)
    first k (Prefer a b          ) = Prefer (first k a) (first k b)
    first k (Merge a b c         ) = Merge (first k a) (first k b) (fmap (first k) c)
    first k (Constructors a      ) = Constructors (first k a)
    first k (Field a b           ) = Field (first k a) b
    first k (Note a b            ) = Note (k a) (first k b)
    first _ (Embed a             ) = Embed a

    second = fmap

instance IsString (Expr s a) where
    fromString str = Var (fromString str)

-- | The body of an interpolated @Text@ literal
data Chunks s a = Chunks [(Builder, Expr s a)] Builder
    deriving (Functor, Foldable, Traversable, Show, Eq)

instance Monoid (Chunks s a) where
    mempty = Chunks [] mempty

    mappend (Chunks xysL zL) (Chunks         []    zR) =
        Chunks xysL (zL <> zR)
    mappend (Chunks xysL zL) (Chunks ((x, y):xysR) zR) =
        Chunks (xysL ++ (zL <> x, y):xysR) zR

instance IsString (Chunks s a) where
    fromString str = Chunks [] (fromString str)

{-  There is a one-to-one correspondence between the builders in this section
    and the sub-parsers in "Dhall.Parser".  Each builder is named after the
    corresponding parser and the relationship between builders exactly matches
    the relationship between parsers.  This leads to the nice emergent property
    of automatically getting all the parentheses and precedences right.

    This approach has one major disadvantage: you can get an infinite loop if
    you add a new constructor to the syntax tree without adding a matching
    case the corresponding builder.
-}

-- | Generates a syntactically valid Dhall program
instance Buildable a => Buildable (Expr s a) where
    build = buildExpr

instance Pretty a => Pretty (Expr s a) where
    pretty = Pretty.unAnnotate . prettyExpr

{-| `shift` is used by both normalization and type-checking to avoid variable
    capture by shifting variable indices

    For example, suppose that you were to normalize the following expression:

> λ(a : Type) → λ(x : a) → (λ(y : a) → λ(x : a) → y) x

    If you were to substitute @y@ with @x@ without shifting any variable
    indices, then you would get the following incorrect result:

> λ(a : Type) → λ(x : a) → λ(x : a) → x  -- Incorrect normalized form

    In order to substitute @x@ in place of @y@ we need to `shift` @x@ by @1@ in
    order to avoid being misinterpreted as the @x@ bound by the innermost
    lambda.  If we perform that `shift` then we get the correct result:

> λ(a : Type) → λ(x : a) → λ(x : a) → x@1

    As a more worked example, suppose that you were to normalize the following
    expression:

>     λ(a : Type)
> →   λ(f : a → a → a)
> →   λ(x : a)
> →   λ(x : a)
> →   (λ(x : a) → f x x@1) x@1

    The correct normalized result would be:

>     λ(a : Type)
> →   λ(f : a → a → a)
> →   λ(x : a)
> →   λ(x : a)
> →   f x@1 x

    The above example illustrates how we need to both increase and decrease
    variable indices as part of substitution:

    * We need to increase the index of the outer @x\@1@ to @x\@2@ before we
      substitute it into the body of the innermost lambda expression in order
      to avoid variable capture.  This substitution changes the body of the
      lambda expression to @(f x\@2 x\@1)@

    * We then remove the innermost lambda and therefore decrease the indices of
      both @x@s in @(f x\@2 x\@1)@ to @(f x\@1 x)@ in order to reflect that one
      less @x@ variable is now bound within that scope

    Formally, @(shift d (V x n) e)@ modifies the expression @e@ by adding @d@ to
    the indices of all variables named @x@ whose indices are greater than
    @(n + m)@, where @m@ is the number of bound variables of the same name
    within that scope

    In practice, @d@ is always @1@ or @-1@ because we either:

    * increment variables by @1@ to avoid variable capture during substitution
    * decrement variables by @1@ when deleting lambdas after substitution

    @n@ starts off at @0@ when substitution begins and increments every time we
    descend into a lambda or let expression that binds a variable of the same
    name in order to avoid shifting the bound variables by mistake.
-}
shift :: Integer -> Var -> Expr s a -> Expr s a
shift _ _ (Const a) = Const a
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
shift _ _ Bool = Bool
shift _ _ (BoolLit a) = BoolLit a
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
shift _ _ Natural = Natural
shift _ _ (NaturalLit a) = NaturalLit a
shift _ _ NaturalFold = NaturalFold
shift _ _ NaturalBuild = NaturalBuild
shift _ _ NaturalIsZero = NaturalIsZero
shift _ _ NaturalEven = NaturalEven
shift _ _ NaturalOdd = NaturalOdd
shift _ _ NaturalToInteger = NaturalToInteger
shift _ _ NaturalShow = NaturalShow
shift d v (NaturalPlus a b) = NaturalPlus a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (NaturalTimes a b) = NaturalTimes a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift _ _ Integer = Integer
shift _ _ (IntegerLit a) = IntegerLit a
shift _ _ IntegerShow = IntegerShow
shift _ _ Double = Double
shift _ _ (DoubleLit a) = DoubleLit a
shift _ _ DoubleShow = DoubleShow
shift _ _ Text = Text
shift d v (TextLit (Chunks a b)) = TextLit (Chunks a' b)
  where
    a' = fmap (fmap (shift d v)) a
shift d v (TextAppend a b) = TextAppend a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift _ _ List = List
shift d v (ListLit a b) = ListLit a' b'
  where
    a' = fmap (shift d v) a
    b' = fmap (shift d v) b
shift _ _ ListBuild = ListBuild
shift d v (ListAppend a b) = ListAppend a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift _ _ ListFold = ListFold
shift _ _ ListLength = ListLength
shift _ _ ListHead = ListHead
shift _ _ ListLast = ListLast
shift _ _ ListIndexed = ListIndexed
shift _ _ ListReverse = ListReverse
shift _ _ Optional = Optional
shift d v (OptionalLit a b) = OptionalLit a' b'
  where
    a' =       shift d v  a
    b' = fmap (shift d v) b
shift _ _ OptionalFold = OptionalFold
shift _ _ OptionalBuild = OptionalBuild
shift d v (Record a) = Record a'
  where
    a' = fmap (shift d v) a
shift d v (RecordLit a) = RecordLit a'
  where
    a' = fmap (shift d v) a
shift d v (Union a) = Union a'
  where
    a' = fmap (shift d v) a
shift d v (UnionLit a b c) = UnionLit a b' c'
  where
    b' =       shift d v  b
    c' = fmap (shift d v) c
shift d v (Combine a b) = Combine a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (Prefer a b) = Prefer a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (Merge a b c) = Merge a' b' c'
  where
    a' =       shift d v  a
    b' =       shift d v  b
    c' = fmap (shift d v) c
shift d v (Constructors a) = Constructors a'
  where
    a' = shift d v  a
shift d v (Field a b) = Field a' b
  where
    a' = shift d v a
shift d v (Note a b) = Note a b'
  where
    b' = shift d v b
-- The Dhall compiler enforces that all embedded values are closed expressions
-- and `shift` does nothing to a closed expression
shift _ _ (Embed p) = Embed p

{-| Substitute all occurrences of a variable with an expression

> subst x C B  ~  B[x := C]
-}
subst :: Var -> Expr s a -> Expr s a -> Expr s a
subst _ _ (Const a) = Const a
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
subst x e (Annot a b) = Annot a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst _ _ Bool = Bool
subst _ _ (BoolLit a) = BoolLit a
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
subst _ _ Natural = Natural
subst _ _ (NaturalLit a) = NaturalLit a
subst _ _ NaturalFold = NaturalFold
subst _ _ NaturalBuild = NaturalBuild
subst _ _ NaturalIsZero = NaturalIsZero
subst _ _ NaturalEven = NaturalEven
subst _ _ NaturalOdd = NaturalOdd
subst _ _ NaturalToInteger = NaturalToInteger
subst _ _ NaturalShow = NaturalShow
subst x e (NaturalPlus a b) = NaturalPlus a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (NaturalTimes a b) = NaturalTimes a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst _ _ Integer = Integer
subst _ _ (IntegerLit a) = IntegerLit a
subst _ _ IntegerShow = IntegerShow
subst _ _ Double = Double
subst _ _ (DoubleLit a) = DoubleLit a
subst _ _ DoubleShow = DoubleShow
subst _ _ Text = Text
subst x e (TextLit (Chunks a b)) = TextLit (Chunks a' b)
  where
    a' = fmap (fmap (subst x e)) a
subst x e (TextAppend a b) = TextAppend a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst _ _ List = List
subst x e (ListLit a b) = ListLit a' b'
  where
    a' = fmap (subst x e) a
    b' = fmap (subst x e) b
subst x e (ListAppend a b) = ListAppend a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst _ _ ListBuild = ListBuild
subst _ _ ListFold = ListFold
subst _ _ ListLength = ListLength
subst _ _ ListHead = ListHead
subst _ _ ListLast = ListLast
subst _ _ ListIndexed = ListIndexed
subst _ _ ListReverse = ListReverse
subst _ _ Optional = Optional
subst x e (OptionalLit a b) = OptionalLit a' b'
  where
    a' =       subst x e  a
    b' = fmap (subst x e) b
subst _ _ OptionalFold = OptionalFold
subst _ _ OptionalBuild = OptionalBuild
subst x e (Record       kts) = Record                   (fmap (subst x e) kts)
subst x e (RecordLit    kvs) = RecordLit                (fmap (subst x e) kvs)
subst x e (Union        kts) = Union                    (fmap (subst x e) kts)
subst x e (UnionLit a b kts) = UnionLit a (subst x e b) (fmap (subst x e) kts)
subst x e (Combine a b) = Combine a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (Prefer a b) = Prefer a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (Merge a b c) = Merge a' b' c'
  where
    a' =       subst x e  a
    b' =       subst x e  b
    c' = fmap (subst x e) c
subst x e (Constructors a) = Constructors a'
  where
    a' = subst x e  a
subst x e (Field a b) = Field a' b
  where
    a' = subst x e a
subst x e (Note a b) = Note a b'
  where
    b' = subst x e b
-- The Dhall compiler enforces that all embedded values are closed expressions
-- and `subst` does nothing to a closed expression
subst _ _ (Embed p) = Embed p

{-| Reduce an expression to its normal form, performing beta reduction

    `normalize` does not type-check the expression.  You may want to type-check
    expressions before normalizing them since normalization can convert an
    ill-typed expression into a well-typed expression.

    However, `normalize` will not fail if the expression is ill-typed and will
    leave ill-typed sub-expressions unevaluated.
-}
normalize ::  Expr s a -> Expr t a
normalize = normalizeWith (const Nothing)

{-| This function is used to determine whether folds like @Natural/fold@ or
    @List/fold@ should be lazy or strict in their accumulator based on the type
    of the accumulator

    If this function returns `True`, then they will be strict in their
    accumulator since we can guarantee an upper bound on the amount of work to
    normalize the accumulator on each step of the loop.  If this function
    returns `False` then they will be lazy in their accumulator and only
    normalize the final result at the end of the fold
-}
boundedType :: Expr s a -> Bool
boundedType Bool             = True
boundedType Natural          = True
boundedType Integer          = True
boundedType Double           = True
boundedType Text             = True
boundedType (App List _)     = False
boundedType (App Optional t) = boundedType t
boundedType (Record kvs)     = all boundedType kvs
boundedType (Union kvs)      = all boundedType kvs
boundedType _                = False

-- | Remove all `Note` constructors from an `Expr` (i.e. de-`Note`)
denote :: Expr s a -> Expr t a
denote (Note _ b            ) = denote b
denote (Const a             ) = Const a
denote (Var a               ) = Var a
denote (Lam a b c           ) = Lam a (denote b) (denote c)
denote (Pi a b c            ) = Pi a (denote b) (denote c)
denote (App a b             ) = App (denote a) (denote b)
denote (Let a b c d         ) = Let a (fmap denote b) (denote c) (denote d)
denote (Annot a b           ) = Annot (denote a) (denote b)
denote  Bool                  = Bool
denote (BoolLit a           ) = BoolLit a
denote (BoolAnd a b         ) = BoolAnd (denote a) (denote b)
denote (BoolOr a b          ) = BoolOr (denote a) (denote b)
denote (BoolEQ a b          ) = BoolEQ (denote a) (denote b)
denote (BoolNE a b          ) = BoolNE (denote a) (denote b)
denote (BoolIf a b c        ) = BoolIf (denote a) (denote b) (denote c)
denote  Natural               = Natural
denote (NaturalLit a        ) = NaturalLit a
denote  NaturalFold           = NaturalFold
denote  NaturalBuild          = NaturalBuild
denote  NaturalIsZero         = NaturalIsZero
denote  NaturalEven           = NaturalEven
denote  NaturalOdd            = NaturalOdd
denote  NaturalToInteger      = NaturalToInteger
denote  NaturalShow           = NaturalShow
denote (NaturalPlus a b     ) = NaturalPlus (denote a) (denote b)
denote (NaturalTimes a b    ) = NaturalTimes (denote a) (denote b)
denote  Integer               = Integer
denote (IntegerLit a        ) = IntegerLit a
denote  IntegerShow           = IntegerShow
denote  Double                = Double
denote (DoubleLit a         ) = DoubleLit a
denote  DoubleShow            = DoubleShow
denote  Text                  = Text
denote (TextLit (Chunks a b)) = TextLit (Chunks (fmap (fmap denote) a) b)
denote (TextAppend a b      ) = TextAppend (denote a) (denote b)
denote  List                  = List
denote (ListLit a b         ) = ListLit (fmap denote a) (fmap denote b)
denote (ListAppend a b      ) = ListAppend (denote a) (denote b)
denote  ListBuild             = ListBuild
denote  ListFold              = ListFold
denote  ListLength            = ListLength
denote  ListHead              = ListHead
denote  ListLast              = ListLast
denote  ListIndexed           = ListIndexed
denote  ListReverse           = ListReverse
denote  Optional              = Optional
denote (OptionalLit a b     ) = OptionalLit (denote a) (fmap denote b)
denote  OptionalFold          = OptionalFold
denote  OptionalBuild         = OptionalBuild
denote (Record a            ) = Record (fmap denote a)
denote (RecordLit a         ) = RecordLit (fmap denote a)
denote (Union a             ) = Union (fmap denote a)
denote (UnionLit a b c      ) = UnionLit a (denote b) (fmap denote c)
denote (Combine a b         ) = Combine (denote a) (denote b)
denote (Prefer a b          ) = Prefer (denote a) (denote b)
denote (Merge a b c         ) = Merge (denote a) (denote b) (fmap denote c)
denote (Constructors a      ) = Constructors (denote a)
denote (Field a b           ) = Field (denote a) b
denote (Embed a             ) = Embed a

{-| Reduce an expression to its normal form, performing beta reduction and applying
    any custom definitions.
   
    `normalizeWith` is designed to be used with function `typeWith`. The `typeWith`
    function allows typing of Dhall functions in a custom typing context whereas 
    `normalizeWith` allows evaluating Dhall expressions in a custom context. 

    To be more precise `normalizeWith` applies the given normalizer when it finds an
    application term that it cannot reduce by other means.

    Note that the context used in normalization will determine the properties of normalization.
    That is, if the functions in custom context are not total then the Dhall language, evaluated
    with those functions is not total either.  
   
-}
normalizeWith :: Normalizer a -> Expr s a -> Expr t a
normalizeWith ctx e0 = loop (denote e0)
 where
    -- This is to avoid a `Show` constraint on the @a@ and @s@ in the type of
    -- `loop`.  In theory, this might change a failing repro case into
    -- a successful one, but the risk of that is low enough to not warrant
    -- the `Show` constraint.  I care more about proving at the type level
    -- that the @a@ and @s@ type parameters are never used
 e'' = bimap (\_ -> ()) (\_ -> ()) e0

 text = "NormalizeWith.loop (" <> Data.Text.pack (show e'') <> ")"
 loop e =  case e of
    Const k -> Const k
    Var v -> Var v
    Lam x _A b -> Lam x _A' b'
      where
        _A' = loop _A
        b'  = loop b
    Pi  x _A _B -> Pi  x _A' _B'
      where
        _A' = loop _A
        _B' = loop _B
    App f a -> case loop f of
        Lam x _A b -> loop b''  -- Beta reduce
          where
            a'  = shift   1  (V x 0) a
            b'  = subst (V x 0) a' b
            b'' = shift (-1) (V x 0) b'
        f' -> case App f' a' of
            -- fold/build fusion for `List`
            App (App ListBuild _) (App (App ListFold _) e') -> loop e'
            App (App ListFold _) (App (App ListBuild _) e') -> loop e'
            -- fold/build fusion for `Natural`
            App NaturalBuild (App NaturalFold e') -> loop e'
            App NaturalFold (App NaturalBuild e') -> loop e'

            -- fold/build fusion for `Optional`
            App (App OptionalBuild _) (App (App OptionalFold _) e') -> loop e'
            App (App OptionalFold _) (App (App OptionalBuild _) e') -> loop e'

            App (App (App (App NaturalFold (NaturalLit n0)) t) succ') zero ->
                if boundedType (loop t) then strict else lazy
              where
                strict =       strictLoop n0
                lazy   = loop (  lazyLoop n0)

                strictLoop !0 = loop zero
                strictLoop !n = loop (App succ' (strictLoop (n - 1)))

                lazyLoop !0 = zero
                lazyLoop !n = App succ' (lazyLoop (n - 1))
            App NaturalBuild k
                | check     -> NaturalLit n
                | otherwise -> App f' a'
              where
                labeled =
                    loop (App (App (App k Natural) "Succ") "Zero")

                n = go 0 labeled
                  where
                    go !m (App (Var "Succ") e') = go (m + 1) e'
                    go !m (Var "Zero")          = m
                    go !_  _                    = internalError text
                check = go labeled
                  where
                    go (App (Var "Succ") e') = go e'
                    go (Var "Zero")          = True
                    go  _                    = False
            App NaturalIsZero (NaturalLit n) -> BoolLit (n == 0)
            App NaturalEven (NaturalLit n) -> BoolLit (even n)
            App NaturalOdd (NaturalLit n) -> BoolLit (odd n)
            App NaturalToInteger (NaturalLit n) -> IntegerLit (toInteger n)
            App NaturalShow (NaturalLit n) ->
                TextLit (Chunks [] ("+" <> buildNatural n))
            App IntegerShow (IntegerLit n) ->
                TextLit (Chunks [] (buildNumber n))
            App DoubleShow (DoubleLit n) ->
                TextLit (Chunks [] (buildScientific n))
            App (App OptionalBuild t) k
                | check     -> OptionalLit t k'
                | otherwise -> App f' a'
              where
                labeled = loop (App (App (App k (App Optional t)) "Just") "Nothing")

                k' = go labeled
                  where
                    go (App (Var "Just") e') = pure e'
                    go (Var "Nothing")       = empty
                    go  _                    = internalError text
                check = go labeled
                  where
                    go (App (Var "Just") _) = True
                    go (Var "Nothing")      = True
                    go  _                   = False
            App (App ListBuild t) k
                | check     -> ListLit (Just t) (buildVector k')
                | otherwise -> App f' a'
              where
                labeled =
                    loop (App (App (App k (App List t)) "Cons") "Nil")

                k' cons nil = go labeled
                  where
                    go (App (App (Var "Cons") x) e') = cons x (go e')
                    go (Var "Nil")                   = nil
                    go  _                            = internalError text
                check = go labeled
                  where
                    go (App (App (Var "Cons") _) e') = go e'
                    go (Var "Nil")                   = True
                    go  _                            = False
            App (App (App (App (App ListFold _) (ListLit _ xs)) t) cons) nil ->
                if boundedType (loop t) then strict else lazy
              where
                strict =       Data.Vector.foldr strictCons strictNil xs
                lazy   = loop (Data.Vector.foldr   lazyCons   lazyNil xs)

                strictNil = loop nil
                lazyNil   =      nil

                strictCons y ys = loop (App (App cons y) ys)
                lazyCons   y ys =       App (App cons y) ys
            App (App ListLength _) (ListLit _ ys) ->
                NaturalLit (fromIntegral (Data.Vector.length ys))
            App (App ListHead t) (ListLit _ ys) ->
                loop (OptionalLit t (Data.Vector.take 1 ys))
            App (App ListLast t) (ListLit _ ys) ->
                loop (OptionalLit t y)
              where
                y = if Data.Vector.null ys
                    then Data.Vector.empty
                    else Data.Vector.singleton (Data.Vector.last ys)
            App (App ListIndexed t) (ListLit _ xs) ->
                loop (ListLit (Just t') (fmap adapt (Data.Vector.indexed xs)))
              where
                t' = Record (Data.HashMap.Strict.InsOrd.fromList kts)
                  where
                    kts = [ ("index", Natural)
                          , ("value", t)
                          ]
                adapt (n, x) = RecordLit (Data.HashMap.Strict.InsOrd.fromList kvs)
                  where
                    kvs = [ ("index", NaturalLit (fromIntegral n))
                          , ("value", x)
                          ]
            App (App ListReverse t) (ListLit _ xs) ->
                loop (ListLit (Just t) (Data.Vector.reverse xs))
            App (App (App (App (App OptionalFold _) (OptionalLit _ xs)) _) just) nothing ->
                loop (maybe nothing just' (toMaybe xs))
              where
                just' y = App just y
                toMaybe = Data.Maybe.listToMaybe . Data.Vector.toList
            _ ->  case ctx (App f' a') of
                    Nothing -> App f' a'
                    Just app' -> loop app'
          where
            a' = loop a
    Let f _ r b -> loop b''
      where
        r'  = shift   1  (V f 0) r
        b'  = subst (V f 0) r' b
        b'' = shift (-1) (V f 0) b'
    Annot x _ -> loop x
    Bool -> Bool
    BoolLit b -> BoolLit b
    BoolAnd x y ->
        case x' of
            BoolLit xn ->
                case y' of
                    BoolLit yn -> BoolLit (xn && yn)
                    _ -> BoolAnd x' y'
            _ -> BoolAnd x' y'
      where
        x' = loop x
        y' = loop y
    BoolOr x y ->
        case x' of
            BoolLit xn ->
                case y' of
                    BoolLit yn -> BoolLit (xn || yn)
                    _ -> BoolOr x' y'
            _ -> BoolOr x' y'
      where
        x' = loop x
        y' = loop y
    BoolEQ x y ->
        case x' of
            BoolLit xn ->
                case y' of
                    BoolLit yn -> BoolLit (xn == yn)
                    _ -> BoolEQ x' y'
            _ -> BoolEQ x' y'
      where
        x' = loop x
        y' = loop y
    BoolNE x y ->
        case x' of
            BoolLit xn ->
                case y' of
                    BoolLit yn -> BoolLit (xn /= yn)
                    _ -> BoolNE x' y'
            _ -> BoolNE x' y'
      where
        x' = loop x
        y' = loop y
    BoolIf b true false -> case loop b of
        BoolLit True  -> true'
        BoolLit False -> false'
        b'            -> BoolIf b' true' false'
      where
        true'  = loop true
        false' = loop false
    Natural -> Natural
    NaturalLit n -> NaturalLit n
    NaturalFold -> NaturalFold
    NaturalBuild -> NaturalBuild
    NaturalIsZero -> NaturalIsZero
    NaturalEven -> NaturalEven
    NaturalOdd -> NaturalOdd
    NaturalToInteger -> NaturalToInteger
    NaturalShow -> NaturalShow
    NaturalPlus  x y ->
        case x' of
            NaturalLit xn ->
                case y' of
                    NaturalLit yn -> NaturalLit (xn + yn)
                    _ -> NaturalPlus x' y'
            _ -> NaturalPlus x' y'
      where
        x' = loop x
        y' = loop y
    NaturalTimes x y ->
        case x' of
            NaturalLit xn ->
                case y' of
                    NaturalLit yn -> NaturalLit (xn * yn)
                    _ -> NaturalTimes x' y'
            _ -> NaturalTimes x' y'
      where
        x' = loop x
        y' = loop y
    Integer -> Integer
    IntegerLit n -> IntegerLit n
    IntegerShow -> IntegerShow
    Double -> Double
    DoubleLit n -> DoubleLit n
    DoubleShow -> DoubleShow
    Text -> Text
    TextLit (Chunks xys z) ->
        case mconcat chunks of
            Chunks [("", x)] "" -> x
            c                   -> TextLit c
      where
        chunks = concatMap process xys ++ [Chunks [] z]

        process (x, y) = case loop y of
            TextLit c -> [Chunks [] x, c]
            y'        -> [Chunks [(x, y')] mempty]
    TextAppend x y   ->
        case x' of
            TextLit xt ->
                case y' of
                    TextLit yt -> TextLit (xt <> yt)
                    _ -> TextAppend x' y'
            _ -> TextAppend x' y'
      where
        x' = loop x
        y' = loop y
    List -> List
    ListLit t es -> ListLit t' es'
      where
        t'  = fmap loop t
        es' = fmap loop es
    ListAppend x y ->
        case x' of
            ListLit t xs ->
                case y' of
                    ListLit _ ys -> ListLit t (xs <> ys)
                    _ -> ListAppend x' y'
            _ -> ListAppend x' y'
      where
        x' = loop x
        y' = loop y
    ListBuild -> ListBuild
    ListFold -> ListFold
    ListLength -> ListLength
    ListHead -> ListHead
    ListLast -> ListLast
    ListIndexed -> ListIndexed
    ListReverse -> ListReverse
    Optional -> Optional
    OptionalLit t es -> OptionalLit t' es'
      where
        t'  =      loop t
        es' = fmap loop es
    OptionalFold -> OptionalFold
    OptionalBuild -> OptionalBuild
    Record kts -> Record kts'
      where
        kts' = fmap loop kts
    RecordLit kvs -> RecordLit kvs'
      where
        kvs' = fmap loop kvs
    Union kts -> Union kts'
      where
        kts' = fmap loop kts
    UnionLit k v kvs -> UnionLit k v' kvs'
      where
        v'   =      loop v
        kvs' = fmap loop kvs
    Combine x0 y0 ->
        let combine x y = case x of
                RecordLit kvsX -> case y of
                    RecordLit kvsY ->
                        let kvs = Data.HashMap.Strict.InsOrd.unionWith combine kvsX kvsY
                        in  RecordLit (fmap loop kvs)
                    _ -> Combine x y
                _ -> Combine x y
        in  combine (loop x0) (loop y0)
    Prefer x y ->
        case x' of
            RecordLit kvsX ->
                case y' of
                    RecordLit kvsY ->
                        RecordLit (fmap loop (Data.HashMap.Strict.InsOrd.union kvsY kvsX))
                    _ -> Prefer x' y'
            _ -> Prefer x' y'
      where
        x' = loop x
        y' = loop y
    Merge x y t      ->
        case x' of
            RecordLit kvsX ->
                case y' of
                    UnionLit kY vY _ ->
                        case Data.HashMap.Strict.InsOrd.lookup kY kvsX of
                            Just vX -> loop (App vX vY)
                            Nothing -> Merge x' y' t'
                    _ -> Merge x' y' t'
            _ -> Merge x' y' t'
      where
        x' =      loop x
        y' =      loop y
        t' = fmap loop t
    Constructors t   ->
        case t' of
            Union kts -> RecordLit kvs
              where
                kvs = Data.HashMap.Strict.InsOrd.mapWithKey adapt kts

                adapt k t_ = Lam k t_ (UnionLit k (Var (V k 0)) rest)
                  where
                    rest = Data.HashMap.Strict.InsOrd.delete k kts
            _ -> Constructors t'
      where
        t' = loop t
    Field r x        ->
        case loop r of
            RecordLit kvs ->
                case Data.HashMap.Strict.InsOrd.lookup x kvs of
                    Just v  -> loop v
                    Nothing -> Field (RecordLit (fmap loop kvs)) x
            r' -> Field r' x
    Note _ e' -> loop e'
    Embed a -> Embed a

-- | Use this to wrap you embedded functions (see `normalizeWith`) to make them
--   polymorphic enough to be used.
type Normalizer a = forall s. Expr s a -> Maybe (Expr s a)

-- | Check if an expression is in a normal form given a context of evaluation.
--   Unlike `isNormalized`, this will fully normalize and traverse through the expression. 
--   
--   It is much more efficient to use `isNormalized`.
isNormalizedWith :: (Eq s, Eq a) => Normalizer a -> Expr s a -> Bool
isNormalizedWith ctx e = e == (normalizeWith ctx e)


-- | Quickly check if an expression is in normal form
isNormalized :: Expr s a -> Bool
isNormalized e = case denote e of
    Const _ -> True
    Var _ -> True
    Lam _ a b -> isNormalized a && isNormalized b
    Pi _ a b -> isNormalized a && isNormalized b
    App f a -> isNormalized f && isNormalized a && case App f a of
        App (Lam _ _ _) _ -> False

        -- fold/build fusion for `List`
        App (App ListBuild _) (App (App ListFold _) _) -> False
        App (App ListFold _) (App (App ListBuild _) _) -> False

        -- fold/build fusion for `Natural`
        App NaturalBuild (App NaturalFold _) -> False
        App NaturalFold (App NaturalBuild _) -> False

        -- fold/build fusion for `Optional`
        App (App OptionalBuild _) (App (App OptionalFold _) _) -> False
        App (App OptionalFold _) (App (App OptionalBuild _) _) -> False

        App (App (App (App NaturalFold (NaturalLit _)) _) _) _ -> False
        App NaturalBuild k0 -> isNormalized k0 && not (check0 k0)
          where
            check0 (Lam _ _ (Lam succ _ (Lam zero _ k))) = check1 succ zero k
            check0 _ = False

            check1 succ zero (App (Var (V succ' n)) k) =
                succ == succ' && n == (if succ == zero then 1 else 0) && check1 succ zero k
            check1 _ zero (Var (V zero' 0)) = zero == zero'
            check1 _ _ _ = False
        App NaturalIsZero (NaturalLit _) -> False
        App NaturalEven (NaturalLit _) -> False
        App NaturalOdd (NaturalLit _) -> False
        App NaturalShow (NaturalLit _) -> False
        App NaturalToInteger (NaturalLit _) -> False
        App IntegerShow (IntegerLit _) -> False
        App DoubleShow (DoubleLit _) -> False
        App (App OptionalBuild t) k0 -> isNormalized t && isNormalized k0 && not (check0 k0)
          where
            check0 (Lam _ _ (Lam just _ (Lam nothing _ k))) = check1 just nothing k
            check0 _ = False

            check1 just nothing (App (Var (V just' n)) _) =
                just == just' && n == (if just == nothing then 1 else 0)
            check1 _ nothing (Var (V nothing' 0)) = nothing == nothing'
            check1 _ _ _ = False
        App (App ListBuild t) k0 -> isNormalized t && isNormalized k0 && not (check0 k0)
          where
            check0 (Lam _ _ (Lam cons _ (Lam nil _ k))) = check1 cons nil k
            check0 _ = False

            check1 cons nil (App (Var (V cons' n)) k) =
                cons == cons' && n == (if cons == nil then 1 else 0) && check1 cons nil k
            check1 _ nil (Var (V nil' 0)) = nil == nil'
            check1 _ _ _ = False
        App (App (App (App (App ListFold _) (ListLit _ _)) _) _) _ ->
            False
        App (App ListLength _) (ListLit _ _) -> False
        App (App ListHead _) (ListLit _ _) -> False
        App (App ListLast _) (ListLit _ _) -> False
        App (App ListIndexed _) (ListLit _ _) -> False
        App (App ListReverse _) (ListLit _ _) -> False
        App (App (App (App (App OptionalFold _) (OptionalLit _ _)) _) _) _ ->
            False
        _ -> True
    Let _ _ _ _ -> False
    Annot _ _ -> False
    Bool -> True
    BoolLit _ -> True
    BoolAnd x y -> isNormalized x && isNormalized y &&
        case x of
            BoolLit _ ->
                case y of
                    BoolLit _ -> False
                    _ -> True
            _ -> True
    BoolOr x y -> isNormalized x && isNormalized y &&
        case x of
            BoolLit _ ->
                case y of
                    BoolLit _ -> False
                    _ -> True
            _ -> True
    BoolEQ x y -> isNormalized x && isNormalized y &&
        case x of
            BoolLit _ ->
                case y of
                    BoolLit _ -> False
                    _ -> True
            _ -> True
    BoolNE x y -> isNormalized x && isNormalized y &&
        case x of
            BoolLit _ ->
                case y of
                    BoolLit _ -> False
                    _ -> True
            _ -> True
    BoolIf b true false -> isNormalized b && case b of
        BoolLit _ -> False
        _         -> isNormalized true && isNormalized false
    Natural -> True
    NaturalLit _ -> True
    NaturalFold -> True
    NaturalBuild -> True
    NaturalIsZero -> True
    NaturalEven -> True
    NaturalOdd -> True
    NaturalShow -> True
    NaturalToInteger -> True
    NaturalPlus x y -> isNormalized x && isNormalized y &&
        case x of
            NaturalLit _ ->
                case y of
                    NaturalLit _ -> False
                    _ -> True
            _ -> True
    NaturalTimes x y -> isNormalized x && isNormalized y &&
        case x of
            NaturalLit _ ->
                case y of
                    NaturalLit _ -> False
                    _ -> True
            _ -> True
    Integer -> True
    IntegerLit _ -> True
    IntegerShow -> True
    Double -> True
    DoubleLit _ -> True
    DoubleShow -> True
    Text -> True
    TextLit (Chunks xys _) -> all (all isNormalized) xys
    TextAppend x y -> isNormalized x && isNormalized y &&
        case x of
            TextLit _ ->
                case y of
                    TextLit _ -> False
                    _ -> True
            _ -> True
    List -> True
    ListLit t es -> all isNormalized t && all isNormalized es
    ListAppend x y -> isNormalized x && isNormalized y &&
        case x of
            ListLit _ _ ->
                case y of
                    ListLit _ _ -> False
                    _ -> True
            _ -> True
    ListBuild -> True
    ListFold -> True
    ListLength -> True
    ListHead -> True
    ListLast -> True
    ListIndexed -> True
    ListReverse -> True
    Optional -> True
    OptionalLit t es -> isNormalized t && all isNormalized es
    OptionalFold -> True
    OptionalBuild -> True
    Record kts -> all isNormalized kts
    RecordLit kvs -> all isNormalized kvs
    Union kts -> all isNormalized kts
    UnionLit _ v kvs -> isNormalized v && all isNormalized kvs
    Combine x y -> isNormalized x && isNormalized y && combine
      where
        combine = case x of
            RecordLit _ -> case y of
                RecordLit _ -> False
                _ -> True
            _ -> True
    Prefer x y -> isNormalized x && isNormalized y && combine
      where
        combine = case x of
            RecordLit _ -> case y of
                RecordLit _ -> False
                _ -> True
            _ -> True
    Merge x y t -> isNormalized x && isNormalized y && any isNormalized t &&
        case x of
            RecordLit kvsX ->
                case y of
                    UnionLit kY _  _ ->
                        case Data.HashMap.Strict.InsOrd.lookup kY kvsX of
                            Just _  -> False
                            Nothing -> True
                    _ -> True
            _ -> True
    Constructors t -> isNormalized t &&
        case t of
            Union _ -> False
            _       -> True
    Field r x -> isNormalized r &&
        case r of
            RecordLit kvs ->
                case Data.HashMap.Strict.InsOrd.lookup x kvs of
                    Just _  -> False
                    Nothing -> True
            _ -> True
    Note _ e' -> isNormalized e'
    Embed _ -> True

_ERROR :: String
_ERROR = "\ESC[1;31mError\ESC[0m"

{-| Utility function used to throw internal errors that should never happen
    (in theory) but that are not enforced by the type system
-}
internalError :: Data.Text.Text -> forall b . b
internalError text = error (unlines
    [ _ERROR <> ": Compiler bug                                                        "
    , "                                                                                "
    , "Explanation: This error message means that there is a bug in the Dhall compiler."
    , "You didn't do anything wrong, but if you would like to see this problem fixed   "
    , "then you should report the bug at:                                              "
    , "                                                                                "
    , "https://github.com/dhall-lang/dhall-haskell/issues                              "
    , "                                                                                "
    , "Please include the following text in your bug report:                           "
    , "                                                                                "
    , "```                                                                             "
    , Data.Text.unpack text <> "                                                       "
    , "```                                                                             "
    ] )

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

-- | The set of reserved identifiers for the Dhall language
reservedIdentifiers :: HashSet Text
reservedIdentifiers =
    Data.HashSet.fromList
        [ "let"
        , "in"
        , "Type"
        , "Kind"
        , "forall"
        , "Bool"
        , "True"
        , "False"
        , "merge"
        , "if"
        , "then"
        , "else"
        , "as"
        , "using"
        , "constructors"
        , "Natural"
        , "Natural/fold"
        , "Natural/build"
        , "Natural/isZero"
        , "Natural/even"
        , "Natural/odd"
        , "Natural/toInteger"
        , "Natural/show"
        , "Integer"
        , "Integer/show"
        , "Double"
        , "Double/show"
        , "Text"
        , "List"
        , "List/build"
        , "List/fold"
        , "List/length"
        , "List/head"
        , "List/last"
        , "List/indexed"
        , "List/reverse"
        , "Optional"
        , "Optional/build"
        , "Optional/fold"
        ]
