{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS_GHC -Wall #-}

-- | This module contains the core calculus for the Dhall language.

module Dhall.Core (
    -- * Syntax
      Const(..)
    , Path(..)
    , Var(..)
    , Expr(..)

    -- * Normalization
    , normalize
    , subst
    , shift

    -- * Builders
    -- $builders
    , pretty
    , buildExpr0
    , buildExpr1
    , buildExpr2
    , buildExpr3
    , buildExpr4
    , buildExpr5
    , buildExpr6
    , buildConst
    , buildVar
    , buildElems
    , buildRecordLit
    , buildFieldValues
    , buildFieldValue
    , buildRecord
    , buildFieldTypes
    , buildFieldType
    , buildUnion
    , buildAlternativeTypes
    , buildAlternativeType
    , buildUnionLit

    -- * Miscellaneous
    , internalError
    ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative (Applicative(..), (<$>))
#endif
import Data.Bifunctor (Bifunctor(..))
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
import qualified Data.Text
import qualified Data.Text.Lazy            as Text
import qualified Data.Text.Lazy.Builder    as Builder
import qualified Data.Vector
import qualified Data.Vector.Mutable
import qualified Filesystem.Path.CurrentOS as Filesystem
import qualified NeatInterpolation

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
    -- | > Let x Nothing  r e  ~  let x     = r in e
    --   > Let x (Just t) r e  ~  let x : t = r in e
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
    -- | > NaturalPlus x y                          ~  x + y
    | NaturalPlus (Expr s a) (Expr s a)
    -- | > NaturalTimes x y                         ~  x * y
    | NaturalTimes (Expr s a) (Expr s a)
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
    | TextAppend (Expr s a) (Expr s a)
    -- | > List                                     ~  List
    | List
    -- | > ListLit t [x, y, z]                      ~  [x, y, z] : List t
    | ListLit (Expr s a) (Vector (Expr s a))
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
    -- | > Record            [(k1, t1), (k2, t2)]   ~  { k1 : t1, k2 : t1 }
    | Record    (Map Text (Expr s a))
    -- | > RecordLit         [(k1, v1), (k2, v2)]   ~  { k1 = v1, k2 = v2 }
    | RecordLit (Map Text (Expr s a))
    -- | > Union             [(k1, t1), (k2, t2)]   ~  < k1 : t1, k2 : t2 >
    | Union     (Map Text (Expr s a))
    -- | > UnionLit (k1, v1) [(k2, t2), (k3, t3)]   ~  < k1 = t1, k2 : t2, k3 : t3 > 
    | UnionLit Text (Expr s a) (Map Text (Expr s a))
    -- | > Combine x y                              ~  x ∧ y
    | Combine (Expr s a) (Expr s a)
    -- | > Merge x y t                              ~  merge x y : t
    | Merge (Expr s a) (Expr s a) (Expr s a)
    -- | > Field e x                                ~  e.x
    | Field (Expr s a) Text
    -- | > Note s x                                 ~  e
    | Note s (Expr s a)
    -- | > Embed path                               ~  path
    | Embed a
    deriving (Functor, Foldable, Traversable, Show)

instance Applicative (Expr s) where
    pure = Embed

    (<*>) = Control.Monad.ap

instance Monad (Expr s) where
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
    NaturalBuild      >>= _ = NaturalBuild
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
    Optional          >>= _ = Optional
    OptionalLit t es  >>= k = OptionalLit (t >>= k) (fmap (>>= k) es)
    OptionalFold      >>= _ = OptionalFold
    Record    kts     >>= k = Record    (fmap (>>= k) kts)
    RecordLit kvs     >>= k = RecordLit (fmap (>>= k) kvs)
    Union     kts     >>= k = Union     (fmap (>>= k) kts)
    UnionLit k' v kts >>= k = UnionLit k' (v >>= k) (fmap (>>= k) kts)
    Combine x y       >>= k = Combine (x >>= k) (y >>= k)
    Merge x y t       >>= k = Merge (x >>= k) (y >>= k) (t >>= k)
    Field r x         >>= k = Field (r >>= k) x
    Note a b          >>= k = Note a (b >>= k)
    Embed r           >>= k = k r

instance Bifunctor Expr where
    first _ (Const a         ) = Const a
    first _ (Var a           ) = Var a
    first k (Lam a b c       ) = Lam a (first k b) (first k c)
    first k (Pi a b c        ) = Pi a (first k b) (first k c)
    first k (App a b         ) = App (first k a) (first k b)
    first k (Let a b c d     ) = Let a (fmap (first k) b) (first k c) (first k d)
    first k (Annot a b       ) = Annot (first k a) (first k b)
    first _  Bool              = Bool
    first _ (BoolLit a       ) = BoolLit a
    first k (BoolAnd a b     ) = BoolAnd (first k a) (first k b)
    first k (BoolOr a b      ) = BoolOr (first k a) (first k b)
    first k (BoolEQ a b      ) = BoolEQ (first k a) (first k b)
    first k (BoolNE a b      ) = BoolNE (first k a) (first k b)
    first k (BoolIf a b c    ) = BoolIf (first k a) (first k b) (first k c)
    first _  Natural           = Natural
    first _ (NaturalLit a    ) = NaturalLit a
    first _  NaturalFold       = NaturalFold
    first _  NaturalBuild      = NaturalBuild
    first _  NaturalIsZero     = NaturalIsZero
    first _  NaturalEven       = NaturalEven
    first _  NaturalOdd        = NaturalOdd
    first k (NaturalPlus a b ) = NaturalPlus (first k a) (first k b)
    first k (NaturalTimes a b) = NaturalTimes (first k a) (first k b)
    first _  Integer           = Integer
    first _ (IntegerLit a    ) = IntegerLit a
    first _  Double            = Double
    first _ (DoubleLit a     ) = DoubleLit a
    first _  Text              = Text
    first _ (TextLit a       ) = TextLit a
    first k (TextAppend a b  ) = TextAppend (first k a) (first k b)
    first _  List              = List
    first k (ListLit a b     ) = ListLit (first k a) (fmap (first k) b)
    first _  ListBuild         = ListBuild
    first _  ListFold          = ListFold
    first _  ListLength        = ListLength
    first _  ListHead          = ListHead
    first _  ListLast          = ListLast
    first _  ListIndexed       = ListIndexed
    first _  ListReverse       = ListReverse
    first _  Optional          = Optional
    first k (OptionalLit a b ) = OptionalLit (first k a) (fmap (first k) b)
    first _  OptionalFold      = OptionalFold
    first k (Record a        ) = Record (fmap (first k) a)
    first k (RecordLit a     ) = RecordLit (fmap (first k) a)
    first k (Union a         ) = Union (fmap (first k) a)
    first k (UnionLit a b c  ) = UnionLit a (first k b) (fmap (first k) c)
    first k (Combine a b     ) = Combine (first k a) (first k b)
    first k (Merge a b c     ) = Merge (first k a) (first k b) (first k c)
    first k (Field a b       ) = Field (first k a) b
    first k (Note a b        ) = Note (k a) (first k b)
    first _ (Embed a         ) = Embed a

    second = fmap

instance IsString (Expr s a)
  where
    fromString str = Var (fromString str)

{- $builders
    There is a one-to-one correspondence between the builders in this section
    and the sub-parsers in "Dhall.Parser".  Each builder is named after the
    corresponding parser and the relationship between builders exactly matches
    the relationship between parsers.  This leads to the nice emergent property
    of automatically getting all the parentheses and precedences right.

    This approach has one major disadvantage: you can get an infinite loop if
    you add a new constructor to the syntax tree without adding a matching
    case the corresponding builder.
-}

-- | Pretty-print a value
pretty :: Buildable a => a -> Text
pretty = Builder.toLazyText . build

-- | Builder corresponding to the @label@ token in "Dhall.Parser"
buildLabel :: Text -> Builder
buildLabel = build

-- | Builder corresponding to the @number@ token in "Dhall.Parser"
buildNumber :: Integer -> Builder
buildNumber a = build (show a)

-- | Builder corresponding to the @natural@ token in "Dhall.Parser"
buildNatural :: Natural -> Builder
buildNatural a = build (show a)

-- | Builder corresponding to the @double@ token in "Dhall.Parser"
buildDouble :: Double -> Builder
buildDouble a = build (show a)

-- | Builder corresponding to the @text@ token in "Dhall.Parser"
buildText :: Builder -> Builder
buildText a = build (show a)

-- | Builder corresponding to the @Expr0@ parser in "Dhall.Parser"
buildExpr0 :: Buildable a => Expr s a -> Builder
buildExpr0 (Annot a b) = buildExpr1 a <> " : " <> buildExpr0 b
buildExpr0 (Note  _ b) = buildExpr0 b
buildExpr0 a           = buildExpr1 a

-- | Builder corresponding to the @Expr1@ parser in "Dhall.Parser"
buildExpr1 :: Buildable a => Expr s a -> Builder
buildExpr1 (Lam a b c) =
        "λ("
    <>  buildLabel a
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
    <>  buildLabel a
    <>  " : "
    <>  buildExpr0 b
    <>  ") → "
    <>  buildExpr1 c
buildExpr1 (Let a Nothing c d) =
        "let "
    <>  buildLabel a
    <>  " = "
    <>  buildExpr0 c
    <>  " in "
    <>  buildExpr1 d
buildExpr1 (Let a (Just b) c d) =
        "let "
    <>  buildLabel a
    <>  " : "
    <>  buildExpr0 b
    <>  " = "
    <>  buildExpr0 c
    <>  " in "
    <>  buildExpr1 d
buildExpr1 (ListLit a b) =
    "[" <> buildElems (Data.Vector.toList b) <> "] : List "  <> buildExpr6 a
buildExpr1 (OptionalLit a b) =
    "[" <> buildElems (Data.Vector.toList b) <> "] : Optional "  <> buildExpr6 a
buildExpr1 (Merge a b c) =
    "merge " <> buildExpr6 a <> " " <> buildExpr6 b <> " : " <> buildExpr5 c
buildExpr1 (Note _ b) =
    buildExpr1 b
buildExpr1 a =
    buildExpr2 a

-- | Builder corresponding to the @Expr2@ parser in "Dhall.Parser"
buildExpr2 :: Buildable a => Expr s a -> Builder
buildExpr2 (BoolEQ a b) = buildExpr2 a <> " == " <> buildExpr2 b
buildExpr2 (BoolNE a b) = buildExpr2 a <> " != " <> buildExpr2 b
buildExpr2 (Note   _ b) = buildExpr2 b
buildExpr2  a           = buildExpr3 a

-- | Builder corresponding to the @Expr3@ parser in "Dhall.Parser"
buildExpr3 :: Buildable a => Expr s a -> Builder
buildExpr3 (BoolOr      a b) = buildExpr3 a <> " || " <> buildExpr3 b
buildExpr3 (NaturalPlus a b) = buildExpr3 a <> " + "  <> buildExpr3 b
buildExpr3 (TextAppend  a b) = buildExpr3 a <> " ++ " <> buildExpr3 b
buildExpr3 (Note        _ b) = buildExpr3 b
buildExpr3  a                = buildExpr4 a

-- | Builder corresponding to the @Expr4@ parser in "Dhall.Parser"
buildExpr4 :: Buildable a => Expr s a -> Builder
buildExpr4 (BoolAnd      a b) = buildExpr4 a <> " && " <> buildExpr4 b
buildExpr4 (NaturalTimes a b) = buildExpr4 a <> " * "  <> buildExpr4 b
buildExpr4 (Combine      a b) = buildExpr4 a <> " ∧ "  <> buildExpr4 b
buildExpr4 (Note         _ b) = buildExpr4 b
buildExpr4  a                 = buildExpr5 a

-- | Builder corresponding to the @Expr5@ parser in "Dhall.Parser"
buildExpr5 :: Buildable a => Expr s a -> Builder
buildExpr5 (App  a b) = buildExpr5 a <> " " <> buildExpr6 b
buildExpr5 (Note _ b) = buildExpr5 b
buildExpr5  a         = buildExpr6 a

-- | Builder corresponding to the @Expr6@ parser in "Dhall.Parser"
buildExpr6 :: Buildable a => Expr s a -> Builder
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
buildExpr6 NaturalBuild =
    "Natural/build"
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
buildExpr6 List =
    "List"
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
buildExpr6 Optional =
    "Optional"
buildExpr6 OptionalFold =
    "Optional/fold"
buildExpr6 (BoolLit True) =
    "True"
buildExpr6 (BoolLit False) =
    "False"
buildExpr6 (IntegerLit a) =
    buildNumber a
buildExpr6 (NaturalLit a) =
    "+" <> buildNatural a
buildExpr6 (DoubleLit a) =
    buildDouble a
buildExpr6 (TextLit a) =
    buildText a
buildExpr6 (Record a) =
    buildRecord a
buildExpr6 (RecordLit a) =
    buildRecordLit a
buildExpr6 (Union a) =
    buildUnion a
buildExpr6 (UnionLit a b c) =
    buildUnionLit a b c
buildExpr6 (Embed a) =
    build a
buildExpr6 (Field a b) =
    buildExpr6 a <> "." <> buildLabel b
buildExpr6 (Note _ b) =
    buildExpr6 b
buildExpr6 a =
    "(" <> buildExpr0 a <> ")"

-- | Builder corresponding to the @Const@ parser in "Dhall.Parser"
buildConst :: Const -> Builder
buildConst Type = "Type"
buildConst Kind = "Kind"

-- | Builder corresponding to the @Var@ parser in "Dhall.Parser"
buildVar :: Var -> Builder
buildVar (V x 0) = buildLabel x
buildVar (V x n) = buildLabel x <> "@" <> buildNumber n

-- | Builder corresponding to the @Elems@ parser in "Dhall.Parser"
buildElems :: Buildable a => [Expr s a] -> Builder
buildElems   []   = ""
buildElems   [a]  = buildExpr0 a
buildElems (a:bs) = buildExpr0 a <> ", " <> buildElems bs

-- | Builder corresponding to the @RecordLit@ parser in "Dhall.Parser"
buildRecordLit :: Buildable a => Map Text (Expr s a) -> Builder
buildRecordLit a | Data.Map.null a =
    "{=}"
buildRecordLit a =
    "{ " <> buildFieldValues (Data.Map.toList a) <> " }"

-- | Builder corresponding to the @FieldValues@ parser in "Dhall.Parser"
buildFieldValues :: Buildable a => [(Text, Expr s a)] -> Builder
buildFieldValues    []  = ""
buildFieldValues   [a]  = buildFieldValue a
buildFieldValues (a:bs) = buildFieldValue a <> ", " <> buildFieldValues bs

-- | Builder corresponding to the @FieldValue@ parser in "Dhall.Parser"
buildFieldValue :: Buildable a => (Text, Expr s a) -> Builder
buildFieldValue (a, b) = buildLabel a <> " = " <> buildExpr0 b

-- | Builder corresponding to the @Record@ parser in "Dhall.Parser"
buildRecord :: Buildable a => Map Text (Expr s a) -> Builder
buildRecord a | Data.Map.null a =
    "{}"
buildRecord a =
    "{ " <> buildFieldTypes (Data.Map.toList a) <> " }"

-- | Builder corresponding to the @FieldTypes@ parser in "Dhall.Parser"
buildFieldTypes :: Buildable a => [(Text, Expr s a)] -> Builder
buildFieldTypes    []  = ""
buildFieldTypes   [a]  = buildFieldType a
buildFieldTypes (a:bs) = buildFieldType a <> ", " <> buildFieldTypes bs

-- | Builder corresponding to the @FieldType@ parser in "Dhall.Parser"
buildFieldType :: Buildable a => (Text, Expr s a) -> Builder
buildFieldType (a, b) = buildLabel a <> " : " <> buildExpr0 b

-- | Builder corresponding to the @Union@ parser in "Dhall.Parser"
buildUnion :: Buildable a => Map Text (Expr s a) -> Builder
buildUnion a | Data.Map.null a =
    "<>"
buildUnion a =
    "< " <> buildAlternativeTypes (Data.Map.toList a) <> " >"

-- | Builder corresponding to the @AlternativeTypes@ parser in "Dhall.Parser"
buildAlternativeTypes :: Buildable a => [(Text, Expr s a)] -> Builder
buildAlternativeTypes [] =
    ""
buildAlternativeTypes [a] =
    buildAlternativeType a
buildAlternativeTypes (a:bs) =
    buildAlternativeType a <> " | " <> buildAlternativeTypes bs

-- | Builder corresponding to the @AlternativeType@ parser in "Dhall.Parser"
buildAlternativeType :: Buildable a => (Text, Expr s a) -> Builder
buildAlternativeType (a, b) = buildLabel a <> " : " <> buildExpr0 b

-- | Builder corresponding to the @UnionLit@ parser in "Dhall.Parser"
buildUnionLit :: Buildable a => Text -> Expr s a -> Map Text (Expr s a) -> Builder
buildUnionLit a b c
    | Data.Map.null c =
            "< "
        <>  buildLabel a
        <>  " = "
        <>  buildExpr0 b
        <>  " >"
    | otherwise =
            "< "
        <>  buildLabel a
        <>  " = "
        <>  buildExpr0 b
        <>  " | "
        <>  buildAlternativeTypes (Data.Map.toList c)
        <>  " >"

-- | Generates a syntactically valid Dhall program
instance Buildable a => Buildable (Expr s a)
  where
    build = buildExpr0

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
shift :: Integer -> Var -> Expr s a -> Expr t a
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
shift _ _ Double = Double
shift _ _ (DoubleLit a) = DoubleLit a
shift _ _ Text = Text
shift _ _ (TextLit a) = TextLit a
shift d v (TextAppend a b) = TextAppend a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift _ _ List = List
shift d v (ListLit a b) = ListLit a' b'
  where
    a' =       shift d v  a
    b' = fmap (shift d v) b
shift _ _ ListBuild = ListBuild
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
shift d v (Merge a b c) = Merge a' b' c'
  where
    a' = shift d v a
    b' = shift d v b
    c' = shift d v c
shift d v (Field a b) = Field a' b
  where
    a' = shift d v a
shift d v (Note _ b) = b'
  where
    b' = shift d v b
-- The Dhall compiler enforces that all embedded values are closed expressions
-- and `shift` does nothing to a closed expression
shift _ _ (Embed p) = Embed p

{-| Substitute all occurrences of a variable with an expression

> subst x C B  ~  B[x := C]
-}
subst :: Var -> Expr s a -> Expr t a -> Expr s a
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
subst _ _ Double = Double
subst _ _ (DoubleLit a) = DoubleLit a
subst _ _ Text = Text
subst _ _ (TextLit a) = TextLit a
subst x e (TextAppend a b) = TextAppend a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst _ _ List = List
subst x e (ListLit a b) = ListLit a' b'
  where
    a' =       subst x e  a
    b' = fmap (subst x e) b
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
subst x e (Record       kts) = Record                   (fmap (subst x e) kts)
subst x e (RecordLit    kvs) = RecordLit                (fmap (subst x e) kvs)
subst x e (Union        kts) = Union                    (fmap (subst x e) kts)
subst x e (UnionLit a b kts) = UnionLit a (subst x e b) (fmap (subst x e) kts)
subst x e (Combine a b) = Combine a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (Merge a b c) = Merge a' b' c'
  where
    a' = subst x e a
    b' = subst x e b
    c' = subst x e c
subst x e (Field a b) = Field a' b
  where
    a' = subst x e a
subst x e (Note _ b) = b'
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
normalize :: Expr s a -> Expr t a
normalize e = case e of
    Const k -> Const k
    Var v -> Var v
    Lam x _A b -> Lam x _A' b'
      where
        _A' = normalize _A
        b'  = normalize b
    Pi  x _A _B -> Pi  x _A' _B'
      where
        _A' = normalize _A
        _B' = normalize _B
    App f a -> case normalize f of
        Lam x _A b -> normalize b''  -- Beta reduce
          where
            a'  = shift   1  (V x 0) a
            b'  = subst (V x 0) a' b
            b'' = shift (-1) (V x 0) b'
        f' -> case App f' a' of
            -- fold/build fusion for `List`
            App (App ListBuild _) (App (App ListFold _) e') -> normalize e'
            App (App ListFold _) (App (App ListBuild _) e') -> normalize e'

            -- fold/build fusion for `Natural`
            App NaturalBuild (App NaturalFold e') -> normalize e'
            App NaturalFold (App NaturalBuild e') -> normalize e'

            App (App (App (App NaturalFold (NaturalLit n0)) _) succ') zero ->
                normalize (go n0)
              where
                go !0 = zero
                go !n = App succ' (go (n - 1))
            App NaturalBuild k
                | check     -> NaturalLit n
                | otherwise -> App f' a'
              where
                labeled =
                    normalize (App (App (App k Natural) "Succ") "Zero")

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
                    go  _                            = internalError text
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
                normalize (OptionalLit t (Data.Vector.take 1 ys))
            App (App ListLast _) (ListLit t ys) ->
                normalize (OptionalLit t y)
              where
                y = if Data.Vector.null ys
                    then Data.Vector.empty
                    else Data.Vector.singleton (Data.Vector.last ys)
            App (App ListIndexed _) (ListLit t xs) ->
                normalize (ListLit t' (fmap adapt (Data.Vector.indexed xs)))
              where
                t' = Record (Data.Map.fromList kts)
                  where
                    kts = [ ("index", Natural)
                          , ("value", t)
                          ]
                adapt (n, x) = RecordLit (Data.Map.fromList kvs)
                  where
                    kvs = [ ("index", NaturalLit (fromIntegral n))
                          , ("value", x)
                          ]
            App (App ListReverse _) (ListLit t xs) ->
                normalize (ListLit t (Data.Vector.reverse xs))
            App (App (App (App (App OptionalFold _) (OptionalLit _ xs)) _) just) nothing ->
                normalize (maybe nothing just' (toMaybe xs))
              where
                just' y = App just y
                toMaybe = Data.Maybe.listToMaybe . Data.Vector.toList
            _ -> App f' a'
          where
            a' = normalize a
    Let f _ r b -> normalize b''
      where
        r'  = shift   1  (V f 0) r
        b'  = subst (V f 0) r' b
        b'' = shift (-1) (V f 0) b'
    Annot x _ -> normalize x
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
    Natural -> Natural
    NaturalLit n -> NaturalLit n
    NaturalFold -> NaturalFold
    NaturalBuild -> NaturalBuild
    NaturalIsZero -> NaturalIsZero
    NaturalEven -> NaturalEven
    NaturalOdd -> NaturalOdd
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
    Integer -> Integer
    IntegerLit n -> IntegerLit n
    Double -> Double
    DoubleLit n -> DoubleLit n
    Text -> Text
    TextLit t -> TextLit t
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
    List -> List
    ListLit t es -> ListLit t' es'
      where
        t'  =      normalize t
        es' = fmap normalize es
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
        t'  =      normalize t
        es' = fmap normalize es
    OptionalFold -> OptionalFold
    Record kts -> Record kts'
      where
        kts' = fmap normalize kts
    RecordLit kvs -> RecordLit kvs'
      where
        kvs' = fmap normalize kvs
    Union kts -> Union kts'
      where
        kts' = fmap normalize kts
    UnionLit k v kvs -> UnionLit k v' kvs'
      where
        v'   =      normalize v
        kvs' = fmap normalize kvs
    Combine x y ->
        case x of
            RecordLit kvsX ->
                case y of
                    RecordLit kvsY ->
                        RecordLit (fmap normalize (Data.Map.union kvsX kvsY))
                    _ -> Combine x' y'
            _ -> Combine x' y'
      where
        x' = normalize x
        y' = normalize y
    Merge x y t      ->
        case x' of
            RecordLit kvsX ->
                case y' of
                    UnionLit kY vY _ ->
                        case Data.Map.lookup kY kvsX of
                            Just vX -> normalize (App vX vY)
                            Nothing -> Merge x' y' t'
                    _ -> Merge x' y' t'
            _ -> Merge x' y' t'
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
    Note _ e' -> normalize e'
    Embed a -> Embed a
  where
    -- This is to avoid a `Show` constraint on the @a@ and @s@ in the type of
    -- `normalize`.  In theory, this might change a failing repro case into
    -- a successful one, but the risk of that is low enough to not warrant
    -- the `Show` constraint.  I care more about proving at the type level
    -- that the @a@ and @s@ type parameters are never used
    e'' = bimap (\_ -> ()) (\_ -> ()) e

    text = "normalize (" <> Data.Text.pack (show e'') <> ")"

internalError :: Data.Text.Text -> forall b . b
internalError text = error (Data.Text.unpack [NeatInterpolation.text|
Error: Compiler bug

Explanation: This error message means that there is a bug in the Dhall compiler.
You didn't do anything wrong, but if you would like to see this problem fixed
then you should report the bug at:

https://github.com/Gabriel439/Haskell-Dhall-Library/issues

Please include the following text in your bug report:

```
$text
```
|])

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
