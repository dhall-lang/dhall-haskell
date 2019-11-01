{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax      #-}

{-| This module contains the core syntax types and optics for them.

'reservedIdentifiers', 'denote' and friends are included because they are
involved in a dependency circle with "Dhall.Pretty.Internal".
-}

module Dhall.Syntax (
    -- * 'Expr'
      Const(..)
    , Var(..)
    , Binding(..)
    , makeBinding
    , Chunks(..)
    , DhallDouble(..)
    , Expr(..)

    -- ** 'Let'-blocks
    , MultiLet(..)
    , multiLet
    , wrapInLets

    -- ** Optics
    , subExpressions
    , chunkExprs
    , bindingExprs

    -- ** Handling 'Note's
    , denote
    , renote
    , shallowDenote

    -- * 'Import'
    , Directory(..)
    , File(..)
    , FilePrefix(..)
    , Import(..)
    , ImportHashed(..)
    , ImportMode(..)
    , ImportType(..)
    , URL(..)
    , Scheme(..)
    , pathCharacter

    -- * Reserved identifiers
    , reservedIdentifiers
    ) where

import Control.DeepSeq (NFData)
import Data.Bifunctor (Bifunctor(..))
import Data.Bits (xor)
import Data.Data (Data)
import Data.Foldable
import Data.HashSet (HashSet)
import Data.List.NonEmpty (NonEmpty(..))
import Data.String (IsString(..))
import Data.Semigroup (Semigroup(..))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty)
import Data.Traversable
import Data.Void (Void)
import Dhall.Map (Map)
import Dhall.Set (Set)
import Dhall.Src (Src(..))
import {-# SOURCE #-} Dhall.Pretty.Internal
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Natural (Natural)
import Prelude hiding (succ)
import Unsafe.Coerce (unsafeCoerce)

import qualified Control.Monad
import qualified Data.HashSet
import qualified Data.List.NonEmpty
import qualified Data.Text
import qualified Data.Text.Prettyprint.Doc    as Pretty
import qualified Dhall.Crypto
import qualified Network.URI                  as URI

{-| Constants for a pure type system

    The axioms are:

> ⊦ Type : Kind
> ⊦ Kind : Sort

    ... and the valid rule pairs are:

> ⊦ Type ↝ Type : Type  -- Functions from terms to terms (ordinary functions)
> ⊦ Kind ↝ Type : Type  -- Functions from types to terms (type-polymorphic functions)
> ⊦ Sort ↝ Type : Type  -- Functions from kinds to terms
> ⊦ Kind ↝ Kind : Kind  -- Functions from types to types (type-level functions)
> ⊦ Sort ↝ Kind : Sort  -- Functions from kinds to types (kind-polymorphic functions)
> ⊦ Sort ↝ Sort : Sort  -- Functions from kinds to kinds (kind-level functions)

    Note that Dhall does not support functions from terms to types and therefore
    Dhall is not a dependently typed language
-}
data Const = Type | Kind | Sort
    deriving (Show, Eq, Ord, Data, Bounded, Enum, Generic, NFData)

instance Lift Const

instance Pretty Const where
    pretty = Pretty.unAnnotate . prettyConst

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
data Var = V Text !Int
    deriving (Data, Generic, Eq, Ord, Show, NFData)

instance Lift Var

instance IsString Var where
    fromString str = V (fromString str) 0

instance Pretty Var where
    pretty = Pretty.unAnnotate . prettyVar

{- | Record the binding part of a @let@ expression.

For example,
> let {- A -} x {- B -} : {- C -} Bool = {- D -} True in x
will be instantiated as follows:

* @bindingSrc0@ corresponds to the @A@ comment.
* @variable@ is @"x"@
* @bindingSrc1@ corresponds to the @B@ comment.
* @annotation@ is 'Just' a pair, corresponding to the @C@ comment and @Bool@.
* @bindingSrc2@ corresponds to the @D@ comment.
* @value@ corresponds to @True@.
-}
data Binding s a = Binding
    { bindingSrc0 :: Maybe s
    , variable    :: Text
    , bindingSrc1 :: Maybe s
    , annotation  :: Maybe (Maybe s, Expr s a)
    , bindingSrc2 :: Maybe s
    , value       :: Expr s a
    } deriving (Data, Eq, Foldable, Functor, Generic, NFData, Ord, Show, Traversable)

instance Bifunctor Binding where
    first k (Binding src0 a src1 b src2 c) =
        Binding (fmap k src0) a (fmap k src1) (fmap adapt0 b) (fmap k src2) (first k c)
      where
        adapt0 (src3, d) = (fmap k src3, first k d)

    second = fmap

{-| Construct a 'Binding' with no source information and no type annotation.
-}
makeBinding :: Text -> Expr s a -> Binding s a
makeBinding name = Binding Nothing name Nothing Nothing Nothing

-- | This wrapper around 'Prelude.Double' exists for its 'Eq' instance which is
-- defined via the binary encoding of Dhall @Double@s.
newtype DhallDouble = DhallDouble { getDhallDouble :: Double }
    deriving (Show, Data, NFData, Generic)

-- | This instance satisfies all the customary 'Eq' laws except substitutivity.
--
-- In particular:
--
-- >>> nan = DhallDouble (0/0)
-- >>> nan == nan
-- True
--
-- This instance is also consistent with with the binary encoding of Dhall @Double@s:
--
-- >>> toBytes n = Dhall.Binary.encodeExpression (DoubleLit n :: Expr Void Import)
--
-- prop> \a b -> (a == b) == (toBytes a == toBytes b)
instance Eq DhallDouble where
    DhallDouble a == DhallDouble b
        | isNaN a && isNaN b                      = True
        | isNegativeZero a `xor` isNegativeZero b = False
        | otherwise                               = a == b

-- | This instance relies on the 'Eq' instance for 'DhallDouble' but cannot
-- satisfy the customary 'Ord' laws when @NaN@ is involved.
instance Ord DhallDouble where
    compare a@(DhallDouble a') b@(DhallDouble b') =
        if a == b
            then EQ
            else compare a' b'

-- | The body of an interpolated @Text@ literal
data Chunks s a = Chunks [(Text, Expr s a)] Text
    deriving (Functor, Foldable, Generic, Traversable, Show, Eq, Ord, Data, NFData)

instance (Lift s, Lift a, Data s, Data a) => Lift (Chunks s a)

instance Data.Semigroup.Semigroup (Chunks s a) where
    Chunks xysL zL <> Chunks         []    zR =
        Chunks xysL (zL <> zR)
    Chunks xysL zL <> Chunks ((x, y):xysR) zR =
        Chunks (xysL ++ (zL <> x, y):xysR) zR

instance Monoid (Chunks s a) where
    mempty = Chunks [] mempty

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

instance IsString (Chunks s a) where
    fromString str = Chunks [] (fromString str)

{-| Syntax tree for expressions

    The @s@ type parameter is used to track the presence or absence of `Src`
    spans:

    * If @s = `Src`@ then the code may contains `Src` spans (either in a `Noted`
      constructor or inline within another constructor, like `Let`)
    * If @s = `Void`@ then the code has no `Src` spans

    The @a@ type parameter is used to track the presence or absence of imports

    * If @a = `Import`@ then the code may contain unresolved `Import`s
    * If @a = `Void`@ then the code has no `Import`s
-}
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
    -- | > Let (Binding _ x _  Nothing  _ r) e      ~  let x     = r in e
    --   > Let (Binding _ x _ (Just t ) _ r) e      ~  let x : t = r in e
    --
    -- The difference between
    --
    -- > let x = a    let y = b in e
    --
    -- and
    --
    -- > let x = a in let y = b in e
    --
    -- is only an additional 'Note' around @'Let' "y" …@ in the second
    -- example.
    --
    -- See 'MultiLet' for a representation of let-blocks that mirrors the
    -- source code more closely.
    | Let (Binding s a) (Expr s a)
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
    -- | > NaturalLit n                             ~  n
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
    -- | > NaturalSubtract                          ~  Natural/subtract
    | NaturalSubtract
    -- | > NaturalPlus x y                          ~  x + y
    | NaturalPlus (Expr s a) (Expr s a)
    -- | > NaturalTimes x y                         ~  x * y
    | NaturalTimes (Expr s a) (Expr s a)
    -- | > Integer                                  ~  Integer
    | Integer
    -- | > IntegerLit n                             ~  ±n
    | IntegerLit Integer
    -- | IntegerClamp                               ~  Integer/clamp
    | IntegerClamp
    -- | IntegerNegate                              ~  Integer/negate
    | IntegerNegate
    -- | > IntegerShow                              ~  Integer/show
    | IntegerShow
    -- | > IntegerToDouble                          ~  Integer/toDouble
    | IntegerToDouble
    -- | > Double                                   ~  Double
    | Double
    -- | > DoubleLit n                              ~  n
    | DoubleLit DhallDouble
    -- | > DoubleShow                               ~  Double/show
    | DoubleShow
    -- | > Text                                     ~  Text
    | Text
    -- | > TextLit (Chunks [(t1, e1), (t2, e2)] t3) ~  "t1${e1}t2${e2}t3"
    | TextLit (Chunks s a)
    -- | > TextAppend x y                           ~  x ++ y
    | TextAppend (Expr s a) (Expr s a)
    -- | > TextShow                                 ~  Text/show
    | TextShow
    -- | > List                                     ~  List
    | List
    -- | > ListLit (Just t ) []                     ~  [] : t
    --   > ListLit  Nothing  [x, y, z]              ~  [x, y, z]
    --
    --   Invariant: A non-empty list literal is always represented as
    --   @ListLit Nothing xs@.
    --
    --   When an annotated, non-empty list literal is parsed, it is represented
    --   as
    --
    --   > Annot (ListLit Nothing [x, y, z]) t      ~ [x, y, z] : t

    -- Eventually we should have separate constructors for empty and non-empty
    -- list literals. For now it's easier to check the invariant in @infer@.
    -- See https://github.com/dhall-lang/dhall-haskell/issues/1359#issuecomment-537087234.
    | ListLit (Maybe (Expr s a)) (Seq (Expr s a))
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
    -- | > Some e                                   ~  Some e
    | Some (Expr s a)
    -- | > None                                     ~  None
    | None
    -- | > OptionalFold                             ~  Optional/fold
    | OptionalFold
    -- | > OptionalBuild                            ~  Optional/build
    | OptionalBuild
    -- | > Record       [(k1, t1), (k2, t2)]        ~  { k1 : t1, k2 : t1 }
    | Record    (Map Text (Expr s a))
    -- | > RecordLit    [(k1, v1), (k2, v2)]        ~  { k1 = v1, k2 = v2 }
    | RecordLit (Map Text (Expr s a))
    -- | > Union        [(k1, Just t1), (k2, Nothing)] ~  < k1 : t1 | k2 >
    | Union     (Map Text (Maybe (Expr s a)))
    -- | > Combine x y                              ~  x ∧ y
    | Combine (Expr s a) (Expr s a)
    -- | > CombineTypes x y                         ~  x ⩓ y
    | CombineTypes (Expr s a) (Expr s a)
    -- | > Prefer x y                               ~  x ⫽ y
    | Prefer (Expr s a) (Expr s a)
    -- | > RecordCompletion x y                     ~  x::y
    | RecordCompletion (Expr s a) (Expr s a)
    -- | > Merge x y (Just t )                      ~  merge x y : t
    --   > Merge x y  Nothing                       ~  merge x y
    | Merge (Expr s a) (Expr s a) (Maybe (Expr s a))
    -- | > ToMap x (Just t)                         ~  toMap x : t
    --   > ToMap x  Nothing                         ~  toMap x
    | ToMap (Expr s a) (Maybe (Expr s a))
    -- | > Field e x                                ~  e.x
    | Field (Expr s a) Text
    -- | > Project e (Left xs)                      ~  e.{ xs }
    -- | > Project e (Right t)                      ~  e.(t)
    | Project (Expr s a) (Either (Set Text) (Expr s a))
    -- | > Assert e                                 ~  assert : e
    | Assert (Expr s a)
    -- | > Equivalent x y                           ~  x ≡ y
    | Equivalent (Expr s a) (Expr s a)
    -- | > Note s x                                 ~  e
    | Note s (Expr s a)
    -- | > ImportAlt                                ~  e1 ? e2
    | ImportAlt (Expr s a) (Expr s a)
    -- | > Embed import                             ~  import
    | Embed a
    deriving (Foldable, Generic, Traversable, Show, Data, NFData)
-- NB: If you add a constructor to Expr, please also update the Arbitrary
-- instance in Dhall.Test.QuickCheck.

-- | This instance encodes what the Dhall standard calls an \"exact match\"
-- between two expressions.
--
-- Note that
--
-- >>> nan = DhallDouble (0/0)
-- >>> DoubleLit nan == DoubleLit nan
-- True
deriving instance (Eq s, Eq a) => Eq (Expr s a)

-- | Note that this 'Ord' instance inherits `DhallDouble`'s defects.
deriving instance (Ord s, Ord a) => Ord (Expr s a)

instance (Lift s, Lift a, Data s, Data a) => Lift (Expr s a)

-- This instance is hand-written due to the fact that deriving
-- it does not give us an INLINABLE pragma. We annotate this fmap
-- implementation with this pragma below to allow GHC to, possibly,
-- inline the implementation for performance improvements.
instance Functor (Expr s) where
  fmap _ (Const c) = Const c
  fmap _ (Var v) = Var v
  fmap f (Lam v e1 e2) = Lam v (fmap f e1) (fmap f e2)
  fmap f (Pi v e1 e2) = Pi v (fmap f e1) (fmap f e2)
  fmap f (App e1 e2) = App (fmap f e1) (fmap f e2)
  fmap f (Let b e2) = Let (fmap f b) (fmap f e2)
  fmap f (Annot e1 e2) = Annot (fmap f e1) (fmap f e2)
  fmap _ Bool = Bool
  fmap _ (BoolLit b) = BoolLit b
  fmap f (BoolAnd e1 e2) = BoolAnd (fmap f e1) (fmap f e2)
  fmap f (BoolOr e1 e2) = BoolOr (fmap f e1) (fmap f e2)
  fmap f (BoolEQ e1 e2) = BoolEQ (fmap f e1) (fmap f e2)
  fmap f (BoolNE e1 e2) = BoolNE (fmap f e1) (fmap f e2)
  fmap f (BoolIf e1 e2 e3) = BoolIf (fmap f e1) (fmap f e2) (fmap f e3)
  fmap _ Natural = Natural
  fmap _ (NaturalLit n) = NaturalLit n
  fmap _ NaturalFold = NaturalFold
  fmap _ NaturalBuild = NaturalBuild
  fmap _ NaturalIsZero = NaturalIsZero
  fmap _ NaturalEven = NaturalEven
  fmap _ NaturalOdd = NaturalOdd
  fmap _ NaturalToInteger = NaturalToInteger
  fmap _ NaturalShow = NaturalShow
  fmap _ NaturalSubtract = NaturalSubtract
  fmap f (NaturalPlus e1 e2) = NaturalPlus (fmap f e1) (fmap f e2)
  fmap f (NaturalTimes e1 e2) = NaturalTimes (fmap f e1) (fmap f e2)
  fmap _ Integer = Integer
  fmap _ (IntegerLit i) = IntegerLit i
  fmap _ IntegerClamp = IntegerClamp
  fmap _ IntegerNegate = IntegerNegate
  fmap _ IntegerShow = IntegerShow
  fmap _ IntegerToDouble = IntegerToDouble
  fmap _ Double = Double
  fmap _ (DoubleLit d) = DoubleLit d
  fmap _ DoubleShow = DoubleShow
  fmap _ Text = Text
  fmap f (TextLit cs) = TextLit (fmap f cs)
  fmap f (TextAppend e1 e2) = TextAppend (fmap f e1) (fmap f e2)
  fmap _ TextShow = TextShow
  fmap _ List = List
  fmap f (ListLit maybeE seqE) = ListLit (fmap (fmap f) maybeE) (fmap (fmap f) seqE)
  fmap f (ListAppend e1 e2) = ListAppend (fmap f e1) (fmap f e2)
  fmap _ ListBuild = ListBuild
  fmap _ ListFold = ListFold
  fmap _ ListLength = ListLength
  fmap _ ListHead = ListHead
  fmap _ ListLast = ListLast
  fmap _ ListIndexed = ListIndexed
  fmap _ ListReverse = ListReverse
  fmap _ Optional = Optional
  fmap f (Some e) = Some (fmap f e)
  fmap _ None = None
  fmap _ OptionalFold = OptionalFold
  fmap _ OptionalBuild = OptionalBuild
  fmap f (Record r) = Record (fmap (fmap f) r)
  fmap f (RecordLit r) = RecordLit (fmap (fmap f) r)
  fmap f (Union u) = Union (fmap (fmap (fmap f)) u)
  fmap f (Combine e1 e2) = Combine (fmap f e1) (fmap f e2)
  fmap f (CombineTypes e1 e2) = CombineTypes (fmap f e1) (fmap f e2)
  fmap f (Prefer e1 e2) = Prefer (fmap f e1) (fmap f e2)
  fmap f (RecordCompletion e1 e2) = RecordCompletion (fmap f e1) (fmap f e2)
  fmap f (Merge e1 e2 maybeE) = Merge (fmap f e1) (fmap f e2) (fmap (fmap f) maybeE)
  fmap f (ToMap e maybeE) = ToMap (fmap f e) (fmap (fmap f) maybeE)
  fmap f (Field e1 v) = Field (fmap f e1) v
  fmap f (Project e1 vs) = Project (fmap f e1) (fmap (fmap f) vs)
  fmap f (Assert t) = Assert (fmap f t)
  fmap f (Equivalent e1 e2) = Equivalent (fmap f e1) (fmap f e2)
  fmap f (Note s e1) = Note s (fmap f e1)
  fmap f (ImportAlt e1 e2) = ImportAlt (fmap f e1) (fmap f e2)
  fmap f (Embed a) = Embed (f a)
  {-# INLINABLE fmap #-}

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
    Let a b              >>= k = Let (adapt0 a) (b >>= k)
      where
        adapt0 (Binding src0 c src1 d src2 e) =
            Binding src0 c src1 (fmap adapt1 d) src2 (e >>= k)

        adapt1 (src3, f) = (src3, f >>= k)
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
    NaturalSubtract      >>= _ = NaturalSubtract
    NaturalPlus  a b     >>= k = NaturalPlus  (a >>= k) (b >>= k)
    NaturalTimes a b     >>= k = NaturalTimes (a >>= k) (b >>= k)
    Integer              >>= _ = Integer
    IntegerLit a         >>= _ = IntegerLit a
    IntegerClamp         >>= _ = IntegerClamp
    IntegerNegate        >>= _ = IntegerNegate
    IntegerShow          >>= _ = IntegerShow
    IntegerToDouble      >>= _ = IntegerToDouble
    Double               >>= _ = Double
    DoubleLit a          >>= _ = DoubleLit a
    DoubleShow           >>= _ = DoubleShow
    Text                 >>= _ = Text
    TextLit (Chunks a b) >>= k = TextLit (Chunks (fmap (fmap (>>= k)) a) b)
    TextAppend a b       >>= k = TextAppend (a >>= k) (b >>= k)
    TextShow             >>= _ = TextShow
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
    Some a               >>= k = Some (a >>= k)
    None                 >>= _ = None
    OptionalFold         >>= _ = OptionalFold
    OptionalBuild        >>= _ = OptionalBuild
    Record    a          >>= k = Record (fmap (>>= k) a)
    RecordLit a          >>= k = RecordLit (fmap (>>= k) a)
    Union     a          >>= k = Union (fmap (fmap (>>= k)) a)
    Combine a b          >>= k = Combine (a >>= k) (b >>= k)
    CombineTypes a b     >>= k = CombineTypes (a >>= k) (b >>= k)
    Prefer a b           >>= k = Prefer (a >>= k) (b >>= k)
    RecordCompletion a b >>= k = RecordCompletion (a >>= k) (b >>= k)
    Merge a b c          >>= k = Merge (a >>= k) (b >>= k) (fmap (>>= k) c)
    ToMap a b            >>= k = ToMap (a >>= k) (fmap (>>= k) b)
    Field a b            >>= k = Field (a >>= k) b
    Project a b          >>= k = Project (a >>= k) (fmap (>>= k) b)
    Assert a             >>= k = Assert (a >>= k)
    Equivalent a b       >>= k = Equivalent (a >>= k) (b >>= k)
    Note a b             >>= k = Note a (b >>= k)
    ImportAlt a b        >>= k = ImportAlt (a >>= k) (b >>= k)
    Embed a              >>= k = k a

instance Bifunctor Expr where
    first _ (Const a             ) = Const a
    first _ (Var a               ) = Var a
    first k (Lam a b c           ) = Lam a (first k b) (first k c)
    first k (Pi a b c            ) = Pi a (first k b) (first k c)
    first k (App a b             ) = App (first k a) (first k b)
    first k (Let a b             ) = Let (first k a) (first k b)
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
    first _  NaturalSubtract       = NaturalSubtract
    first k (NaturalPlus a b     ) = NaturalPlus (first k a) (first k b)
    first k (NaturalTimes a b    ) = NaturalTimes (first k a) (first k b)
    first _  Integer               = Integer
    first _ (IntegerLit a        ) = IntegerLit a
    first _  IntegerClamp          = IntegerClamp
    first _  IntegerNegate         = IntegerNegate
    first _  IntegerShow           = IntegerShow
    first _  IntegerToDouble       = IntegerToDouble
    first _  Double                = Double
    first _ (DoubleLit a         ) = DoubleLit a
    first _  DoubleShow            = DoubleShow
    first _  Text                  = Text
    first k (TextLit (Chunks a b)) = TextLit (Chunks (fmap (fmap (first k)) a) b)
    first k (TextAppend a b      ) = TextAppend (first k a) (first k b)
    first _  TextShow              = TextShow
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
    first k (Some a              ) = Some (first k a)
    first _  None                  = None
    first _  OptionalFold          = OptionalFold
    first _  OptionalBuild         = OptionalBuild
    first k (Record a            ) = Record (fmap (first k) a)
    first k (RecordLit a         ) = RecordLit (fmap (first k) a)
    first k (Union a             ) = Union (fmap (fmap (first k)) a)
    first k (Combine a b         ) = Combine (first k a) (first k b)
    first k (CombineTypes a b    ) = CombineTypes (first k a) (first k b)
    first k (Prefer a b          ) = Prefer (first k a) (first k b)
    first k (RecordCompletion a b) = RecordCompletion (first k a) (first k b)
    first k (Merge a b c         ) = Merge (first k a) (first k b) (fmap (first k) c)
    first k (ToMap a b           ) = ToMap (first k a) (fmap (first k) b)
    first k (Field a b           ) = Field (first k a) b
    first k (Assert a            ) = Assert (first k a)
    first k (Equivalent a b      ) = Equivalent (first k a) (first k b)
    first k (Project a b         ) = Project (first k a) (fmap (first k) b)
    first k (Note a b            ) = Note (k a) (first k b)
    first k (ImportAlt a b       ) = ImportAlt (first k a) (first k b)
    first _ (Embed a             ) = Embed a

    second = fmap

instance IsString (Expr s a) where
    fromString str = Var (fromString str)

-- | Generates a syntactically valid Dhall program
instance Pretty a => Pretty (Expr s a) where
    pretty = Pretty.unAnnotate . prettyExpr

{-
Instead of converting explicitly between 'Expr's and 'MultiLet', it might
be nicer to use a pattern synonym:

> pattern MultiLet' :: NonEmpty (Binding s a) -> Expr s a -> Expr s a
> pattern MultiLet' as b <- (multiLetFromExpr -> Just (MultiLet as b)) where
>   MultiLet' as b = wrapInLets as b
>
> multiLetFromExpr :: Expr s a -> Maybe (MultiLet s a)
> multiLetFromExpr = \case
>     Let x mA a b -> Just (multiLet x mA a b)
>     _ -> Nothing

This works in principle, but GHC as of v8.8.1 doesn't handle it well:
https://gitlab.haskell.org/ghc/ghc/issues/17096

This should be fixed by GHC-8.10, so it might be worth revisiting then.
-}

{-| Generate a 'MultiLet' from the contents of a 'Let'.

    In the resulting @'MultiLet' bs e@, @e@ is guaranteed not to be a 'Let',
    but it might be a @('Note' … ('Let' …))@.

    Given parser output, 'multiLet' consolidates @let@s that formed a
    let-block in the original source.
-}
multiLet :: Binding s a -> Expr s a -> MultiLet s a
multiLet b0 = \case
    Let b1 e1 ->
        let MultiLet bs e = multiLet b1 e1
        in  MultiLet (Data.List.NonEmpty.cons b0 bs) e
    e -> MultiLet (b0 :| []) e

{-| Wrap let-'Binding's around an 'Expr'.

'wrapInLets' can be understood as an inverse for 'multiLet':

> let MultiLet bs e1 = multiLet b e0
>
> wrapInLets bs e1 == Let b e0
-}
wrapInLets :: Foldable f => f (Binding s a) -> Expr s a -> Expr s a
wrapInLets bs e = foldr Let e bs

{-| This type represents 1 or more nested `Let` bindings that have been
    coalesced together for ease of manipulation
-}
data MultiLet s a = MultiLet (NonEmpty (Binding s a)) (Expr s a)

-- | A traversal over the immediate sub-expressions of an expression.
subExpressions :: Applicative f => (Expr s a -> f (Expr s a)) -> Expr s a -> f (Expr s a)
subExpressions _ (Const c) = pure (Const c)
subExpressions _ (Var v) = pure (Var v)
subExpressions f (Lam a b c) = Lam a <$> f b <*> f c
subExpressions f (Pi a b c) = Pi a <$> f b <*> f c
subExpressions f (App a b) = App <$> f a <*> f b
subExpressions f (Let a b) = Let <$> bindingExprs f a <*> f b
subExpressions f (Annot a b) = Annot <$> f a <*> f b
subExpressions _ Bool = pure Bool
subExpressions _ (BoolLit b) = pure (BoolLit b)
subExpressions f (BoolAnd a b) = BoolAnd <$> f a <*> f b
subExpressions f (BoolOr a b) = BoolOr <$> f a <*> f b
subExpressions f (BoolEQ a b) = BoolEQ <$> f a <*> f b
subExpressions f (BoolNE a b) = BoolNE <$> f a <*> f b
subExpressions f (BoolIf a b c) = BoolIf <$> f a <*> f b <*> f c
subExpressions _ Natural = pure Natural
subExpressions _ (NaturalLit n) = pure (NaturalLit n)
subExpressions _ NaturalFold = pure NaturalFold
subExpressions _ NaturalBuild = pure NaturalBuild
subExpressions _ NaturalIsZero = pure NaturalIsZero
subExpressions _ NaturalEven = pure NaturalEven
subExpressions _ NaturalOdd = pure NaturalOdd
subExpressions _ NaturalToInteger = pure NaturalToInteger
subExpressions _ NaturalShow = pure NaturalShow
subExpressions _ NaturalSubtract = pure NaturalSubtract
subExpressions f (NaturalPlus a b) = NaturalPlus <$> f a <*> f b
subExpressions f (NaturalTimes a b) = NaturalTimes <$> f a <*> f b
subExpressions _ Integer = pure Integer
subExpressions _ (IntegerLit n) = pure (IntegerLit n)
subExpressions _ IntegerClamp = pure IntegerClamp
subExpressions _ IntegerNegate = pure IntegerNegate
subExpressions _ IntegerShow = pure IntegerShow
subExpressions _ IntegerToDouble = pure IntegerToDouble
subExpressions _ Double = pure Double
subExpressions _ (DoubleLit n) = pure (DoubleLit n)
subExpressions _ DoubleShow = pure DoubleShow
subExpressions _ Text = pure Text
subExpressions f (TextLit chunks) =
    TextLit <$> chunkExprs f chunks
subExpressions f (TextAppend a b) = TextAppend <$> f a <*> f b
subExpressions _ TextShow = pure TextShow
subExpressions _ List = pure List
subExpressions f (ListLit a b) = ListLit <$> traverse f a <*> traverse f b
subExpressions f (ListAppend a b) = ListAppend <$> f a <*> f b
subExpressions _ ListBuild = pure ListBuild
subExpressions _ ListFold = pure ListFold
subExpressions _ ListLength = pure ListLength
subExpressions _ ListHead = pure ListHead
subExpressions _ ListLast = pure ListLast
subExpressions _ ListIndexed = pure ListIndexed
subExpressions _ ListReverse = pure ListReverse
subExpressions _ Optional = pure Optional
subExpressions f (Some a) = Some <$> f a
subExpressions _ None = pure None
subExpressions _ OptionalFold = pure OptionalFold
subExpressions _ OptionalBuild = pure OptionalBuild
subExpressions f (Record a) = Record <$> traverse f a
subExpressions f ( RecordLit a ) = RecordLit <$> traverse f a
subExpressions f (Union a) = Union <$> traverse (traverse f) a
subExpressions f (Combine a b) = Combine <$> f a <*> f b
subExpressions f (CombineTypes a b) = CombineTypes <$> f a <*> f b
subExpressions f (Prefer a b) = Prefer <$> f a <*> f b
subExpressions f (RecordCompletion a b) = RecordCompletion <$> f a <*> f b
subExpressions f (Merge a b t) = Merge <$> f a <*> f b <*> traverse f t
subExpressions f (ToMap a t) = ToMap <$> f a <*> traverse f t
subExpressions f (Field a b) = Field <$> f a <*> pure b
subExpressions f (Project a b) = Project <$> f a <*> traverse f b
subExpressions f (Assert a) = Assert <$> f a
subExpressions f (Equivalent a b) = Equivalent <$> f a <*> f b
subExpressions f (Note a b) = Note a <$> f b
subExpressions f (ImportAlt l r) = ImportAlt <$> f l <*> f r
subExpressions _ (Embed a) = pure (Embed a)

{-| Traverse over the immediate 'Expr' children in a 'Binding'.
-}
bindingExprs
  :: (Applicative f)
  => (Expr s a -> f (Expr s b))
  -> Binding s a -> f (Binding s b)
bindingExprs f (Binding s0 n s1 t s2 v) =
  Binding
    <$> pure s0
    <*> pure n
    <*> pure s1
    <*> traverse (traverse f) t
    <*> pure s2
    <*> f v

-- | A traversal over the immediate sub-expressions in 'Chunks'.
chunkExprs
  :: Applicative f
  => (Expr s a -> f (Expr t b))
  -> Chunks s a -> f (Chunks t b)
chunkExprs f (Chunks chunks final) =
  flip Chunks final <$> traverse (traverse f) chunks

{-| Internal representation of a directory that stores the path components in
    reverse order

    In other words, the directory @\/foo\/bar\/baz@ is encoded as
    @Directory { components = [ "baz", "bar", "foo" ] }@
-}
newtype Directory = Directory { components :: [Text] }
    deriving (Eq, Generic, Ord, Show, NFData)

instance Semigroup Directory where
    Directory components₀ <> Directory components₁ =
        Directory (components₁ <> components₀)

instance Pretty Directory where
    pretty (Directory {..}) = foldMap prettyPathComponent (reverse components)

prettyPathComponent :: Text -> Doc ann
prettyPathComponent text
    | Data.Text.all pathCharacter text =
        "/" <> Pretty.pretty text
    | otherwise =
        "/\"" <> Pretty.pretty text <> "\""

{-| A `File` is a `directory` followed by one additional path component
    representing the `file` name
-}
data File = File
    { directory :: Directory
    , file      :: Text
    } deriving (Eq, Generic, Ord, Show, NFData)

instance Pretty File where
    pretty (File {..}) =
            Pretty.pretty directory
        <>  prettyPathComponent file

instance Semigroup File where
    File directory₀ _ <> File directory₁ file =
        File (directory₀ <> directory₁) file

-- | The beginning of a file path which anchors subsequent path components
data FilePrefix
    = Absolute
    -- ^ Absolute path
    | Here
    -- ^ Path relative to @.@
    | Parent
    -- ^ Path relative to @..@
    | Home
    -- ^ Path relative to @~@
    deriving (Eq, Generic, Ord, Show, NFData)

instance Pretty FilePrefix where
    pretty Absolute = ""
    pretty Here     = "."
    pretty Parent   = ".."
    pretty Home     = "~"

-- | The URI scheme
data Scheme = HTTP | HTTPS deriving (Eq, Generic, Ord, Show, NFData)

-- | This type stores all of the components of a remote import
data URL = URL
    { scheme    :: Scheme
    , authority :: Text
    , path      :: File
    , query     :: Maybe Text
    , headers   :: Maybe (Expr Src Import)
    } deriving (Eq, Generic, Ord, Show, NFData)

instance Pretty URL where
    pretty (URL {..}) =
            schemeDoc
        <>  "://"
        <>  Pretty.pretty authority
        <>  pathDoc
        <>  queryDoc
        <>  foldMap prettyHeaders headers
      where
        prettyHeaders h = " using " <> Pretty.pretty h

        File {..} = path

        Directory {..} = directory

        pathDoc =
                foldMap prettyURIComponent (reverse components)
            <>  prettyURIComponent file

        schemeDoc = case scheme of
            HTTP  -> "http"
            HTTPS -> "https"

        queryDoc = case query of
            Nothing -> ""
            Just q  -> "?" <> Pretty.pretty q

prettyURIComponent :: Text -> Doc ann
prettyURIComponent text =
        Pretty.pretty $ URI.normalizeCase $ URI.normalizeEscape $ "/" <> Data.Text.unpack text

-- | The type of import (i.e. local vs. remote vs. environment)
data ImportType
    = Local FilePrefix File
    -- ^ Local path
    | Remote URL
    -- ^ URL of remote resource and optional headers stored in an import
    | Env  Text
    -- ^ Environment variable
    | Missing
    deriving (Eq, Generic, Ord, Show, NFData)

parent :: File
parent = File { directory = Directory { components = [ ".." ] }, file = "" }

instance Semigroup ImportType where
    Local prefix file₀ <> Local Here file₁ = Local prefix (file₀ <> file₁)

    Remote (URL { path = path₀, ..}) <> Local Here path₁ =
        Remote (URL { path = path₀ <> path₁, ..})

    Local prefix file₀ <> Local Parent file₁ =
        Local prefix (file₀ <> parent <> file₁)

    Remote (URL { path = path₀, .. }) <> Local Parent path₁ =
        Remote (URL { path = path₀ <> parent <> path₁, .. })

    import₀ <> Remote (URL { headers = headers₀, .. }) =
        Remote (URL { headers = headers₁, .. })
      where
        importHashed₀ = Import (ImportHashed Nothing import₀) Code

        headers₁ = fmap (fmap (importHashed₀ <>)) headers₀

    _ <> import₁ =
        import₁

instance Pretty ImportType where
    pretty (Local prefix file) =
        Pretty.pretty prefix <> Pretty.pretty file

    pretty (Remote url) = Pretty.pretty url

    pretty (Env env) = "env:" <> Pretty.pretty env

    pretty Missing = "missing"

-- | How to interpret the import's contents (i.e. as Dhall code or raw text)
data ImportMode = Code | RawText | Location
  deriving (Eq, Generic, Ord, Show, NFData)

-- | A `ImportType` extended with an optional hash for semantic integrity checks
data ImportHashed = ImportHashed
    { hash       :: Maybe Dhall.Crypto.SHA256Digest
    , importType :: ImportType
    } deriving (Eq, Generic, Ord, Show, NFData)

instance Semigroup ImportHashed where
    ImportHashed _ importType₀ <> ImportHashed hash importType₁ =
        ImportHashed hash (importType₀ <> importType₁)

instance Pretty ImportHashed where
    pretty (ImportHashed  Nothing p) =
      Pretty.pretty p
    pretty (ImportHashed (Just h) p) =
      Pretty.pretty p <> " sha256:" <> Pretty.pretty (show h)

-- | Reference to an external resource
data Import = Import
    { importHashed :: ImportHashed
    , importMode   :: ImportMode
    } deriving (Eq, Generic, Ord, Show, NFData)

instance Semigroup Import where
    Import importHashed₀ _ <> Import importHashed₁ code =
        Import (importHashed₀ <> importHashed₁) code

instance Pretty Import where
    pretty (Import {..}) = Pretty.pretty importHashed <> Pretty.pretty suffix
      where
        suffix :: Text
        suffix = case importMode of
            RawText  -> " as Text"
            Location -> " as Location"
            Code     -> ""

{-| Returns `True` if the given `Char` is valid within an unquoted path
    component

    This is exported for reuse within the @"Dhall.Parser.Token"@ module
-}
pathCharacter :: Char -> Bool
pathCharacter c =
         '\x21' == c
    ||  ('\x24' <= c && c <= '\x27')
    ||  ('\x2A' <= c && c <= '\x2B')
    ||  ('\x2D' <= c && c <= '\x2E')
    ||  ('\x30' <= c && c <= '\x3B')
    ||  c == '\x3D'
    ||  ('\x40' <= c && c <= '\x5A')
    ||  ('\x5E' <= c && c <= '\x7A')
    ||  c == '\x7C'
    ||  c == '\x7E'

-- | Remove all `Note` constructors from an `Expr` (i.e. de-`Note`)
denote :: Expr s a -> Expr t a
denote (Note _ b            ) = denote b
denote (Const a             ) = Const a
denote (Var a               ) = Var a
denote (Lam a b c           ) = Lam a (denote b) (denote c)
denote (Pi a b c            ) = Pi a (denote b) (denote c)
denote (App a b             ) = App (denote a) (denote b)
denote (Let a b             ) = Let (adapt0 a) (denote b)
  where
    adapt0 (Binding _ c _ d _ e) =
        Binding Nothing c Nothing (fmap adapt1 d) Nothing (denote e)

    adapt1 (_, f) = (Nothing, denote f)
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
denote  NaturalSubtract       = NaturalSubtract
denote (NaturalPlus a b     ) = NaturalPlus (denote a) (denote b)
denote (NaturalTimes a b    ) = NaturalTimes (denote a) (denote b)
denote  Integer               = Integer
denote (IntegerLit a        ) = IntegerLit a
denote  IntegerClamp          = IntegerClamp
denote  IntegerNegate         = IntegerNegate
denote  IntegerShow           = IntegerShow
denote  IntegerToDouble       = IntegerToDouble
denote  Double                = Double
denote (DoubleLit a         ) = DoubleLit a
denote  DoubleShow            = DoubleShow
denote  Text                  = Text
denote (TextLit (Chunks a b)) = TextLit (Chunks (fmap (fmap denote) a) b)
denote (TextAppend a b      ) = TextAppend (denote a) (denote b)
denote  TextShow              = TextShow
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
denote (Some a              ) = Some (denote a)
denote  None                  = None
denote  OptionalFold          = OptionalFold
denote  OptionalBuild         = OptionalBuild
denote (Record a            ) = Record (fmap denote a)
denote (RecordLit a         ) = RecordLit (fmap denote a)
denote (Union a             ) = Union (fmap (fmap denote) a)
denote (Combine a b         ) = Combine (denote a) (denote b)
denote (CombineTypes a b    ) = CombineTypes (denote a) (denote b)
denote (Prefer a b          ) = Prefer (denote a) (denote b)
denote (RecordCompletion a b) = RecordCompletion (denote a) (denote b)
denote (Merge a b c         ) = Merge (denote a) (denote b) (fmap denote c)
denote (ToMap a b           ) = ToMap (denote a) (fmap denote b)
denote (Field a b           ) = Field (denote a) b
denote (Project a b         ) = Project (denote a) (fmap denote b)
denote (Assert a            ) = Assert (denote a)
denote (Equivalent a b      ) = Equivalent (denote a) (denote b)
denote (ImportAlt a b       ) = ImportAlt (denote a) (denote b)
denote (Embed a             ) = Embed a

-- | The \"opposite\" of `denote`, like @first absurd@ but faster
renote :: Expr Void a -> Expr s a
renote = unsafeCoerce
{-# INLINE renote #-}

{-| Remove any outermost `Note` constructors

    This is typically used when you want to get the outermost non-`Note`
    constructor without removing internal `Note` constructors
-}
shallowDenote :: Expr s a -> Expr s a
shallowDenote (Note _ e) = shallowDenote e
shallowDenote         e  = e

-- | The set of reserved identifiers for the Dhall language
reservedIdentifiers :: HashSet Text
reservedIdentifiers =
    Data.HashSet.fromList
        [ -- Keywords according to the `keyword` rule in the grammar
          "if"
        , "then"
        , "else"
        , "let"
        , "in"
        , "using"
        , "missing"
        , "as"
        , "Infinity"
        , "NaN"
        , "merge"
        , "Some"
        , "toMap"
        , "assert"
        , "forall"

          -- Builtins according to the `builtin` rule in the grammar
        , "Natural/fold"
        , "Natural/build"
        , "Natural/isZero"
        , "Natural/even"
        , "Natural/odd"
        , "Natural/toInteger"
        , "Natural/show"
        , "Natural/subtract"
        , "Integer"
        , "Integer/clamp"
        , "Integer/negate"
        , "Integer/show"
        , "Integer/toDouble"
        , "Integer/show"
        , "Natural/subtract"
        , "Double/show"
        , "List/build"
        , "List/fold"
        , "List/length"
        , "List/head"
        , "List/last"
        , "List/indexed"
        , "List/reverse"
        , "Optional/fold"
        , "Optional/build"
        , "Text/show"
        , "Bool"
        , "True"
        , "False"
        , "Optional"
        , "None"
        , "Natural"
        , "Integer"
        , "Double"
        , "Text"
        , "List"
        , "Type"
        , "Kind"
        , "Sort"
        ]
