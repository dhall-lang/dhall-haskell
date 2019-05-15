{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE UnicodeSyntax      #-}
{-# OPTIONS_GHC -Wall #-}

{-| This module contains the core calculus for the Dhall language.

    Dhall is essentially a fork of the @morte@ compiler but with more built-in
    functionality, better error messages, and Haskell integration
-}

module Dhall.Core (
    -- * Syntax
      Const(..)
    , Directory(..)
    , File(..)
    , FilePrefix(..)
    , Import(..)
    , ImportHashed(..)
    , ImportMode(..)
    , ImportType(..)
    , URL(..)
    , Scheme(..)
    , Var(..)
    , Binding(..)
    , Chunks(..)
    , Expr(..)

    -- * Normalization
    , Dhall.Core.alphaNormalize
    , normalize
    , normalizeWith
    , normalizeWithM
    , Normalizer
    , NormalizerM
    , ReifiedNormalizer (..)
    , judgmentallyEqual
    , subst
    , shift
    , isNormalized
    , isNormalizedWith
    , denote
    , freeIn

    -- * Pretty-printing
    , pretty

    -- * Miscellaneous
    , internalError
    , reservedIdentifiers
    , escapeText
    , subExpressions
    , chunkExprs
    , pathCharacter
    , throws
    ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative (Applicative(..), (<$>))
#endif
import Control.Applicative (empty)
import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO(..))
import Crypto.Hash (SHA256)
import Data.Bifunctor (Bifunctor(..))
import Data.Data (Data)
import Data.Foldable
import Data.Functor.Identity (Identity(..))
import Data.HashSet (HashSet)
import Data.List.NonEmpty (NonEmpty(..))
import Data.String (IsString(..))
import Data.Semigroup (Semigroup(..))
import Data.Sequence (Seq, ViewL(..), ViewR(..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty)
import Data.Traversable
import Dhall.Map (Map)
import Dhall.Set (Set)
import {-# SOURCE #-} Dhall.Pretty.Internal
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Prelude hiding (succ)

import qualified Control.Exception
import qualified Control.Monad
import qualified Crypto.Hash
import qualified Data.Char
import {-# SOURCE #-} qualified Dhall.Eval
import qualified Data.HashSet
import qualified Data.Sequence
import qualified Data.Text
import qualified Data.Text.Prettyprint.Doc  as Pretty
import qualified Dhall.Map
import qualified Dhall.Set
import qualified Network.URI.Encode         as URI.Encode
import qualified Text.Printf


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
data Const = Type | Kind | Sort deriving (Show, Eq, Data, Bounded, Enum, Generic)

instance Pretty Const where
    pretty = Pretty.unAnnotate . prettyConst

{-| Internal representation of a directory that stores the path components in
    reverse order

    In other words, the directory @\/foo\/bar\/baz@ is encoded as
    @Directory { components = [ "baz", "bar", "foo" ] }@
-}
newtype Directory = Directory { components :: [Text] }
    deriving (Eq, Generic, Ord, Show)

instance Semigroup Directory where
    Directory components₀ <> Directory components₁ =
        Directory (components₁ <> components₀)

instance Pretty Directory where
    pretty (Directory {..}) = foldMap prettyPathComponent (reverse components)

{-| A `File` is a `directory` followed by one additional path component
    representing the `file` name
-}
data File = File
    { directory :: Directory
    , file      :: Text
    } deriving (Eq, Generic, Ord, Show)

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
    deriving (Eq, Generic, Ord, Show)

instance Pretty FilePrefix where
    pretty Absolute = ""
    pretty Here     = "."
    pretty Parent   = ".."
    pretty Home     = "~"

data Scheme = HTTP | HTTPS deriving (Eq, Generic, Ord, Show)

data URL = URL
    { scheme    :: Scheme
    , authority :: Text
    , path      :: File
    , query     :: Maybe Text
    , headers   :: Maybe ImportHashed
    } deriving (Eq, Generic, Ord, Show)

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

-- | The type of import (i.e. local vs. remote vs. environment)
data ImportType
    = Local FilePrefix File
    -- ^ Local path
    | Remote URL
    -- ^ URL of remote resource and optional headers stored in an import
    | Env  Text
    -- ^ Environment variable
    | Missing
    deriving (Eq, Generic, Ord, Show)

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
        importHashed₀ = ImportHashed Nothing import₀

        headers₁ = fmap (importHashed₀ <>) headers₀

    _ <> import₁ =
        import₁

instance Pretty ImportType where
    pretty (Local prefix file) =
        Pretty.pretty prefix <> Pretty.pretty file

    pretty (Remote url) = Pretty.pretty url

    pretty (Env env) = "env:" <> Pretty.pretty env

    pretty Missing = "missing"

-- | How to interpret the import's contents (i.e. as Dhall code or raw text)
data ImportMode = Code | RawText deriving (Eq, Generic, Ord, Show)

-- | A `ImportType` extended with an optional hash for semantic integrity checks
data ImportHashed = ImportHashed
    { hash       :: Maybe (Crypto.Hash.Digest SHA256)
    , importType :: ImportType
    } deriving (Eq, Generic, Ord, Show)

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
    } deriving (Eq, Generic, Ord, Show)

instance Semigroup Import where
    Import importHashed₀ _ <> Import importHashed₁ code =
        Import (importHashed₀ <> importHashed₁) code

instance Pretty Import where
    pretty (Import {..}) = Pretty.pretty importHashed <> Pretty.pretty suffix
      where
        suffix :: Text
        suffix = case importMode of
            RawText -> " as Text"
            Code    -> ""

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
    deriving (Data, Generic, Eq, Show)

instance IsString Var where
    fromString str = V (fromString str) 0

instance Pretty Var where
    pretty = Pretty.unAnnotate . prettyVar

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
    -- | > Let [Binding x Nothing  r] e             ~  let x     = r in e
    --   > Let [Binding x (Just t) r] e             ~  let x : t = r in e
    | Let (NonEmpty (Binding s a)) (Expr s a)
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
    -- | > NaturalPlus x y                          ~  x + y
    | NaturalPlus (Expr s a) (Expr s a)
    -- | > NaturalTimes x y                         ~  x * y
    | NaturalTimes (Expr s a) (Expr s a)
    -- | > Integer                                  ~  Integer
    | Integer
    -- | > IntegerLit n                             ~  ±n
    | IntegerLit Integer
    -- | > IntegerShow                              ~  Integer/show
    | IntegerShow
    -- | > IntegerToDouble                          ~  Integer/toDouble
    | IntegerToDouble
    -- | > Double                                   ~  Double
    | Double
    -- | > DoubleLit n                              ~  n
    | DoubleLit Double
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
    -- | > ListLit (Just t ) [x, y, z]              ~  [x, y, z] : List t
    --   > ListLit  Nothing  [x, y, z]              ~  [x, y, z]
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
    -- | > OptionalLit t (Just e)                   ~  [e] : Optional t
    --   > OptionalLit t Nothing                    ~  []  : Optional t
    | OptionalLit (Expr s a) (Maybe (Expr s a))
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
    -- | > UnionLit k v [(k1, Just t1), (k2, Nothing)] ~  < k = v | k1 : t1 | k2 >
    | UnionLit Text (Expr s a) (Map Text (Maybe (Expr s a)))
    -- | > Combine x y                              ~  x ∧ y
    | Combine (Expr s a) (Expr s a)
    -- | > CombineTypes x y                         ~  x ⩓ y
    | CombineTypes (Expr s a) (Expr s a)
    -- | > Prefer x y                               ~  x ⫽ y
    | Prefer (Expr s a) (Expr s a)
    -- | > Merge x y (Just t )                      ~  merge x y : t
    --   > Merge x y  Nothing                       ~  merge x y
    | Merge (Expr s a) (Expr s a) (Maybe (Expr s a))
    -- | > Field e x                                ~  e.x
    | Field (Expr s a) Text
    -- | > Project e xs                             ~  e.{ xs }
    | Project (Expr s a) (Set Text)
    -- | > Note s x                                 ~  e
    | Note s (Expr s a)
    -- | > ImportAlt                                ~  e1 ? e2
    | ImportAlt (Expr s a) (Expr s a)
    -- | > Embed import                             ~  import
    | Embed a
    deriving (Eq, Foldable, Generic, Traversable, Show, Data)

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
  fmap f (Let as b) = Let (fmap (fmap f) as) (fmap f b)
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
  fmap f (NaturalPlus e1 e2) = NaturalPlus (fmap f e1) (fmap f e2)
  fmap f (NaturalTimes e1 e2) = NaturalTimes (fmap f e1) (fmap f e2)
  fmap _ Integer = Integer
  fmap _ (IntegerLit i) = IntegerLit i
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
  fmap f (OptionalLit e maybeE) = OptionalLit (fmap f e) (fmap (fmap f) maybeE)
  fmap f (Some e) = Some (fmap f e)
  fmap _ None = None
  fmap _ OptionalFold = OptionalFold
  fmap _ OptionalBuild = OptionalBuild
  fmap f (Record r) = Record (fmap (fmap f) r)
  fmap f (RecordLit r) = RecordLit (fmap (fmap f) r)
  fmap f (Union u) = Union (fmap (fmap (fmap f)) u)
  fmap f (UnionLit v e u) = UnionLit v (fmap f e) (fmap (fmap (fmap f)) u)
  fmap f (Combine e1 e2) = Combine (fmap f e1) (fmap f e2)
  fmap f (CombineTypes e1 e2) = CombineTypes (fmap f e1) (fmap f e2)
  fmap f (Prefer e1 e2) = Prefer (fmap f e1) (fmap f e2)
  fmap f (Merge e1 e2 maybeE) = Merge (fmap f e1) (fmap f e2) (fmap (fmap f) maybeE)
  fmap f (Field e1 v) = Field (fmap f e1) v
  fmap f (Project e1 vs) = Project (fmap f e1) vs
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
    Let as b             >>= k = Let (fmap f as) (b >>= k)
      where
        f (Binding c d e) = Binding c (fmap (>>= k) d) (e >>= k)
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
    OptionalLit a b      >>= k = OptionalLit (a >>= k) (fmap (>>= k) b)
    Some a               >>= k = Some (a >>= k)
    None                 >>= _ = None
    OptionalFold         >>= _ = OptionalFold
    OptionalBuild        >>= _ = OptionalBuild
    Record    a          >>= k = Record (fmap (>>= k) a)
    RecordLit a          >>= k = RecordLit (fmap (>>= k) a)
    Union     a          >>= k = Union (fmap (fmap (>>= k)) a)
    UnionLit a b c       >>= k = UnionLit a (b >>= k) (fmap (fmap (>>= k)) c)
    Combine a b          >>= k = Combine (a >>= k) (b >>= k)
    CombineTypes a b     >>= k = CombineTypes (a >>= k) (b >>= k)
    Prefer a b           >>= k = Prefer (a >>= k) (b >>= k)
    Merge a b c          >>= k = Merge (a >>= k) (b >>= k) (fmap (>>= k) c)
    Field a b            >>= k = Field (a >>= k) b
    Project a b          >>= k = Project (a >>= k) b
    Note a b             >>= k = Note a (b >>= k)
    ImportAlt a b        >>= k = ImportAlt (a >>= k) (b >>= k)
    Embed a              >>= k = k a

instance Bifunctor Expr where
    first _ (Const a             ) = Const a
    first _ (Var a               ) = Var a
    first k (Lam a b c           ) = Lam a (first k b) (first k c)
    first k (Pi a b c            ) = Pi a (first k b) (first k c)
    first k (App a b             ) = App (first k a) (first k b)
    first k (Let as b            ) = Let (fmap (first k) as) (first k b)
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
    first k (OptionalLit a b     ) = OptionalLit (first k a) (fmap (first k) b)
    first k (Some a              ) = Some (first k a)
    first _  None                  = None
    first _  OptionalFold          = OptionalFold
    first _  OptionalBuild         = OptionalBuild
    first k (Record a            ) = Record (fmap (first k) a)
    first k (RecordLit a         ) = RecordLit (fmap (first k) a)
    first k (Union a             ) = Union (fmap (fmap (first k)) a)
    first k (UnionLit a b c      ) = UnionLit a (first k b) (fmap (fmap (first k)) c)
    first k (Combine a b         ) = Combine (first k a) (first k b)
    first k (CombineTypes a b    ) = CombineTypes (first k a) (first k b)
    first k (Prefer a b          ) = Prefer (first k a) (first k b)
    first k (Merge a b c         ) = Merge (first k a) (first k b) (fmap (first k) c)
    first k (Field a b           ) = Field (first k a) b
    first k (Project a b         ) = Project (first k a) b
    first k (Note a b            ) = Note (k a) (first k b)
    first k (ImportAlt a b       ) = ImportAlt (first k a) (first k b)
    first _ (Embed a             ) = Embed a

    second = fmap

instance IsString (Expr s a) where
    fromString str = Var (fromString str)

data Binding s a = Binding
    { variable   :: Text
    , annotation :: Maybe (Expr s a)
    , value      :: Expr s a
    } deriving (Functor, Foldable, Generic, Traversable, Show, Eq, Data)

instance Bifunctor Binding where
    first k (Binding a b c) = Binding a (fmap (first k) b) (first k c)

    second = fmap

-- | The body of an interpolated @Text@ literal
data Chunks s a = Chunks [(Text, Expr s a)] Text
    deriving (Functor, Foldable, Generic, Traversable, Show, Eq, Data)

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
shift d (V x₀ n₀) (Let (Binding x₁ mA₀ a₀ :| []) b₀) =
    Let (Binding x₁ mA₁ a₁ :| []) b₁
  where
    n₁ = if x₀ == x₁ then n₀ + 1 else n₀

    mA₁ = fmap (shift d (V x₀ n₀)) mA₀
    a₁  =       shift d (V x₀ n₀)   a₀

    b₁  =       shift d (V x₀ n₁)   b₀
shift d (V x₀ n₀) (Let (Binding x₁ mA₀ a₀ :| (l₀ : ls₀)) b₀) =
    case shift d (V x₀ n₁) (Let (l₀ :| ls₀) b₀) of
        Let (l₁ :| ls₁) b₁ -> Let (Binding x₁ mA₁ a₁ :| (l₁ : ls₁)) b₁
        e                  -> Let (Binding x₁ mA₁ a₁ :|       []  ) e
  where
    n₁ = if x₀ == x₁ then n₀ + 1 else n₀

    mA₁ = fmap (shift d (V x₀ n₀)) mA₀
    a₁  =       shift d (V x₀ n₀)   a₀

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
shift _ _ IntegerToDouble = IntegerToDouble
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
shift _ _ TextShow = TextShow
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
shift d v (Some a) = Some a'
  where
    a' = shift d v a
shift _ _ None = None
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
    a' = fmap (fmap (shift d v)) a
shift d v (UnionLit a b c) = UnionLit a b' c'
  where
    b' =             shift d v   b
    c' = fmap (fmap (shift d v)) c
shift d v (Combine a b) = Combine a' b'
  where
    a' = shift d v a
    b' = shift d v b
shift d v (CombineTypes a b) = CombineTypes a' b'
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
shift d v (Field a b) = Field a' b
  where
    a' = shift d v a
shift d v (Project a b) = Project a' b
  where
    a' = shift d v a
shift d v (Note a b) = Note a b'
  where
    b' = shift d v b
shift d v (ImportAlt a b) = ImportAlt a' b'
  where
    a' = shift d v a
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
subst (V x₀ n₀) e₀ (Let (Binding x₁ mA₀ a₀ :| []) b₀) =
    Let (Binding x₁ mA₁ a₁ :| []) b₁
  where
    n₁ = if x₀ == x₁ then n₀ + 1 else n₀

    e₁ = shift 1 (V x₁ 0) e₀

    mA₁ = fmap (subst (V x₀ n₀) e₀) mA₀
    a₁  =       subst (V x₀ n₀) e₀   a₀

    b₁  =       subst (V x₀ n₁) e₁   b₀
subst (V x₀ n₀) e₀ (Let (Binding x₁ mA₀ a₀ :| (l₀ : ls₀)) b₀) =
    case subst (V x₀ n₁) e₁ (Let (l₀ :| ls₀) b₀) of
        Let (l₁ :| ls₁) b₁ -> Let (Binding x₁ mA₁ a₁ :| (l₁ : ls₁)) b₁
        e                  -> Let (Binding x₁ mA₁ a₁ :|       []  ) e
  where
    n₁ = if x₀ == x₁ then n₀ + 1 else n₀

    e₁ = shift 1 (V x₁ 0) e₀

    mA₁ = fmap (subst (V x₀ n₀) e₀) mA₀
    a₁  =       subst (V x₀ n₀) e₀   a₀

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
subst _ _ IntegerToDouble = IntegerToDouble
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
subst _ _ TextShow = TextShow
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
subst x e (Some a) = Some a'
  where
    a' = subst x e a
subst _ _ None = None
subst _ _ OptionalFold = OptionalFold
subst _ _ OptionalBuild = OptionalBuild
subst x e (Record kts) = Record kts'
  where
    kts' = fmap (subst x e) kts
subst x e (RecordLit kvs) = RecordLit kvs'
  where
    kvs' = fmap (subst x e) kvs
subst x e (Union kts) = Union kts'
  where
    kts' = fmap (fmap (subst x e)) kts
subst x e (UnionLit a b kts) = UnionLit a b' kts'
  where
    b'   =             subst x e   b
    kts' = fmap (fmap (subst x e)) kts
subst x e (Combine a b) = Combine a' b'
  where
    a' = subst x e a
    b' = subst x e b
subst x e (CombineTypes a b) = CombineTypes a' b'
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
subst x e (Field a b) = Field a' b
  where
    a' = subst x e a
subst x e (Project a b) = Project a' b
  where
    a' = subst x e a
subst x e (Note a b) = Note a b'
  where
    b' = subst x e b
subst x e (ImportAlt a b) = ImportAlt a' b'
  where
    a' = subst x e a
    b' = subst x e b
-- The Dhall compiler enforces that all embedded values are closed expressions
-- and `subst` does nothing to a closed expression
subst _ _ (Embed p) = Embed p

{-| α-normalize an expression by renaming all bound variables to @\"_\"@ and
    using De Bruijn indices to distinguish them

>>> alphaNormalize (Lam "a" (Const Type) (Lam "b" (Const Type) (Lam "x" "a" (Lam "y" "b" "x"))))
Lam "_" (Const Type) (Lam "_" (Const Type) (Lam "_" (Var (V "_" 1)) (Lam "_" (Var (V "_" 1)) (Var (V "_" 1)))))

    α-normalization does not affect free variables:

>>> alphaNormalize "x"
Var (V "x" 0)

-}
alphaNormalize :: Expr s a -> Expr s a
alphaNormalize = Dhall.Eval.alphaNormalize

{-| Reduce an expression to its normal form, performing beta reduction

    `normalize` does not type-check the expression.  You may want to type-check
    expressions before normalizing them since normalization can convert an
    ill-typed expression into a well-typed expression.

    However, `normalize` will not fail if the expression is ill-typed and will
    leave ill-typed sub-expressions unevaluated.
-}

normalize :: Eq a => Expr s a -> Expr t a
normalize = Dhall.Eval.nfEmpty


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
boundedType (Union kvs)      = all (all boundedType) kvs
boundedType _                = False

-- | Remove all `Note` constructors from an `Expr` (i.e. de-`Note`)
denote :: Expr s a -> Expr t a
denote (Note _ b            ) = denote b
denote (Const a             ) = Const a
denote (Var a               ) = Var a
denote (Lam a b c           ) = Lam a (denote b) (denote c)
denote (Pi a b c            ) = Pi a (denote b) (denote c)
denote (App a b             ) = App (denote a) (denote b)
denote (Let as b            ) = Let (fmap f as) (denote b)
  where
    f (Binding c d e) = Binding c (fmap denote d) (denote e)
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
denote (OptionalLit a b     ) = OptionalLit (denote a) (fmap denote b)
denote (Some a              ) = Some (denote a)
denote  None                  = None
denote  OptionalFold          = OptionalFold
denote  OptionalBuild         = OptionalBuild
denote (Record a            ) = Record (fmap denote a)
denote (RecordLit a         ) = RecordLit (fmap denote a)
denote (Union a             ) = Union (fmap (fmap denote) a)
denote (UnionLit a b c      ) = UnionLit a (denote b) (fmap (fmap denote) c)
denote (Combine a b         ) = Combine (denote a) (denote b)
denote (CombineTypes a b    ) = CombineTypes (denote a) (denote b)
denote (Prefer a b          ) = Prefer (denote a) (denote b)
denote (Merge a b c         ) = Merge (denote a) (denote b) (fmap denote c)
denote (Field a b           ) = Field (denote a) b
denote (Project a b         ) = Project (denote a) b
denote (ImportAlt a b       ) = ImportAlt (denote a) (denote b)
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
normalizeWith :: Eq a => Maybe (ReifiedNormalizer a) -> Expr s a -> Expr t a
normalizeWith (Just ctx) t = runIdentity (normalizeWithM (getReifiedNormalizer ctx) t)
normalizeWith _          t = Dhall.Eval.nfEmpty t

normalizeWithM
    :: (Monad m, Eq a) => NormalizerM m a -> Expr s a -> m (Expr t a)
normalizeWithM ctx e0 = loop (denote e0)
 where
 loop e =  case e of
    Const k -> pure (Const k)
    Var v -> pure (Var v)
    Lam x _A b -> Lam x <$> _A' <*> b'
      where
        _A' = loop _A
        b'  = loop b
    Pi x _A _B -> Pi x <$> _A' <*> _B'
      where
        _A' = loop _A
        _B' = loop _B
    App f a -> do
      res <- ctx (App f a)
      case res of
          Just e1 -> loop e1
          Nothing -> do
              f' <- loop f
              a' <- loop a
              case f' of
                Lam x _A b₀ -> do

                    let a₂ = shift 1 (V x 0) a'
                    let b₁ = subst (V x 0) a₂ b₀
                    let b₂ = shift (-1) (V x 0) b₁

                    loop b₂
                _ -> do
                  case App f' a' of
                    -- build/fold fusion for `List`
                    App (App ListBuild _) (App (App ListFold _) e') -> loop e'

                    -- build/fold fusion for `Natural`
                    App NaturalBuild (App NaturalFold e') -> loop e'

                    -- build/fold fusion for `Optional`
                    App (App OptionalBuild _) (App (App OptionalFold _) e') -> loop e'

                    App (App (App (App NaturalFold (NaturalLit n0)) t) succ') zero -> do
                      t' <- loop t
                      if boundedType t' then strict else lazy
                      where
                        strict =       strictLoop (fromIntegral n0 :: Integer)
                        lazy   = loop (  lazyLoop (fromIntegral n0 :: Integer))

                        strictLoop !0 = loop zero
                        strictLoop !n = App succ' <$> strictLoop (n - 1) >>= loop

                        lazyLoop !0 = zero
                        lazyLoop !n = App succ' (lazyLoop (n - 1))
                    App NaturalBuild g -> loop (App (App (App g Natural) succ) zero)
                      where
                        succ = Lam "x" Natural (NaturalPlus "x" (NaturalLit 1))

                        zero = NaturalLit 0
                    App NaturalIsZero (NaturalLit n) -> pure (BoolLit (n == 0))
                    App NaturalEven (NaturalLit n) -> pure (BoolLit (even n))
                    App NaturalOdd (NaturalLit n) -> pure (BoolLit (odd n))
                    App NaturalToInteger (NaturalLit n) -> pure (IntegerLit (toInteger n))
                    App NaturalShow (NaturalLit n) ->
                        pure (TextLit (Chunks [] (Data.Text.pack (show n))))
                    App IntegerShow (IntegerLit n)
                        | 0 <= n    -> pure (TextLit (Chunks [] ("+" <> Data.Text.pack (show n))))
                        | otherwise -> pure (TextLit (Chunks [] (Data.Text.pack (show n))))
                    -- `(read . show)` is used instead of `fromInteger` because `read` uses the correct rounding rule
                    App IntegerToDouble (IntegerLit n) -> pure (DoubleLit ((read . show) n))
                    App DoubleShow (DoubleLit n) ->
                        pure (TextLit (Chunks [] (Data.Text.pack (show n))))
                    App (App OptionalBuild _A₀) g ->
                        loop (App (App (App g optional) just) nothing)
                      where
                        optional = App Optional _A₀

                        just = Lam "a" _A₀ (Some "a")

                        nothing = App None _A₀
                    App (App ListBuild _A₀) g -> loop (App (App (App g list) cons) nil)
                      where
                        _A₁ = shift 1 "a" _A₀

                        list = App List _A₀

                        cons =
                            Lam "a" _A₀
                                (Lam "as"
                                    (App List _A₁)
                                    (ListAppend (ListLit Nothing (pure "a")) "as")
                                )

                        nil = ListLit (Just _A₀) empty
                    App (App (App (App (App ListFold _) (ListLit _ xs)) t) cons) nil -> do
                      t' <- loop t
                      if boundedType t' then strict else lazy
                      where
                        strict =       foldr strictCons strictNil xs
                        lazy   = loop (foldr   lazyCons   lazyNil xs)

                        strictNil = loop nil
                        lazyNil   =      nil

                        strictCons y ys = do
                          App (App cons y) <$> ys >>= loop
                        lazyCons   y ys =       App (App cons y) ys
                    App (App ListLength _) (ListLit _ ys) ->
                        pure (NaturalLit (fromIntegral (Data.Sequence.length ys)))
                    App (App ListHead t) (ListLit _ ys) -> loop o
                      where
                        o = case Data.Sequence.viewl ys of
                                y :< _ -> Some y
                                _      -> App None t
                    App (App ListLast t) (ListLit _ ys) -> loop o
                      where
                        o = case Data.Sequence.viewr ys of
                                _ :> y -> Some y
                                _      -> App None t
                    App (App ListIndexed _A₀) (ListLit _A₁ as₀) -> loop (ListLit t as₁)
                      where
                        as₁ = Data.Sequence.mapWithIndex adapt as₀

                        _A₂ = Record (Dhall.Map.fromList kts)
                          where
                            kts = [ ("index", Natural)
                                  , ("value", _A₀)
                                  ]

                        t | null as₀  = Just _A₂
                          | otherwise = Nothing

                        adapt n a_ =
                            RecordLit (Dhall.Map.fromList kvs)
                          where
                            kvs = [ ("index", NaturalLit (fromIntegral n))
                                  , ("value", a_)
                                  ]
                    App (App ListReverse t) (ListLit _ xs) ->
                        loop (ListLit m (Data.Sequence.reverse xs))
                      where
                        m = if Data.Sequence.null xs then Just t else Nothing
                    App (App (App (App (App OptionalFold _) (App None _)) _) _) nothing ->
                        loop nothing
                    App (App (App (App (App OptionalFold _) (Some x)) _) just) _ ->
                        loop (App just x)
                    App TextShow (TextLit (Chunks [] oldText)) ->
                        loop (TextLit (Chunks [] newText))
                      where
                        newText = textShow oldText
                    _ -> do
                        res2 <- ctx (App f' a')
                        case res2 of
                            Nothing -> pure (App f' a')
                            Just app' -> loop app'
    Let (Binding x _ a₀ :| ls₀) b₀ -> do
        a₁ <- loop a₀

        rest <- case ls₀ of
                []       -> loop b₀
                l₁ : ls₁ -> loop (Let (l₁ :| ls₁) b₀)

        let a₂ = shift 1 (V x 0) a₁
        let b₁ = subst (V x 0) a₂ rest
        let b₂ = shift (-1) (V x 0) b₁

        loop b₂
    Annot x _ -> loop x
    Bool -> pure Bool
    BoolLit b -> pure (BoolLit b)
    BoolAnd x y -> decide <$> loop x <*> loop y
      where
        decide (BoolLit True )  r              = r
        decide (BoolLit False)  _              = BoolLit False
        decide  l              (BoolLit True ) = l
        decide  _              (BoolLit False) = BoolLit False
        decide  l               r
            | judgmentallyEqual l r = l
            | otherwise             = BoolAnd l r
    BoolOr x y -> decide <$> loop x <*> loop y
      where
        decide (BoolLit False)  r              = r
        decide (BoolLit True )  _              = BoolLit True
        decide  l              (BoolLit False) = l
        decide  _              (BoolLit True ) = BoolLit True
        decide  l               r
            | judgmentallyEqual l r = l
            | otherwise             = BoolOr l r
    BoolEQ x y -> decide <$> loop x <*> loop y
      where
        decide (BoolLit True )  r              = r
        decide  l              (BoolLit True ) = l
        decide  l               r
            | judgmentallyEqual l r = BoolLit True
            | otherwise             = BoolEQ l r
    BoolNE x y -> decide <$> loop x <*> loop y
      where
        decide (BoolLit False)  r              = r
        decide  l              (BoolLit False) = l
        decide  l               r
            | judgmentallyEqual l r = BoolLit False
            | otherwise             = BoolNE l r
    BoolIf bool true false -> decide <$> loop bool <*> loop true <*> loop false
      where
        decide (BoolLit True )  l              _              = l
        decide (BoolLit False)  _              r              = r
        decide  b              (BoolLit True) (BoolLit False) = b
        decide  b               l              r
            | judgmentallyEqual l r = l
            | otherwise             = BoolIf b l r
    Natural -> pure Natural
    NaturalLit n -> pure (NaturalLit n)
    NaturalFold -> pure NaturalFold
    NaturalBuild -> pure NaturalBuild
    NaturalIsZero -> pure NaturalIsZero
    NaturalEven -> pure NaturalEven
    NaturalOdd -> pure NaturalOdd
    NaturalToInteger -> pure NaturalToInteger
    NaturalShow -> pure NaturalShow
    NaturalPlus x y -> decide <$> loop x <*> loop y
      where
        decide (NaturalLit 0)  r             = r
        decide  l             (NaturalLit 0) = l
        decide (NaturalLit m) (NaturalLit n) = NaturalLit (m + n)
        decide  l              r             = NaturalPlus l r
    NaturalTimes x y -> decide <$> loop x <*> loop y
      where
        decide (NaturalLit 1)  r             = r
        decide  l             (NaturalLit 1) = l
        decide (NaturalLit 0)  _             = NaturalLit 0
        decide  _             (NaturalLit 0) = NaturalLit 0
        decide (NaturalLit m) (NaturalLit n) = NaturalLit (m * n)
        decide  l              r             = NaturalTimes l r
    Integer -> pure Integer
    IntegerLit n -> pure (IntegerLit n)
    IntegerShow -> pure IntegerShow
    IntegerToDouble -> pure IntegerToDouble
    Double -> pure Double
    DoubleLit n -> pure (DoubleLit n)
    DoubleShow -> pure DoubleShow
    Text -> pure Text
    TextLit (Chunks xys z) -> do
        chunks' <- mconcat <$> chunks
        case chunks' of
            Chunks [("", x)] "" -> pure x
            c                   -> pure (TextLit c)
      where
        chunks =
          ((++ [Chunks [] z]) . concat) <$> traverse process xys

        process (x, y) = do
          y' <- loop y
          case y' of
            TextLit c -> pure [Chunks [] x, c]
            _         -> pure [Chunks [(x, y')] mempty]
    TextAppend x y -> loop (TextLit (Chunks [("", x), ("", y)] ""))
    TextShow -> pure TextShow
    List -> pure List
    ListLit t es
        | Data.Sequence.null es -> ListLit <$> t' <*> es'
        | otherwise             -> ListLit Nothing <$> es'
      where
        t'  = traverse loop t
        es' = traverse loop es
    ListAppend x y -> decide <$> loop x <*> loop y
      where
        decide (ListLit _ m)  r            | Data.Sequence.null m = r
        decide  l            (ListLit _ n) | Data.Sequence.null n = l
        decide (ListLit t m) (ListLit _ n)                        = ListLit t (m <> n)
        decide  l             r                                   = ListAppend l r
    ListBuild -> pure ListBuild
    ListFold -> pure ListFold
    ListLength -> pure ListLength
    ListHead -> pure ListHead
    ListLast -> pure ListLast
    ListIndexed -> pure ListIndexed
    ListReverse -> pure ListReverse
    Optional -> pure Optional
    OptionalLit _A Nothing -> loop (App None _A)
    OptionalLit _ (Just a) -> loop (Some a)
    Some a -> Some <$> a'
      where
        a' = loop a
    None -> pure None
    OptionalFold -> pure OptionalFold
    OptionalBuild -> pure OptionalBuild
    Record kts -> Record . Dhall.Map.sort <$> kts'
      where
        kts' = traverse loop kts
    RecordLit kvs -> RecordLit . Dhall.Map.sort <$> kvs'
      where
        kvs' = traverse loop kvs
    Union kts -> Union . Dhall.Map.sort <$> kts'
      where
        kts' = traverse (traverse loop) kts
    UnionLit k v kvs -> UnionLit k <$> v' <*> (Dhall.Map.sort <$> kvs')
      where
        v'   =                    loop  v
        kvs' = traverse (traverse loop) kvs
    Combine x y -> decide <$> loop x <*> loop y
      where
        decide (RecordLit m) r | Data.Foldable.null m =
            r
        decide l (RecordLit n) | Data.Foldable.null n =
            l
        decide (RecordLit m) (RecordLit n) =
            RecordLit (Dhall.Map.sort (Dhall.Map.unionWith decide m n))
        decide l r =
            Combine l r
    CombineTypes x y -> decide <$> loop x <*> loop y
      where
        decide (Record m) r | Data.Foldable.null m =
            r
        decide l (Record n) | Data.Foldable.null n =
            l
        decide (Record m) (Record n) =
            Record (Dhall.Map.sort (Dhall.Map.unionWith decide m n))
        decide l r =
            CombineTypes l r
    Prefer x y -> decide <$> loop x <*> loop y
      where
        decide (RecordLit m) r | Data.Foldable.null m =
            r
        decide l (RecordLit n) | Data.Foldable.null n =
            l
        decide (RecordLit m) (RecordLit n) =
            RecordLit (Dhall.Map.sort (Dhall.Map.union n m))
        decide l r =
            Prefer l r
    Merge x y t      -> do
        x' <- loop x
        y' <- loop y
        case x' of
            RecordLit kvsX ->
                case y' of
                    UnionLit kY vY _ ->
                        case Dhall.Map.lookup kY kvsX of
                            Just vX -> loop (App vX vY)
                            Nothing -> Merge x' y' <$> t'
                    Field (Union ktsY) kY ->
                        case Dhall.Map.lookup kY ktsY of
                            Just Nothing ->
                                case Dhall.Map.lookup kY kvsX of
                                    Just vX -> return vX
                                    Nothing -> Merge x' y' <$> t'
                            _ ->
                                Merge x' y' <$> t'
                    App (Field (Union ktsY) kY) vY ->
                        case Dhall.Map.lookup kY ktsY of
                            Just (Just _) ->
                                case Dhall.Map.lookup kY kvsX of
                                    Just vX -> loop (App vX vY)
                                    Nothing -> Merge x' y' <$> t'
                            _ ->
                                Merge x' y' <$> t'
                    _ -> Merge x' y' <$> t'
            _ -> Merge x' y' <$> t'
      where
        t' = traverse loop t
    Field r x        -> do
        r' <- loop r
        case r' of
            RecordLit kvs ->
                case Dhall.Map.lookup x kvs of
                    Just v  -> loop v
                    Nothing -> Field <$> (RecordLit <$> traverse loop kvs) <*> pure x
            _ -> pure (Field r' x)
    Project r xs     -> do
        r' <- loop r
        case r' of
            RecordLit kvs ->
                case traverse adapt (Dhall.Set.toList xs) of
                    Just s  ->
                        loop (RecordLit kvs')
                      where
                        kvs' = Dhall.Map.fromList s
                    Nothing ->
                        Project <$> (RecordLit <$> traverse loop kvs) <*> pure xs
              where
                adapt x = do
                    v <- Dhall.Map.lookup x kvs
                    return (x, v)
            _   | null xs -> pure (RecordLit mempty)
                | otherwise -> pure (Project r' xs)
    Note _ e' -> loop e'
    ImportAlt l _r -> loop l
    Embed a -> pure (Embed a)

textShow :: Text -> Text
textShow text = "\"" <> Data.Text.concatMap f text <> "\""
  where
    f '"'  = "\\\""
    f '$'  = "\\u0024"
    f '\\' = "\\\\"
    f '\b' = "\\b"
    f '\n' = "\\n"
    f '\r' = "\\r"
    f '\t' = "\\t"
    f '\f' = "\\f"
    f c | c <= '\x1F' = Data.Text.pack (Text.Printf.printf "\\u%04x" (Data.Char.ord c))
        | otherwise   = Data.Text.singleton c

{-| Returns `True` if two expressions are α-equivalent and β-equivalent and
    `False` otherwise
-}
judgmentallyEqual :: Eq a => Expr s a -> Expr t a -> Bool
judgmentallyEqual = Dhall.Eval.convEmpty

-- | Use this to wrap you embedded functions (see `normalizeWith`) to make them
--   polymorphic enough to be used.
type NormalizerM m a = forall s. Expr s a -> m (Maybe (Expr s a))

type Normalizer a = NormalizerM Identity a

-- | A reified 'Normalizer', which can be stored in structures without
-- running into impredicative polymorphism.
newtype ReifiedNormalizer a = ReifiedNormalizer
  { getReifiedNormalizer :: Normalizer a }

-- | Check if an expression is in a normal form given a context of evaluation.
--   Unlike `isNormalized`, this will fully normalize and traverse through the expression.
--
--   It is much more efficient to use `isNormalized`.
isNormalizedWith :: (Eq s, Eq a) => Normalizer a -> Expr s a -> Bool
isNormalizedWith ctx e = e == normalizeWith (Just (ReifiedNormalizer ctx)) e

-- | Quickly check if an expression is in normal form
isNormalized :: Eq a => Expr s a -> Bool
isNormalized e0 = loop (denote e0)
  where
    loop e = case e of
      Const _ -> True
      Var _ -> True
      Lam _ a b -> loop a && loop b
      Pi _ a b -> loop a && loop b
      App f a -> loop f && loop a && case App f a of
          App (Lam _ _ _) _ -> False

          -- build/fold fusion for `List`
          App (App ListBuild _) (App (App ListFold _) _) -> False

          -- build/fold fusion for `Natural`
          App NaturalBuild (App NaturalFold _) -> False

          -- build/fold fusion for `Optional`
          App (App OptionalBuild _) (App (App OptionalFold _) _) -> False

          App (App (App (App NaturalFold (NaturalLit _)) _) _) _ -> False
          App NaturalBuild _ -> False
          App NaturalIsZero (NaturalLit _) -> False
          App NaturalEven (NaturalLit _) -> False
          App NaturalOdd (NaturalLit _) -> False
          App NaturalShow (NaturalLit _) -> False
          App NaturalToInteger (NaturalLit _) -> False
          App IntegerShow (IntegerLit _) -> False
          App IntegerToDouble (IntegerLit _) -> False
          App DoubleShow (DoubleLit _) -> False
          App (App OptionalBuild _) _ -> False
          App (App ListBuild _) _ -> False
          App (App (App (App (App ListFold _) (ListLit _ _)) _) _) _ ->
              False
          App (App ListLength _) (ListLit _ _) -> False
          App (App ListHead _) (ListLit _ _) -> False
          App (App ListLast _) (ListLit _ _) -> False
          App (App ListIndexed _) (ListLit _ _) -> False
          App (App ListReverse _) (ListLit _ _) -> False
          App (App (App (App (App OptionalFold _) (Some _)) _) _) _ ->
              False
          App (App (App (App (App OptionalFold _) (App None _)) _) _) _ ->
              False
          App TextShow (TextLit (Chunks [] _)) ->
              False
          _ -> True
      Let _ _ -> False
      Annot _ _ -> False
      Bool -> True
      BoolLit _ -> True
      BoolAnd x y -> loop x && loop y && decide x y
        where
          decide (BoolLit _)  _          = False
          decide  _          (BoolLit _) = False
          decide  l           r          = not (judgmentallyEqual l r)
      BoolOr x y -> loop x && loop y && decide x y
        where
          decide (BoolLit _)  _          = False
          decide  _          (BoolLit _) = False
          decide  l           r          = not (judgmentallyEqual l r)
      BoolEQ x y -> loop x && loop y && decide x y
        where
          decide (BoolLit True)  _             = False
          decide  _             (BoolLit True) = False
          decide  l              r             = not (judgmentallyEqual l r)
      BoolNE x y -> loop x && loop y && decide x y
        where
          decide (BoolLit False)  _               = False
          decide  _              (BoolLit False ) = False
          decide  l               r               = not (judgmentallyEqual l r)
      BoolIf x y z ->
          loop x && loop y && loop z && decide x y z
        where
          decide (BoolLit _)  _              _              = False
          decide  _          (BoolLit True) (BoolLit False) = False
          decide  _           l              r              = not (judgmentallyEqual l r)
      Natural -> True
      NaturalLit _ -> True
      NaturalFold -> True
      NaturalBuild -> True
      NaturalIsZero -> True
      NaturalEven -> True
      NaturalOdd -> True
      NaturalShow -> True
      NaturalToInteger -> True
      NaturalPlus x y -> loop x && loop y && decide x y
        where
          decide (NaturalLit 0)  _             = False
          decide  _             (NaturalLit 0) = False
          decide (NaturalLit _) (NaturalLit _) = False
          decide  _              _             = True
      NaturalTimes x y -> loop x && loop y && decide x y
        where
          decide (NaturalLit 0)  _             = False
          decide  _             (NaturalLit 0) = False
          decide (NaturalLit 1)  _             = False
          decide  _             (NaturalLit 1) = False
          decide (NaturalLit _) (NaturalLit _) = False
          decide  _              _             = True
      Integer -> True
      IntegerLit _ -> True
      IntegerShow -> True
      IntegerToDouble -> True
      Double -> True
      DoubleLit _ -> True
      DoubleShow -> True
      Text -> True
      TextLit (Chunks [("", _)] "") -> False
      TextLit (Chunks xys _) -> all (all check) xys
        where
          check y = loop y && case y of
              TextLit _ -> False
              _         -> True
      TextAppend _ _ -> False
      TextShow -> True
      List -> True
      ListLit t es -> all loop t && all loop es
      ListAppend x y -> loop x && loop y && decide x y
        where
          decide (ListLit _ m)  _            | Data.Sequence.null m = False
          decide  _            (ListLit _ n) | Data.Sequence.null n = False
          decide (ListLit _ _) (ListLit _ _)                        = False
          decide  _             _                                   = True
      ListBuild -> True
      ListFold -> True
      ListLength -> True
      ListHead -> True
      ListLast -> True
      ListIndexed -> True
      ListReverse -> True
      Optional -> True
      OptionalLit _ _ -> False
      Some a -> loop a
      None -> True
      OptionalFold -> True
      OptionalBuild -> True
      Record kts -> Dhall.Map.isSorted kts && all loop kts
      RecordLit kvs -> Dhall.Map.isSorted kvs && all loop kvs
      Union kts -> Dhall.Map.isSorted kts && all (all loop) kts
      UnionLit _ v kvs -> loop v && Dhall.Map.isSorted kvs && all (all loop) kvs
      Combine x y -> loop x && loop y && decide x y
        where
          decide (RecordLit m) _ | Data.Foldable.null m = False
          decide _ (RecordLit n) | Data.Foldable.null n = False
          decide (RecordLit _) (RecordLit _) = False
          decide  _ _ = True
      CombineTypes x y -> loop x && loop y && decide x y
        where
          decide (Record m) _ | Data.Foldable.null m = False
          decide _ (Record n) | Data.Foldable.null n = False
          decide (Record _) (Record _) = False
          decide  _ _ = True
      Prefer x y -> loop x && loop y && decide x y
        where
          decide (RecordLit m) _ | Data.Foldable.null m = False
          decide _ (RecordLit n) | Data.Foldable.null n = False
          decide (RecordLit _) (RecordLit _) = False
          decide  _ _ = True
      Merge x y t -> loop x && loop y && all loop t &&
          case x of
              RecordLit kvsX ->
                  case y of
                      UnionLit kY _  _ ->
                          case Dhall.Map.lookup kY kvsX of
                              Just _  -> False
                              Nothing -> True
                      _ -> True
              _ -> True
      Field r x -> loop r &&
          case r of
              RecordLit kvs ->
                  case Dhall.Map.lookup x kvs of
                      Just _  -> False
                      Nothing -> True
              Union kvs ->
                  case Dhall.Map.lookup x kvs of
                      Just _  -> False
                      Nothing -> True
              _ -> True
      Project r xs -> loop r &&
          case r of
              RecordLit kvs ->
                  if all (flip Dhall.Map.member kvs) xs
                      then False
                      else True
              _ -> not (null xs)
      Note _ e' -> loop e'
      ImportAlt l _r -> loop l
      Embed _ -> True

{-| Detect if the given variable is free within the given expression

>>> "x" `freeIn` "x"
True
>>> "x" `freeIn` "y"
False
>>> "x" `freeIn` Lam "x" (Const Type) "x"
False
-}
freeIn :: Eq a => Var -> Expr s a -> Bool
variable `freeIn` expression =
    Dhall.Core.shift 1 variable strippedExpression /= strippedExpression
  where
    denote' :: Expr t b -> Expr () b
    denote' = denote

    strippedExpression = denote' expression

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

-- | The set of reserved identifiers for the Dhall language
reservedIdentifiers :: HashSet Text
reservedIdentifiers =
    Data.HashSet.fromList
        [ "let"
        , "in"
        , "Type"
        , "Kind"
        , "Sort"
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
        , "Integer/toDouble"
        , "Double"
        , "Double/show"
        , "Text"
        , "Text/show"
        , "List"
        , "List/build"
        , "List/fold"
        , "List/length"
        , "List/head"
        , "List/last"
        , "List/indexed"
        , "List/reverse"
        , "Optional"
        , "Some"
        , "None"
        , "Optional/build"
        , "Optional/fold"
        , "NaN"
        , "Infinity"
        ]


-- | A traversal over the immediate sub-expressions of an expression.
subExpressions :: Applicative f => (Expr s a -> f (Expr s a)) -> Expr s a -> f (Expr s a)
subExpressions _ (Const c) = pure (Const c)
subExpressions _ (Var v) = pure (Var v)
subExpressions f (Lam a b c) = Lam a <$> f b <*> f c
subExpressions f (Pi a b c) = Pi a <$> f b <*> f c
subExpressions f (App a b) = App <$> f a <*> f b
subExpressions f (Let as b) = Let <$> traverse g as <*> f b
  where
    g (Binding c d e) = Binding c <$> traverse f d <*> f e
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
subExpressions f (NaturalPlus a b) = NaturalPlus <$> f a <*> f b
subExpressions f (NaturalTimes a b) = NaturalTimes <$> f a <*> f b
subExpressions _ Integer = pure Integer
subExpressions _ (IntegerLit n) = pure (IntegerLit n)
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
subExpressions f (OptionalLit a b) = OptionalLit <$> f a <*> traverse f b
subExpressions f (Some a) = Some <$> f a
subExpressions _ None = pure None
subExpressions _ OptionalFold = pure OptionalFold
subExpressions _ OptionalBuild = pure OptionalBuild
subExpressions f (Record a) = Record <$> traverse f a
subExpressions f ( RecordLit a ) = RecordLit <$> traverse f a
subExpressions f (Union a) = Union <$> traverse (traverse f) a
subExpressions f (UnionLit a b c) =
    UnionLit a <$> f b <*> traverse (traverse f) c
subExpressions f (Combine a b) = Combine <$> f a <*> f b
subExpressions f (CombineTypes a b) = CombineTypes <$> f a <*> f b
subExpressions f (Prefer a b) = Prefer <$> f a <*> f b
subExpressions f (Merge a b t) = Merge <$> f a <*> f b <*> traverse f t
subExpressions f (Field a b) = Field <$> f a <*> pure b
subExpressions f (Project a b) = Project <$> f a <*> pure b
subExpressions f (Note a b) = Note a <$> f b
subExpressions f (ImportAlt l r) = ImportAlt <$> f l <*> f r
subExpressions _ (Embed a) = pure (Embed a)

-- | A traversal over the immediate sub-expressions in 'Chunks'.
chunkExprs
  :: Applicative f
  => (Expr s a -> f (Expr t b))
  -> Chunks s a -> f (Chunks t b)
chunkExprs f (Chunks chunks final) =
  flip Chunks final <$> traverse (traverse f) chunks

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

prettyPathComponent :: Text -> Doc ann
prettyPathComponent text
    | Data.Text.all pathCharacter text =
        "/" <> Pretty.pretty text
    | otherwise =
        "/\"" <> Pretty.pretty text <> "\""

prettyURIComponent :: Text -> Doc ann
prettyURIComponent text
    | Data.Text.all (\c -> pathCharacter c && URI.Encode.isAllowed c) text =
        "/" <> Pretty.pretty text
    | otherwise =
        "/\"" <> Pretty.pretty text <> "\""

{-| Convenience utility for converting `Either`-based exceptions to `IO`-based
    exceptions
-}
throws :: (Exception e, MonadIO io) => Either e a -> io a
throws (Left  e) = liftIO (Control.Exception.throwIO e)
throws (Right r) = return r
