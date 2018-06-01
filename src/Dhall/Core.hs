{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}
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
    , Path
    , Var(..)
    , Chunks(..)
    , Expr(..)

    -- * Normalization
    , alphaNormalize
    , normalize
    , normalizeWith
    , Normalizer
    , judgmentallyEqual
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
import Crypto.Hash (SHA256)
import Data.Bifunctor (Bifunctor(..))
import Data.Foldable
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashSet (HashSet)
import Data.String (IsString(..))
import Data.Scientific (Scientific)
import Data.Semigroup (Semigroup(..))
import Data.Sequence (Seq, ViewL(..), ViewR(..))
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty)
import Data.Traversable
import {-# SOURCE #-} Dhall.Pretty.Internal
import Formatting.Buildable (Buildable(..))
import Numeric.Natural (Natural)
import Prelude hiding (succ)

import qualified Control.Monad
import qualified Crypto.Hash
import qualified Data.HashMap.Strict.InsOrd
import qualified Data.HashSet
import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Prettyprint.Doc  as Pretty

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
    build = Builder.fromText . buildConst

{-| Internal representation of a directory that stores the path components in
    reverse order

    In other words, the directory @\/foo\/bar\/baz@ is encoded as
    @Directory { components = [ "baz", "bar", "foo" ] }@
-}
newtype Directory = Directory { components :: [Text] }
    deriving (Eq, Ord, Show)

instance Semigroup Directory where
    Directory components₀ <> Directory components₁ =
        Directory (components₁ <> components₀)

instance Buildable Directory where
    build (Directory {..}) =
        foldMap buildComponent (reverse components)
      where
        buildComponent text = "/" <> build text

{-| A `File` is a `directory` followed by one additional path component
    representing the `file` name
-}
data File = File
    { directory :: Directory
    , file      :: Text
    } deriving (Eq, Ord, Show)

instance Buildable File where
    build (File {..}) = build directory <> "/" <> build file

instance Semigroup File where
    File directory₀ _ <> File directory₁ file =
        File (directory₀ <> directory₁) file

data FilePrefix
    = Absolute
    -- ^ Absolute path
    | Here
    -- ^ Path relative to @.@
    | Home
    -- ^ Path relative to @~@
    deriving (Eq, Ord, Show)

instance Buildable FilePrefix where
    build Absolute = ""
    build Here     = "."
    build Home     = "~"

-- | The type of import (i.e. local vs. remote vs. environment)
data ImportType
    = Local FilePrefix File
    -- ^ Local path
    | URL Text File Text (Maybe ImportHashed)
    -- ^ URL of remote resource and optional headers stored in an import
    | Env  Text
    -- ^ Environment variable
    deriving (Eq, Ord, Show)

instance Semigroup ImportType where
    Local prefix file₀ <> Local Here file₁ = Local prefix (file₀ <> file₁)

    URL prefix file₀ suffix headers <> Local Here file₁ =
        URL prefix (file₀ <> file₁) suffix headers

    _ <> import₁ =
        import₁

instance Buildable ImportType where
    build (Local prefix file) =
        build prefix <> build file <> " "

    build (URL prefix file suffix headers) =
            build prefix
        <>  build file
        <>  build suffix
        <>  foldMap buildHeaders headers
        <>  " "
      where
        buildHeaders h = " using " <> build h

    build (Env env) =
        "env:" <> build env <> " "

-- | How to interpret the import's contents (i.e. as Dhall code or raw text)
data ImportMode = Code | RawText deriving (Eq, Ord, Show)

-- | A `ImportType` extended with an optional hash for semantic integrity checks
data ImportHashed = ImportHashed
    { hash       :: Maybe (Crypto.Hash.Digest SHA256)
    , importType :: ImportType
    } deriving (Eq, Ord, Show)

instance Semigroup ImportHashed where
    ImportHashed _ importType₀ <> ImportHashed hash importType₁ =
        ImportHashed hash (importType₀ <> importType₁)

instance Buildable ImportHashed where
    build (ImportHashed  Nothing p) =
      build p
    build (ImportHashed (Just h) p) =
      build p <> "sha256:" <> build (show h) <> " "

-- | Reference to an external resource
data Import = Import
    { importHashed :: ImportHashed
    , importMode   :: ImportMode
    } deriving (Eq, Ord, Show)

instance Semigroup Import where
    Import importHashed₀ _ <> Import importHashed₁ code =
        Import (importHashed₀ <> importHashed₁) code

instance Buildable Import where
    build (Import {..}) = build importHashed <> suffix
      where
        suffix = case importMode of
            RawText -> "as Text"
            Code    -> ""

instance Pretty Import where
    pretty import_ = Pretty.pretty (Builder.toLazyText (build import_))

-- | Type synonym for `Import`, provided for backwards compatibility
type Path = Import

{-# DEPRECATED Path "Use Dhall.Core.Import instead" #-}

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
    build = Builder.fromText . buildVar

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
    -- | > OptionalFold                             ~  Optional/fold
    | OptionalFold
    -- | > OptionalBuild                            ~  Optional/build
    | OptionalBuild
    -- | > Record       [(k1, t1), (k2, t2)]        ~  { k1 : t1, k2 : t1 }
    | Record    (InsOrdHashMap Text (Expr s a))
    -- | > RecordLit    [(k1, v1), (k2, v2)]        ~  { k1 = v1, k2 = v2 }
    | RecordLit (InsOrdHashMap Text (Expr s a))
    -- | > Union        [(k1, t1), (k2, t2)]        ~  < k1 : t1 | k2 : t2 >
    | Union     (InsOrdHashMap Text (Expr s a))
    -- | > UnionLit k v [(k1, t1), (k2, t2)]        ~  < k = v | k1 : t1 | k2 : t2 >
    | UnionLit Text (Expr s a) (InsOrdHashMap Text (Expr s a))
    -- | > Combine x y                              ~  x ∧ y
    | Combine (Expr s a) (Expr s a)
    -- | > CombineTypes x y                         ~  x ⩓ y
    | CombineTypes (Expr s a) (Expr s a)
    -- | > CombineRight x y                         ~  x ⫽ y
    | Prefer (Expr s a) (Expr s a)
    -- | > Merge x y (Just t )                      ~  merge x y : t
    --   > Merge x y  Nothing                       ~  merge x y
    | Merge (Expr s a) (Expr s a) (Maybe (Expr s a))
    -- | > Constructors e                           ~  constructors e
    | Constructors (Expr s a)
    -- | > Field e x                                ~  e.x
    | Field (Expr s a) Text
    -- | > Project e xs                             ~  e.{ xs }
    | Project (Expr s a) (Set Text)
    -- | > Note s x                                 ~  e
    | Note s (Expr s a)
    -- | > Embed import                             ~  import
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
    CombineTypes a b     >>= k = CombineTypes (a >>= k) (b >>= k)
    Prefer a b           >>= k = Prefer (a >>= k) (b >>= k)
    Merge a b c          >>= k = Merge (a >>= k) (b >>= k) (fmap (>>= k) c)
    Constructors a       >>= k = Constructors (a >>= k)
    Field a b            >>= k = Field (a >>= k) b
    Project a b          >>= k = Project (a >>= k) b
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
    first k (CombineTypes a b    ) = CombineTypes (first k a) (first k b)
    first k (Prefer a b          ) = Prefer (first k a) (first k b)
    first k (Merge a b c         ) = Merge (first k a) (first k b) (fmap (first k) c)
    first k (Constructors a      ) = Constructors (first k a)
    first k (Field a b           ) = Field (first k a) b
    first k (Project a b         ) = Project (first k a) b
    first k (Note a b            ) = Note (k a) (first k b)
    first _ (Embed a             ) = Embed a

    second = fmap

instance IsString (Expr s a) where
    fromString str = Var (fromString str)

-- | The body of an interpolated @Text@ literal
data Chunks s a = Chunks [(Text, Expr s a)] Text
    deriving (Functor, Foldable, Traversable, Show, Eq)

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
instance Buildable a => Buildable (Expr s a) where
    build = Builder.fromText . buildExpr

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
shift d v (Constructors a) = Constructors a'
  where
    a' = shift d v  a
shift d v (Field a b) = Field a' b
  where
    a' = shift d v a
shift d v (Project a b) = Project a' b
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
subst x e (Constructors a) = Constructors a'
  where
    a' = subst x e  a
subst x e (Field a b) = Field a' b
  where
    a' = subst x e a
subst x e (Project a b) = Project a' b
  where
    a' = subst x e a
subst x e (Note a b) = Note a b'
  where
    b' = subst x e b
-- The Dhall compiler enforces that all embedded values are closed expressions
-- and `subst` does nothing to a closed expression
subst _ _ (Embed p) = Embed p

{-| α-normalize an expression by renaming all variables to @\"_\"@ and using
    De Bruijn indices to distinguish them
-}
alphaNormalize :: Expr s a -> Expr s a
alphaNormalize (Const c) =
    Const c
alphaNormalize (Var v) =
    Var v
alphaNormalize (Lam x _A₀ b₀) =
    Lam "_" _A₁ b₃
  where
    _A₁ = alphaNormalize _A₀

    v₀ = Var (V "_" 0)
    v₁ = shift 1 (V x 0) v₀
    b₁ = subst (V x 0) v₁ b₀
    b₂ = shift (-1) (V x 0) b₁
    b₃ = alphaNormalize b₂
alphaNormalize (Pi x _A₀ _B₀) =
    Pi "_" _A₁ _B₃
  where
    _A₁ = alphaNormalize _A₀

    v₀  = Var (V "_" 0)
    v₁  = shift 1 (V x 0) v₀
    _B₁ = subst (V x 0) v₁ _B₀
    _B₂ = shift (-1) (V x 0) _B₁
    _B₃ = alphaNormalize _B₂
alphaNormalize (App f₀ a₀) =
    App f₁ a₁
  where
    f₁ = alphaNormalize f₀

    a₁ = alphaNormalize a₀
alphaNormalize (Let x (Just _A₀) a₀ b₀) =
    Let "_" (Just _A₁) a₁ b₃
  where
    _A₁ = alphaNormalize _A₀

    a₁ = alphaNormalize a₀

    v₀ = Var (V "_" 0)
    v₁ = shift 1 (V x 0) v₀
    b₁ = subst (V x 0) v₁ b₀
    b₂ = shift (-1) (V x 0) b₁
    b₃ = alphaNormalize b₂
alphaNormalize (Let x Nothing a₀ b₀) =
    Let "_" Nothing a₁ b₃
  where
    a₁ = alphaNormalize a₀

    v₀ = Var (V "_" 0)
    v₁ = shift 1 (V x 0) v₀
    b₁ = subst (V x 0) v₁ b₀
    b₂ = shift (-1) (V x 0) b₁
    b₃ = alphaNormalize b₂
alphaNormalize (Annot t₀ _T₀) =
    Annot t₁ _T₁
  where
    t₁ = alphaNormalize t₀

    _T₁ = alphaNormalize _T₀
alphaNormalize Bool =
    Bool
alphaNormalize (BoolLit b) =
    BoolLit b
alphaNormalize (BoolAnd l₀ r₀) =
    BoolAnd l₁ r₁
  where
    l₁ = alphaNormalize l₀

    r₁ = alphaNormalize r₀
alphaNormalize (BoolOr l₀ r₀) =
    BoolOr l₁ r₁
  where
    l₁ = alphaNormalize l₀

    r₁ = alphaNormalize r₀
alphaNormalize (BoolEQ l₀ r₀) =
    BoolEQ l₁ r₁
  where
    l₁ = alphaNormalize l₀

    r₁ = alphaNormalize r₀
alphaNormalize (BoolNE l₀ r₀) =
    BoolNE l₁ r₁
  where
    l₁ = alphaNormalize l₀

    r₁ = alphaNormalize r₀
alphaNormalize (BoolIf t₀ l₀ r₀) =
    BoolIf t₁ l₁ r₁
  where
    t₁ = alphaNormalize t₀

    l₁ = alphaNormalize l₀

    r₁ = alphaNormalize r₀
alphaNormalize Natural =
    Natural
alphaNormalize (NaturalLit n) =
    NaturalLit n
alphaNormalize NaturalFold =
    NaturalFold
alphaNormalize NaturalBuild =
    NaturalBuild
alphaNormalize NaturalIsZero =
    NaturalIsZero
alphaNormalize NaturalEven =
    NaturalEven
alphaNormalize NaturalOdd =
    NaturalOdd
alphaNormalize NaturalToInteger =
    NaturalToInteger
alphaNormalize NaturalShow =
    NaturalShow
alphaNormalize (NaturalPlus l₀ r₀) =
    NaturalPlus l₁ r₁
  where
    l₁ = alphaNormalize l₀

    r₁ = alphaNormalize r₀
alphaNormalize (NaturalTimes l₀ r₀) =
    NaturalTimes l₁ r₁
  where
    l₁ = alphaNormalize l₀

    r₁ = alphaNormalize r₀
alphaNormalize Integer =
    Integer
alphaNormalize (IntegerLit n) =
    IntegerLit n
alphaNormalize IntegerShow =
    IntegerShow
alphaNormalize Double =
    Double
alphaNormalize (DoubleLit n) =
    DoubleLit n
alphaNormalize DoubleShow =
    DoubleShow
alphaNormalize Text =
    Text
alphaNormalize (TextLit (Chunks xys₀ z)) =
    TextLit (Chunks xys₁ z)
  where
    xys₁ = do
        (x, y₀) <- xys₀
        let y₁ = alphaNormalize y₀
        return (x, y₁)
alphaNormalize (TextAppend l₀ r₀) =
    TextAppend l₁ r₁
  where
    l₁ = alphaNormalize l₀

    r₁ = alphaNormalize r₀
alphaNormalize List =
    List
alphaNormalize (ListLit (Just _T₀) ts₀) =
    ListLit (Just _T₁) ts₁
  where
    _T₁ = alphaNormalize _T₀

    ts₁ = fmap alphaNormalize ts₀
alphaNormalize (ListLit Nothing ts₀) =
    ListLit Nothing ts₁
  where
    ts₁ = fmap alphaNormalize ts₀
alphaNormalize (ListAppend l₀ r₀) =
    ListAppend l₁ r₁
  where
    l₁ = alphaNormalize l₀

    r₁ = alphaNormalize r₀
alphaNormalize ListBuild =
    ListBuild
alphaNormalize ListFold =
    ListFold
alphaNormalize ListLength =
    ListLength
alphaNormalize ListHead =
    ListHead
alphaNormalize ListLast =
    ListLast
alphaNormalize ListIndexed =
    ListIndexed
alphaNormalize ListReverse =
    ListReverse
alphaNormalize Optional =
    Optional
alphaNormalize (OptionalLit _T₀ ts₀) =
    OptionalLit _T₁ ts₁
  where
    _T₁ = alphaNormalize _T₀

    ts₁ = fmap alphaNormalize ts₀
alphaNormalize OptionalFold =
    OptionalFold
alphaNormalize OptionalBuild =
    OptionalBuild
alphaNormalize (Record kts₀) =
    Record kts₁
  where
    kts₁ = fmap alphaNormalize kts₀
alphaNormalize (RecordLit kvs₀) =
    RecordLit kvs₁
  where
    kvs₁ = fmap alphaNormalize kvs₀
alphaNormalize (Union kts₀) =
    Union kts₁
  where
    kts₁ = fmap alphaNormalize kts₀
alphaNormalize (UnionLit k v₀ kts₀) =
    UnionLit k v₁ kts₁
  where
    v₁ = alphaNormalize v₀

    kts₁ = fmap alphaNormalize kts₀
alphaNormalize (Combine l₀ r₀) =
    Combine l₁ r₁
  where
    l₁ = alphaNormalize l₀

    r₁ = alphaNormalize r₀
alphaNormalize (CombineTypes l₀ r₀) =
    CombineTypes l₁ r₁
  where
    l₁ = alphaNormalize l₀

    r₁ = alphaNormalize r₀
alphaNormalize (Prefer l₀ r₀) =
    Prefer l₁ r₁
  where
    l₁ = alphaNormalize l₀

    r₁ = alphaNormalize r₀
alphaNormalize (Merge t₀ u₀ _T₀) =
    Merge t₁ u₁ _T₁
  where
    t₁ = alphaNormalize t₀

    u₁ = alphaNormalize u₀

    _T₁ = fmap alphaNormalize _T₀
alphaNormalize (Constructors u₀) =
    Constructors u₁
  where
    u₁ = alphaNormalize u₀
alphaNormalize (Field e₀ a) =
    Field e₁ a
  where
    e₁ = alphaNormalize e₀
alphaNormalize (Project e₀ a) =
    Project e₁ a
  where
    e₁ = alphaNormalize e₀
alphaNormalize (Note s e₀) =
    Note s e₁
  where
    e₁ = alphaNormalize e₀
alphaNormalize (Embed a) =
    Embed a

{-| Reduce an expression to its normal form, performing beta reduction

    `normalize` does not type-check the expression.  You may want to type-check
    expressions before normalizing them since normalization can convert an
    ill-typed expression into a well-typed expression.

    However, `normalize` will not fail if the expression is ill-typed and will
    leave ill-typed sub-expressions unevaluated.
-}
normalize :: Eq a => Expr s a -> Expr t a
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
denote (CombineTypes a b    ) = CombineTypes (denote a) (denote b)
denote (Prefer a b          ) = Prefer (denote a) (denote b)
denote (Merge a b c         ) = Merge (denote a) (denote b) (fmap denote c)
denote (Constructors a      ) = Constructors (denote a)
denote (Field a b           ) = Field (denote a) b
denote (Project a b         ) = Project (denote a) b
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
normalizeWith :: Eq a => Normalizer a -> Expr s a -> Expr t a
normalizeWith ctx e0 = loop (denote e0)
 where
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
            -- build/fold fusion for `List`
            App (App ListBuild _) (App (App ListFold _) e') -> loop e'

            -- build/fold fusion for `Natural`
            App NaturalBuild (App NaturalFold e') -> loop e'

            -- build/fold fusion for `Optional`
            App (App OptionalBuild _) (App (App OptionalFold _) e') -> loop e'

            App (App (App (App NaturalFold (NaturalLit n0)) t) succ') zero ->
                if boundedType (loop t) then strict else lazy
              where
                strict =       strictLoop n0
                lazy   = loop (  lazyLoop n0)

                strictLoop !0 = loop zero
                strictLoop !n = loop (App succ' (strictLoop (n - 1)))

                lazyLoop !0 = zero
                lazyLoop !n = App succ' (lazyLoop (n - 1))
            App NaturalBuild g -> loop (App (App (App g Natural) succ) zero)
              where
                succ = Lam "x" Natural (NaturalPlus "x" (NaturalLit 1))

                zero = NaturalLit 0
            App NaturalIsZero (NaturalLit n) -> BoolLit (n == 0)
            App NaturalEven (NaturalLit n) -> BoolLit (even n)
            App NaturalOdd (NaturalLit n) -> BoolLit (odd n)
            App NaturalToInteger (NaturalLit n) -> IntegerLit (toInteger n)
            App NaturalShow (NaturalLit n) ->
                TextLit (Chunks [] (buildNatural n))
            App IntegerShow (IntegerLit n)
                | 0 <= n    -> TextLit (Chunks [] ("+" <> buildNumber n))
                | otherwise -> TextLit (Chunks [] (buildNumber n))
            App DoubleShow (DoubleLit n) ->
                TextLit (Chunks [] (buildScientific n))
            App (App OptionalBuild _A₀) g ->
                loop (App (App (App g optional) just) nothing)
              where
                _A₁ = shift 1 "a" _A₀

                optional = App Optional _A₀

                just = Lam "a" _A₀ (OptionalLit _A₁ (pure "a"))

                nothing = OptionalLit _A₀ empty
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
            App (App (App (App (App ListFold _) (ListLit _ xs)) t) cons) nil ->
                if boundedType (loop t) then strict else lazy
              where
                strict =       foldr strictCons strictNil xs
                lazy   = loop (foldr   lazyCons   lazyNil xs)

                strictNil = loop nil
                lazyNil   =      nil

                strictCons y ys = loop (App (App cons y) ys)
                lazyCons   y ys =       App (App cons y) ys
            App (App ListLength _) (ListLit _ ys) ->
                NaturalLit (fromIntegral (Data.Sequence.length ys))
            App (App ListHead t) (ListLit _ ys) -> loop (OptionalLit t m)
              where
                m = case Data.Sequence.viewl ys of
                        y :< _ -> Just y
                        _      -> Nothing
            App (App ListLast t) (ListLit _ ys) -> loop (OptionalLit t m)
              where
                m = case Data.Sequence.viewr ys of
                        _ :> y -> Just y
                        _      -> Nothing
            App (App ListIndexed _A₀) (ListLit _A₁ as₀) -> loop (ListLit t as₁)
              where
                as₁ = Data.Sequence.mapWithIndex adapt as₀

                _A₂ = Record (Data.HashMap.Strict.InsOrd.fromList kts)
                  where
                    kts = [ ("index", Natural)
                          , ("value", _A₀)
                          ]

                t | null as₀  = Just _A₂
                  | otherwise = Nothing

                adapt n a_ =
                    RecordLit (Data.HashMap.Strict.InsOrd.fromList kvs)
                  where
                    kvs = [ ("index", NaturalLit (fromIntegral n))
                          , ("value", a_)
                          ]
            App (App ListReverse t) (ListLit _ xs) ->
                loop (ListLit m (Data.Sequence.reverse xs))
              where
                m = if Data.Sequence.null xs then Just t else Nothing
            App (App (App (App (App OptionalFold _) (OptionalLit _ xs)) _) just) nothing ->
                loop (maybe nothing just' xs)
              where
                just' = App just
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
    BoolAnd x y -> decide (loop x) (loop y)
      where
        decide (BoolLit True )  r              = r
        decide (BoolLit False)  _              = BoolLit False
        decide  l              (BoolLit True ) = l
        decide  _              (BoolLit False) = BoolLit False
        decide  l               r
            | judgmentallyEqual l r = l
            | otherwise             = BoolAnd l r
    BoolOr x y -> decide (loop x) (loop y)
      where
        decide (BoolLit False)  r              = r
        decide (BoolLit True )  _              = BoolLit True
        decide  l              (BoolLit False) = l
        decide  _              (BoolLit True ) = BoolLit True
        decide  l               r
            | judgmentallyEqual l r = l
            | otherwise             = BoolOr l r
    BoolEQ x y -> decide (loop x) (loop y)
      where
        decide (BoolLit True )  r              = r
        decide  l              (BoolLit True ) = l
        decide  l               r
            | judgmentallyEqual l r = BoolLit True
            | otherwise             = BoolEQ l r
    BoolNE x y -> decide (loop x) (loop y)
      where
        decide (BoolLit False)  r              = r
        decide  l              (BoolLit False) = l
        decide  l               r
            | judgmentallyEqual l r = BoolLit False
            | otherwise             = BoolNE l r
    BoolIf bool true false -> decide (loop bool) (loop true) (loop false)
      where
        decide (BoolLit True )  l              _              = l
        decide (BoolLit False)  _              r              = r
        decide  b              (BoolLit True) (BoolLit False) = b
        decide  b               l              r
            | judgmentallyEqual l r = l
            | otherwise             = BoolIf b l r
    Natural -> Natural
    NaturalLit n -> NaturalLit n
    NaturalFold -> NaturalFold
    NaturalBuild -> NaturalBuild
    NaturalIsZero -> NaturalIsZero
    NaturalEven -> NaturalEven
    NaturalOdd -> NaturalOdd
    NaturalToInteger -> NaturalToInteger
    NaturalShow -> NaturalShow
    NaturalPlus x y -> decide (loop x) (loop y)
      where
        decide (NaturalLit 0)  r             = r
        decide  l             (NaturalLit 0) = l
        decide (NaturalLit m) (NaturalLit n) = NaturalLit (m + n)
        decide  l              r             = NaturalPlus l r
    NaturalTimes x y -> decide (loop x) (loop y)
      where
        decide (NaturalLit 1)  r             = r
        decide  l             (NaturalLit 1) = l
        decide (NaturalLit 0)  _             = NaturalLit 0
        decide  _             (NaturalLit 0) = NaturalLit 0
        decide (NaturalLit m) (NaturalLit n) = NaturalLit (m * n)
        decide  l              r             = NaturalTimes l r
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
    TextAppend x y -> decide (loop x) (loop y)
      where
        isEmpty (Chunks [] "") = True
        isEmpty  _             = False

        decide (TextLit m)  r          | isEmpty m = r
        decide  l          (TextLit n) | isEmpty n = l
        decide (TextLit m) (TextLit n)             = TextLit (m <> n)
        decide  l           r                      = TextAppend l r
    List -> List
    ListLit t es -> ListLit t' es'
      where
        t'  = fmap loop t
        es' = fmap loop es
    ListAppend x y -> decide (loop x) (loop y)
      where
        decide (ListLit _ m)  r            | Data.Sequence.null m = r
        decide  l            (ListLit _ n) | Data.Sequence.null n = l
        decide (ListLit t m) (ListLit _ n)                        = ListLit t (m <> n)
        decide  l             r                                   = ListAppend l r
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
    Combine x y -> decide (loop x) (loop y)
      where
        decide (RecordLit m) r | Data.HashMap.Strict.InsOrd.null m =
            r
        decide l (RecordLit n) | Data.HashMap.Strict.InsOrd.null n =
            l
        decide (RecordLit m) (RecordLit n) =
            RecordLit (Data.HashMap.Strict.InsOrd.unionWith decide m n)
        decide l r =
            Combine l r
    CombineTypes x y -> decide (loop x) (loop y)
      where
        decide (Record m) r | Data.HashMap.Strict.InsOrd.null m =
            r
        decide l (Record n) | Data.HashMap.Strict.InsOrd.null n =
            l
        decide (Record m) (Record n) =
            Record (Data.HashMap.Strict.InsOrd.unionWith decide m n)
        decide l r =
            CombineTypes l r

    Prefer x y -> decide (loop x) (loop y)
      where
        decide (RecordLit m) r | Data.HashMap.Strict.InsOrd.null m =
            r
        decide l (RecordLit n) | Data.HashMap.Strict.InsOrd.null n =
            l
        decide (RecordLit m) (RecordLit n) =
            RecordLit (Data.HashMap.Strict.InsOrd.union n m)
        decide l r =
            Prefer l r
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
    Project r xs     ->
        case loop r of
            RecordLit kvs ->
                case traverse adapt (Data.Set.toList xs) of
                    Just s  ->
                        loop (RecordLit kvs')
                      where
                        kvs' = Data.HashMap.Strict.InsOrd.fromList s
                    Nothing ->
                        Project (RecordLit (fmap loop kvs)) xs
              where
                adapt x = do
                    v <- Data.HashMap.Strict.InsOrd.lookup x kvs
                    return (x, v)
            r' -> Project r' xs
    Note _ e' -> loop e'
    Embed a -> Embed a

{-| Returns `True` if two expressions are α-equivalent and β-equivalent and
    `False` otherwise
-}
judgmentallyEqual :: Eq a => Expr s a -> Expr t a -> Bool
judgmentallyEqual eL0 eR0 = alphaBetaNormalize eL0 == alphaBetaNormalize eR0
  where
    alphaBetaNormalize :: Eq a => Expr s a -> Expr () a
    alphaBetaNormalize = alphaNormalize . normalize

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
    CombineTypes x y -> isNormalized x && isNormalized y && combine
      where
        combine = case x of
            Record _ -> case y of
                Record _ -> False
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
    Project r xs -> isNormalized r &&
        case r of
            RecordLit kvs ->
                if all (flip Data.HashMap.Strict.InsOrd.member kvs) xs
                    then False
                    else True
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
