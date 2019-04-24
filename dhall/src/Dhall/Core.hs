{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE UnicodeSyntax      #-}
{-# LANGUAGE EmptyCase          #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

{-| This module contains the core calculus for the Dhall language.

    Dhall is essentially a fork of the @morte@ compiler but with more built-in
    functionality, better error messages, and Haskell integration
-}

module Dhall.Core (
    -- * Syntax
      Const(..)
    , Binding(..)
    , Chunks(..)
    , Directory(..)
    , Expr(..)
    , File(..)
    , FilePrefix(..)
    , Import(..)
    , ImportHashed(..)
    , ImportMode(..)
    , ImportType(..)
    , Scheme(..)
    , URL(..)
    , Var(..)
    , X
    , absurd
    , coerceEmbed
    , coerceNote

    -- * Pretty-printing
    , pretty

    -- * Miscellaneous
    , escapeText
    ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative (Applicative(..), (<$>))
#endif

import Control.Applicative (empty)
import Crypto.Hash (SHA256)
import Data.Data (Data(..))
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup(..))
import Data.Sequence (Seq)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty)
import {-# SOURCE #-} Dhall.Pretty.Internal
import {-# SOURCE #-} Dhall.Binary (ToTerm(..), FromTerm(..))
import Dhall.Map (Map)
import Dhall.Set (Set)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Prelude hiding (succ)
import Unsafe.Coerce (unsafeCoerce)

import qualified Crypto.Hash
import qualified Data.Text.Prettyprint.Doc  as Pretty


-- | Like `Data.Void.Void`, except with a shorter inferred type
data X

absurd :: X -> a
absurd x = case x of

instance Show X where
    show _ = undefined

instance Eq X where
  _ == _ = True

instance Data X where
    dataTypeOf _ = undefined
    gunfold _ _ _ = undefined
    toConstr _ = undefined

instance Pretty X where
    pretty _ = undefined

instance FromTerm X where
    decode _ = empty

instance ToTerm X where
    encode _ = undefined


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
    { directory :: !Directory
    , file      :: !Text
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
    { scheme    :: !Scheme
    , authority :: !Text
    , path      :: !File
    , query     :: !(Maybe Text)
    , headers   :: !(Maybe ImportHashed)
    } deriving (Eq, Generic, Ord, Show)

instance Pretty URL where
    pretty (URL {..}) =
            schemeDoc
        <>  "://"
        <>  Pretty.pretty authority
        <>  Pretty.pretty path
        <>  queryDoc
        <>  foldMap prettyHeaders headers
      where
        prettyHeaders h = " using " <> Pretty.pretty h

        schemeDoc = case scheme of
            HTTP  -> "http"
            HTTPS -> "https"

        queryDoc = case query of
            Nothing -> ""
            Just q  -> "?" <> Pretty.pretty q

-- | The type of import (i.e. local vs. remote vs. environment)
data ImportType
    = Local !FilePrefix !File
    -- ^ Local path
    | Remote !URL
    -- ^ URL of remote resource and optional headers stored in an import
    | Env  !Text
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
    { hash       :: !(Maybe (Crypto.Hash.Digest SHA256))
    , importType :: !ImportType
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
    { importHashed :: !ImportHashed
    , importMode   :: !ImportMode
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
data Var = V {-# UNPACK #-} !Text !Int
    deriving (Data, Generic, Eq, Show)

instance IsString Var where
    fromString str = V (fromString str) 0

instance Pretty Var where
    pretty = Pretty.unAnnotate . prettyVar

-- | Syntax tree for expressions
data Expr s a
    -- | > Const c                                  ~  c
    = Const !Const
    -- | > Var (V x 0)                              ~  x
    --   > Var (V x n)                              ~  x@n
    | Var {-# UNPACK #-} !Var
    -- | > Lam x     A b                            ~  λ(x : A) -> b
    | Lam !Text !(Expr s a) !(Expr s a)
    -- | > Pi "_" A B                               ~        A  -> B
    --   > Pi x   A B                               ~  ∀(x : A) -> B
    | Pi  !Text !(Expr s a) !(Expr s a)
    -- | > App f a                                  ~  f a
    | App !(Expr s a) !(Expr s a)
    -- | > Let [Binding x Nothing  r] e             ~  let x     = r in e
    --   > Let [Binding x (Just t) r] e             ~  let x : t = r in e
    | Let !(NonEmpty (Binding s a)) !(Expr s a)
    -- | > Annot x t                                ~  x : t
    | Annot !(Expr s a) !(Expr s a)
    -- | > Bool                                     ~  Bool
    | Bool
    -- | > BoolLit b                                ~  b
    | BoolLit !Bool
    -- | > BoolAnd x y                              ~  x && y
    | BoolAnd !(Expr s a) !(Expr s a)
    -- | > BoolOr  x y                              ~  x || y
    | BoolOr  !(Expr s a) !(Expr s a)
    -- | > BoolEQ  x y                              ~  x == y
    | BoolEQ  !(Expr s a) !(Expr s a)
    -- | > BoolNE  x y                              ~  x != y
    | BoolNE  !(Expr s a) !(Expr s a)
    -- | > BoolIf x y z                             ~  if x then y else z
    | BoolIf !(Expr s a) !(Expr s a) !(Expr s a)
    -- | > Natural                                  ~  Natural
    | Natural
    -- | > NaturalLit n                             ~  n
    | NaturalLit !Natural
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
    | NaturalPlus !(Expr s a) !(Expr s a)
    -- | > NaturalTimes x y                         ~  x * y
    | NaturalTimes !(Expr s a) !(Expr s a)
    -- | > Integer                                  ~  Integer
    | Integer
    -- | > IntegerLit n                             ~  ±n
    | IntegerLit !Integer
    -- | > IntegerShow                              ~  Integer/show
    | IntegerShow
    -- | > IntegerToDouble                          ~  Integer/toDouble
    | IntegerToDouble
    -- | > Double                                   ~  Double
    | Double
    -- | > DoubleLit n                              ~  n
    | DoubleLit !Double
    -- | > DoubleShow                               ~  Double/show
    | DoubleShow
    -- | > Text                                     ~  Text
    | Text
    -- | > TextLit (Chunks [(t1, e1), (t2, e2)] t3) ~  "t1${e1}t2${e2}t3"
    | TextLit !(Chunks s a)
    -- | > TextAppend x y                           ~  x ++ y
    | TextAppend !(Expr s a) !(Expr s a)
    -- | > TextShow                                 ~  Text/show
    | TextShow
    -- | > List                                     ~  List
    | List
    -- | > ListLit (Just t ) [x, y, z]              ~  [x, y, z] : List t
    --   > ListLit  Nothing  [x, y, z]              ~  [x, y, z]
    | ListLit !(Maybe (Expr s a)) !(Seq (Expr s a))
    -- | > ListAppend x y                           ~  x # y
    | ListAppend !(Expr s a) !(Expr s a)
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
    | OptionalLit !(Expr s a) !(Maybe (Expr s a))
    -- | > Some e                                   ~  Some e
    | Some !(Expr s a)
    -- | > None                                     ~  None
    | None
    -- | > OptionalFold                             ~  Optional/fold
    | OptionalFold
    -- | > OptionalBuild                            ~  Optional/build
    | OptionalBuild
    -- | > Record       [(k1, t1), (k2, t2)]        ~  { k1 : t1, k2 : t1 }
    | Record    !(Map Text (Expr s a))
    -- | > RecordLit    [(k1, v1), (k2, v2)]        ~  { k1 = v1, k2 = v2 }
    | RecordLit !(Map Text (Expr s a))
    -- | > Union        [(k1, Just t1), (k2, Nothing)] ~  < k1 : t1 | k2 >
    | Union     !(Map Text (Maybe (Expr s a)))
    -- | > UnionLit k v [(k1, Just t1), (k2, Nothing)] ~  < k = v | k1 : t1 | k2 >
    | UnionLit !Text !(Expr s a) !(Map Text (Maybe (Expr s a)))
    -- | > Combine x y                              ~  x ∧ y
    | Combine !(Expr s a) !(Expr s a)
    -- | > CombineTypes x y                         ~  x ⩓ y
    | CombineTypes !(Expr s a) !(Expr s a)
    -- | > Prefer x y                               ~  x ⫽ y
    | Prefer !(Expr s a) !(Expr s a)
    -- | > Merge x y (Just t )                      ~  merge x y : t
    --   > Merge x y  Nothing                       ~  merge x y
    | Merge !(Expr s a) !(Expr s a) !(Maybe (Expr s a))
    -- | > Field e x                                ~  e.x
    | Field !(Expr s a) !Text
    -- | > Project e xs                             ~  e.{ xs }
    | Project !(Expr s a) !(Set Text)
    -- | > Note s x                                 ~  e
    | Note s !(Expr s a)
    -- | > ImportAlt                                ~  e1 ? e2
    | ImportAlt !(Expr s a) !(Expr s a)
    -- | > Embed import                             ~  import
    | Embed a
    deriving (Eq, Show)


coerceNote :: Expr X a -> Expr s a
coerceNote = unsafeCoerce
{-# inline coerceNote #-}

coerceEmbed :: Expr s X -> Expr s a
coerceEmbed = unsafeCoerce
{-# inline coerceEmbed #-}

instance IsString (Expr s a) where
    fromString str = Var (fromString str)

data Binding s a = Binding
    { variable   :: Text
    , annotation :: Maybe (Expr s a)
    , value      :: Expr s a
    } deriving (Show, Eq)

-- | The body of an interpolated @Text@ literal
data Chunks s a = Chunks ![(Text, Expr s a)] !Text
    deriving (Show, Eq)

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
