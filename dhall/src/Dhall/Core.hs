{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# OPTIONS_GHC -Wall #-}

{-| This module contains the core calculus for the Dhall language.

    Dhall is essentially a fork of the @morte@ compiler but with more built-in
    functionality, better error messages, and Haskell integration
-}

module Dhall.Core where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative (Applicative(..), (<$>))
#endif

import Control.Exception (Exception(..), throwIO)
import Control.Monad.IO.Class (MonadIO(..))
import Crypto.Hash (SHA256)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup(..))
import Data.Sequence (Seq)
import Data.String (IsString(..))
import Data.Text (Text)
import Dhall.Map (Map)
import Dhall.Set (Set)
import Numeric.Natural (Natural)
import qualified Crypto.Hash

--------------------------------------------------------------------------------

-- | The body of an interpolated @Text@ literal
data Chunks a = Chunks ![(Text, a)] !Text
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Data.Semigroup.Semigroup (Chunks a) where
    Chunks xysL zL <> Chunks         []    zR =
        Chunks xysL (zL <> zR)
    Chunks xysL zL <> Chunks ((x, y):xysR) zR =
        Chunks (xysL ++ (zL <> x, y):xysR) zR

instance Monoid (Chunks a) where
    mempty = Chunks [] mempty

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

instance IsString (Chunks a) where
    fromString str = Chunks [] (fromString str)

-- | Like `Data.Void.Void`, except with a shorter inferred type
data X

absurd :: X -> a
absurd x = case x of

instance Show X where
    show _ = undefined

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
  deriving (Show, Eq, Ord)


-- Imports
--------------------------------------------------------------------------------

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

-- | How to interpret the import's contents (i.e. as Dhall code or raw text)
data ImportMode = Code | RawText | Location
  deriving (Eq, Ord, Show)

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
    deriving (Eq, Ord, Show)

data Scheme = HTTP | HTTPS
  deriving (Show, Eq, Ord)

instance Semigroup File where
    File directory₀ _ <> File directory₁ file =
        File (directory₀ <> directory₁) file

{-| A `File` is a `directory` followed by one additional path component
    representing the `file` name
-}
data File = File
    { directory :: !Directory
    , file      :: !Text
    } deriving (Eq, Ord, Show)

-- | The type of import (i.e. local vs. remote vs. environment)
data ImportType a
    = Local !FilePrefix !File
    -- ^ Local path
    | Remote !(URL a)
    -- ^ URL of remote resource and optional headers stored in an import
    | Env !Text
    -- ^ Environment variable
    | Missing
    deriving (Show, Eq, Ord)

instance Semigroup (ImportType a) where
    Local prefix file₀ <> Local Here file₁ = Local prefix (file₀ <> file₁)

    Remote (URL { path = path₀, ..}) <> Local Here path₁ =
        Remote (URL { path = path₀ <> path₁, ..})

    Local prefix file₀ <> Local Parent file₁ =
        Local prefix (file₀ <> parent <> file₁)

    Remote (URL { path = path₀, .. }) <> Local Parent path₁ =
        Remote (URL { path = path₀ <> parent <> path₁, .. })

    _ <> import₁ = import₁

-- | An `ImportType` extended with an optional hash for semantic integrity checks
data ImportHashed a = ImportHashed
    { hash       :: !(Maybe (Crypto.Hash.Digest SHA256))
    , importType :: !(ImportType a)
    } deriving (Show, Eq, Ord)

instance Semigroup (ImportHashed a) where
    ImportHashed _ importType₀ <> ImportHashed hash importType₁ =
        ImportHashed hash (importType₀ <> importType₁)

-- | Reference to an external resource
data Import a = Import
    { importHashed :: !(ImportHashed a)
    , importMode   :: !ImportMode
    } deriving (Show, Eq, Ord)

instance Semigroup (Import a) where
    Import importHashed₀ _ <> Import importHashed₁ code =
        Import (importHashed₀ <> importHashed₁) code

data URL a = URL
    { scheme    :: !Scheme
    , authority :: !Text
    , path      :: !File
    , query     :: !(Maybe Text)
    , headers   :: !(Maybe (Expr a))
                                 -- ^ We know by elaboration that this expression
                                 --   is definitionally an association list of headers. We don't
                                 --   store the headers, only the expression, and recompute
                                 --   headers when needed.
    } deriving (Show, Eq, Ord)

-- | This specifies whether an annotation comes from the source syntax, or is
--   inferred during elaboration. We keep track of this information so that
--   we can pretty print elaborated core back with the same annotations as in
--   the source.
data AnnotSource
  -- | > Annotation comes from elaboration.
  = ElabAnnot
  -- | > Annotation comes from source.
  | SourceAnnot
  deriving (Show, Eq, Ord)

data Binding i = Binding
    { variable   :: !Text
    , annotation :: !(Maybe (Expr i))
    , value      :: !(Expr i)
    } deriving (Show, Eq, Ord)

parent :: File
parent = File { directory = Directory { components = [ ".." ] }, file = "" }

-- Core expressions
--------------------------------------------------------------------------------

data Projection i
  -- | > Projecting a single field.
  = ProjSingle !Text
  -- | > Projecting a set of fields. We may have a projection by an expression, in
  --     which case we remember the original expression.
  | ProjSet !(Set Text) !(Maybe (Expr i))
  deriving (Show, Eq, Ord)

data Injection
  -- | > Inject an enum constructor.
  = InjEnum
  -- | > Inject a field constructor.
  | InjField
  deriving (Show, Eq, Ord)

-- | Syntax tree for expressions. The type parameter stands for imports.
data Expr i
    -- | > Const c                                  ~  c
    = Const !Const
    -- | De Bruijn indices.
    | Var !Int
    -- | > Lam (x, _) A b                           ~  λ(x : A) -> b
    | Lam !(Text, AnnotSource) !(Expr i) !(Expr i)
    -- | > Pi "_" i j A B                           ~  (A : i) -> (B : j)
    --   > Pi x   i j   A B                         ~  ∀(x : A : i) -> B : j
    | Pi !Text !Const !Const !(Expr i) !(Expr i)
    -- | > App f a                                  ~  f a
    | App !(Expr i) !(Expr i)
    -- | > Let [Binding x Nothing  r] e             ~  let x     = r in e
    --   > Let [Binding x (Just t) r] e             ~  let x : t = r in e
    | Let !(NonEmpty (Binding i)) !(Expr i)
    -- | > Annot x t                                ~  x : t
    | Annot !(Expr i) !(Expr i)
    -- | > Bool                                     ~  Bool
    | Bool
    -- | > BoolLit b                                ~  b
    | BoolLit !Bool
    -- | > BoolAnd x y                              ~  x && y
    | BoolAnd !(Expr i) !(Expr i)
    -- | > BoolOr  x y                              ~  x || y
    | BoolOr  !(Expr i) !(Expr i)
    -- | > BoolEQ  x y                              ~  x == y
    | BoolEQ  !(Expr i) !(Expr i)
    -- | > BoolNE  x y                              ~  x != y
    | BoolNE  !(Expr i) !(Expr i)
    -- | > BoolIf x y z                             ~  if x then y else z
    | BoolIf !(Expr i) !(Expr i) !(Expr i)
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
    | NaturalPlus !(Expr i) !(Expr i)
    -- | > NaturalTimes x y                         ~  x * y
    | NaturalTimes !(Expr i) !(Expr i)
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
    | TextLit {-# unpack #-} !(Chunks (Expr i))
    -- | > TextAppend x y                           ~  x ++ y
    | TextAppend !(Expr i) !(Expr i)
    -- | > TextShow                                 ~  Text/show
    | TextShow
    -- | > List                                     ~  List
    | List
    -- | > ListLit [x, y, z]                        ~  [x, y, z] : List t
    | ListLit !(Maybe (Expr i, AnnotSource)) !(Seq (Expr i))
    -- | > ListAppend x y                           ~  x # y
    | ListAppend !(Expr i) !(Expr i)
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
    | Some !(Expr i)
    -- | > None                                     ~  None
    | None
    -- | > OptionalFold                             ~  Optional/fold
    | OptionalFold
    -- | > OptionalBuild                            ~  Optional/build
    | OptionalBuild
    -- | > Record       [(k1, t1), (k2, t2)]        ~  { k1 : t1, k2 : t1 }
    | Record    !(Map Text (Expr i))
    -- | > RecordLit    [(k1, v1), (k2, v2)]        ~  { k1 = v1, k2 = v2 }
    | RecordLit !(Map Text (Expr i))
    -- | > Union [(k1, Just t1), (k2, Nothing)]     ~  < k1 : t1 | k2 >
    | Union     !(Map Text (Maybe (Expr i)))
    -- | > Combine x y                              ~  x ∧ y
    | Combine !(Expr i) !(Expr i)
    -- | > CombineTypes x y                         ~  x ⩓ y
    | CombineTypes !(Expr i) !(Expr i)
    -- | > Prefer x y                               ~  x ⫽ y
    | Prefer !(Expr i) !(Expr i)
    -- | > Merge t x y                              ~  merge x y : t
    | Merge !(Expr i) !(Expr i) !(Maybe (Expr i, AnnotSource))
    -- | > ToMap t x                                ~  toMap x : List {mapKey : Text, mapValue : t}
    | ToMap !(Expr i) !(Maybe (Expr i, AnnotSource))
    -- | > Inject e k1 i                            ~ e.k1
    --   We know from elaboration that 'e' is definitionally equal to
    --   a union type, but it is more efficient to only store
    --   the original expression, but not the union itself.
    | Inject !(Expr i) !Text !Injection
    -- | > Project t (ProjSingle k)                 ~ t.k
    --   > Project t (ProjSet [k1, k2])             ~ t.{k1, k2}
    --   > Project t (Proj!(Expr i) ks e            ~ t.(e)
    | Project !(Expr i) !(Projection i)
    -- | > ImportAlt                                ~  e1 ? e2
    --     The 'Bool' annotation picks out the successfully
    --     resolved branch.
    | ImportAlt !(Expr i) !(Expr i) !Bool
    | EmbedImport !i
    deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------

{-| Convenience utility for converting `Either`-based exceptions to `IO`-based
    exceptions
-}
throws :: (Exception e, MonadIO io) => Either e a -> io a
throws (Left  e) = liftIO (Control.Exception.throwIO e)
throws (Right a) = return a


_ERROR :: String
_ERROR = "\ESC[1;31mError\ESC[0m"

{-| Utility function used to throw internal errors that should never happen (in theory). -}
internalError :: String -> String
internalError text = unlines
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
    , text
    , "```                                                                             "
    ]

-- Names
--------------------------------------------------------------------------------

data Names = NEmpty | NBind !Names {-# unpack #-} !Text
  deriving Show

indexNames :: Names -> Int -> Text
indexNames NEmpty       _ = error $ internalError "indexNames: out-of bounds index"
indexNames (NBind _  x) 0 = x
indexNames (NBind ns _) i = indexNames ns (i - 1)

-- | Data type used mostly for printing 'Expr'. Since 'Expr' does not store
--   names in variables, we need a name environment for printing.
data Named a = Named !Names a
  deriving (Show)

countName :: Text -> Names -> Int
countName x = go 0 where
  go !acc NEmpty         = acc
  go  acc (NBind env x') = go (if x == x' then acc + 1 else acc) env


-- Values
--------------------------------------------------------------------------------

-- | Resolved import, containing an elaborated import, the elaborated
--   expression behind the import and the lazy value of this expression.
data I = I !(Import I) !(Expr I) Val

instance Show I where
  show (I i _ _) = show i

instance Eq I where
  (I i _ _) == (I i' _ _) = i == i'

instance Ord I where
  compare (I i _ _) (I i' _ _) = compare i i'

data Env = Empty | Skip !Env | Extend !Env Val
data Closure = Cl !Text !Env !(Expr I)
data VChunks = VChunks ![(Text, Val)] !Text

instance Semigroup VChunks where
  VChunks xys z <> VChunks [] z' = VChunks xys (z <> z')
  VChunks xys z <> VChunks ((x', y'):xys') z' = VChunks (xys ++ (z <> x', y'):xys') z'

instance Monoid VChunks where
  mempty = VChunks [] mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

data HLamInfo
  = Prim
  | Typed !Text Val
  | NaturalFoldCl Val
  | ListFoldCl Val
  | OptionalFoldCl Val

pattern VPrim :: (Val -> Val) -> Val
pattern VPrim f = VHLam Prim f

data VProjection
  = VProjSingle !Text
  | VProjSet !(Set Text) !(Maybe Val)

data Val
  = VConst !Const
  | VVar !Int
  | VPrimVar
  | VApp !Val Val

  | VLam Val {-# unpack #-} !Closure
  | VHLam !HLamInfo !(Val -> Val)

  | VPi !Const !Const Val {-# unpack #-} !Closure
  | VHPi !Text !Const !Const Val !(Val -> Val)

  | VBool
  | VBoolLit !Bool
  | VBoolAnd !Val Val
  | VBoolOr !Val Val
  | VBoolEQ !Val Val
  | VBoolNE !Val Val
  | VBoolIf !Val Val Val

  | VNatural
  | VNaturalLit !Natural
  | VNaturalFold Val !Val !Val !Val
  | VNaturalBuild !Val
  | VNaturalIsZero !Val
  | VNaturalEven !Val
  | VNaturalOdd !Val
  | VNaturalToInteger !Val
  | VNaturalShow !Val
  | VNaturalPlus !Val !Val
  | VNaturalTimes !Val !Val

  | VInteger
  | VIntegerLit !Integer
  | VIntegerShow !Val
  | VIntegerToDouble !Val

  | VDouble
  | VDoubleLit !Double
  | VDoubleShow !Val

  | VText
  | VTextLit {-# unpack #-} !VChunks
  | VTextAppend !Val !Val
  | VTextShow !Val

  | VList !Val
  | VListLit !(Maybe Val) !(Seq Val)
  | VListAppend !Val !Val
  | VListBuild   Val !Val
  | VListFold    Val !Val !Val !Val !Val
  | VListLength  Val !Val
  | VListHead    Val !Val
  | VListLast    Val !Val
  | VListIndexed Val !Val
  | VListReverse Val !Val

  | VOptional Val
  | VSome !Val
  | VNone Val
  | VOptionalFold Val !Val Val !Val !Val
  | VOptionalBuild Val !Val
  | VRecord !(Map Text Val)
  | VRecordLit !(Map Text Val)
  | VUnion !(Map Text (Maybe Val))
  | VCombine !Val !Val
  | VCombineTypes !Val !Val
  | VPrefer !Val !Val
  | VMerge !Val !Val !(Maybe Val)
  | VToMap !Val !(Maybe Val)
  | VInject Val !Text !(Maybe Val)   -- Nothing if enum constructor, Just v if field
  | VProject !Val !VProjection

vFun :: Const -> Const -> Val -> Val -> Val
vFun i j a b = VHPi "_" i j a (\_ -> b)
{-# inline vFun #-}

vType :: Val
vType = VConst Type
{-# inline vType #-}
