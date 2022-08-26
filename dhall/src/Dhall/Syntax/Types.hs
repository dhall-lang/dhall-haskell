{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

{-| This module contains the core syntax types.
-}

module Dhall.Syntax.Types (
    -- * 'Expr'
      Const(..)
    , Var(..)
    , Binding(..)
    , makeBinding
    , CharacterSet(..)
    , Chunks(..)
    , DhallDouble(..)
    , PreferAnnotation(..)
    , Expr(..)
    , RecordField(..)
    , makeRecordField
    , FunctionBinding(..)
    , makeFunctionBinding
    , FieldSelection(..)
    , makeFieldSelection
    , WithComponent(..)

    -- ** 'Let'-blocks
    , MultiLet(..)
    , multiLet
    , wrapInLets

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
    ) where

import                Data.List.NonEmpty    (NonEmpty (..))
import                Data.Sequence         (Seq)
import                Data.String           (IsString (..))
import                Data.Text             (Text)
import                Dhall.Map             (Map)
import {-# SOURCE #-} Dhall.Pretty.Internal (CharacterSet (..))
import                Dhall.Src             (Src (..))
import                GHC.Generics          (Generic)
import                Numeric.Natural       (Natural)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Time          as Time
import qualified Dhall.Crypto

-- $setup
-- >>> import Dhall.Binary () -- For the orphan instance for `Serialise (Expr Void Import)`

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
    deriving (Bounded, Enum, Generic)

{-| Label for a bound variable

    The `Data.Text.Text` field is the variable's name (i.e. \"@x@\").

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

    Zero indices are omitted when pretty-printing @Var@s and non-zero indices
    appear as a numeric suffix.
-}
data Var = V Text !Int
    deriving Generic

instance IsString Var where
    fromString str = V (fromString str) 0

-- | Record the binding part of a @let@ expression.
--
-- For example,
--
-- > let {- A -} x {- B -} : {- C -} Bool = {- D -} True in x
--
-- … will be instantiated as follows:
--
-- * @bindingSrc0@ corresponds to the @A@ comment.
-- * @variable@ is @"x"@
-- * @bindingSrc1@ corresponds to the @B@ comment.
-- * @annotation@ is 'Just' a pair, corresponding to the @C@ comment and @Bool@.
-- * @bindingSrc2@ corresponds to the @D@ comment.
-- * @value@ corresponds to @True@.
data Binding s a = Binding
    { bindingSrc0 :: Maybe s
    , variable    :: Text
    , bindingSrc1 :: Maybe s
    , annotation  :: Maybe (Maybe s, Expr s a)
    , bindingSrc2 :: Maybe s
    , value       :: Expr s a
    } deriving Generic

{-| Construct a 'Binding' with no source information and no type annotation.
-}
makeBinding :: Text -> Expr s a -> Binding s a
makeBinding name = Binding Nothing name Nothing Nothing Nothing

-- | This wrapper around 'Prelude.Double' exists for its 'Eq' instance which is
-- defined via the binary encoding of Dhall @Double@s.
newtype DhallDouble = DhallDouble { getDhallDouble :: Double }
    deriving Generic

-- | The body of an interpolated @Text@ literal
data Chunks s a = Chunks [(Text, Expr s a)] Text
    deriving Generic

instance IsString (Chunks s a) where
    fromString str = Chunks [] (fromString str)

-- | Used to record the origin of a @//@ operator (i.e. from source code or a
-- product of desugaring)
data PreferAnnotation s a
    = PreferFromSource
    | PreferFromWith (Expr s a)
      -- ^ Stores the original @with@ expression
    | PreferFromCompletion
    deriving Generic

-- | Record the field of a record-type and record-literal expression.
-- The reason why we use the same ADT for both of them is because they store
-- the same information.
--
-- For example,
--
-- > { {- A -} x {- B -} : {- C -} T }
--
-- ... or
--
-- > { {- A -} x {- B -} = {- C -} T }
--
-- will be instantiated as follows:
--
-- * @recordFieldSrc0@ corresponds to the @A@ comment.
-- * @recordFieldValue@ is @"T"@
-- * @recordFieldSrc1@ corresponds to the @B@ comment.
-- * @recordFieldSrc2@ corresponds to the @C@ comment.
--
-- Although the @A@ comment isn't annotating the @"T"@ Record Field,
-- this is the best place to keep these comments.
--
-- Note that @recordFieldSrc2@ is always 'Nothing' when the 'RecordField' is for
-- a punned entry, because there is no @=@ sign. For example,
--
-- > { {- A -} x {- B -} }
--
-- will be instantiated as follows:
--
-- * @recordFieldSrc0@ corresponds to the @A@ comment.
-- * @recordFieldValue@ corresponds to @(Var "x")@
-- * @recordFieldSrc1@ corresponds to the @B@ comment.
-- * @recordFieldSrc2@ will be 'Nothing'
--
-- The labels involved in a record using dot-syntax like in this example:
--
-- > { {- A -} a {- B -} . {- C -} b {- D -} . {- E -} c {- F -} = {- G -} e }
--
-- will be instantiated as follows:
--
-- * For both the @a@ and @b@ field, @recordfieldSrc2@ is 'Nothing'
-- * For the @a@ field:
--   * @recordFieldSrc0@ corresponds to the @A@ comment
--   * @recordFieldSrc1@ corresponds to the @B@ comment
-- * For the @b@ field:
--   * @recordFieldSrc0@ corresponds to the @C@ comment
--   * @recordFieldSrc1@ corresponds to the @D@ comment
-- * For the @c@ field:
--   * @recordFieldSrc0@ corresponds to the @E@ comment
--   * @recordFieldSrc1@ corresponds to the @F@ comment
--   * @recordFieldSrc2@ corresponds to the @G@ comment
--
-- That is, for every label except the last one the semantics of
-- @recordFieldSrc0@ and @recordFieldSrc1@ are the same from a regular record
-- label but @recordFieldSrc2@ is always 'Nothing'. For the last keyword, all
-- srcs are 'Just'
data RecordField s a = RecordField
    { recordFieldSrc0  :: Maybe s
    , recordFieldValue :: Expr s a
    , recordFieldSrc1  :: Maybe s
    , recordFieldSrc2  :: Maybe s
    } deriving Generic

-- | Construct a 'RecordField' with no src information
makeRecordField :: Expr s a -> RecordField s a
makeRecordField e = RecordField Nothing e Nothing Nothing

-- | Record the label of a function or a function-type expression
--
-- For example,
--
-- > λ({- A -} a {- B -} : {- C -} T) -> e
--
-- … will be instantiated as follows:
--
-- * @functionBindingSrc0@ corresponds to the @A@ comment
-- * @functionBindingVariable@ is @a@
-- * @functionBindingSrc1@ corresponds to the @B@ comment
-- * @functionBindingSrc2@ corresponds to the @C@ comment
-- * @functionBindingAnnotation@ is @T@
data FunctionBinding s a = FunctionBinding
    { functionBindingSrc0 :: Maybe s
    , functionBindingVariable :: Text
    , functionBindingSrc1 :: Maybe s
    , functionBindingSrc2 :: Maybe s
    , functionBindingAnnotation :: Expr s a
    } deriving Generic

-- | Smart constructor for 'FunctionBinding' with no src information
makeFunctionBinding :: Text -> Expr s a -> FunctionBinding s a
makeFunctionBinding l t = FunctionBinding Nothing l Nothing Nothing t

-- | Record the field on a selector-expression
--
-- For example,
--
-- > e . {- A -} x {- B -}
--
-- … will be instantiated as follows:
--
-- * @fieldSelectionSrc0@ corresponds to the @A@ comment
-- * @fieldSelectionLabel@ corresponds to @x@
-- * @fieldSelectionSrc1@ corresponds to the @B@ comment
--
-- Given our limitation that not all expressions recover their whitespaces, the
-- purpose of @fieldSelectionSrc1@ is to save the 'Text.Megaparsec.SourcePos'
-- where the @fieldSelectionLabel@ ends, but we /still/ use a 'Maybe Src'
-- (@s = 'Src'@) to be consistent with similar data types such as 'Binding', for
-- example.
data FieldSelection s = FieldSelection
    { fieldSelectionSrc0 :: Maybe s
    , fieldSelectionLabel :: !Text
    , fieldSelectionSrc1 :: Maybe s
    } deriving Generic

-- | Smart constructor for 'FieldSelection' with no src information
makeFieldSelection :: Text -> FieldSelection s
makeFieldSelection t = FieldSelection Nothing t Nothing

-- | A path component for a @with@ expression
data WithComponent = WithLabel Text | WithQuestion
    deriving Generic

{-| Syntax tree for expressions

    The @s@ type parameter is used to track the presence or absence of `Src`
    spans:

    * If @s = `Src`@ then the code may contains `Src` spans (either in a `Note`
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
    -- | > Lam _ (FunctionBinding _ "x" _ _ A) b    ~  λ(x : A) -> b
    | Lam (Maybe CharacterSet) (FunctionBinding s a) (Expr s a)
    -- | > Pi _ "_" A B                               ~        A  -> B
    --   > Pi _ x   A B                               ~  ∀(x : A) -> B
    | Pi  (Maybe CharacterSet) Text (Expr s a) (Expr s a)
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
    -- | > IntegerClamp                             ~  Integer/clamp
    | IntegerClamp
    -- | > IntegerNegate                            ~  Integer/negate
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
    -- | > TextReplace                              ~ Text/replace
    | TextReplace
    -- | > TextShow                                 ~  Text/show
    | TextShow
    -- | > Date                                     ~  Date
    | Date
    -- | > DateLiteral (fromGregorian _YYYY _MM _DD) ~ YYYY-MM-DD
    | DateLiteral Time.Day
    -- | > Time                                     ~  Time
    | Time
    -- | > TimeLiteral (TimeOfDay hh mm ss) _       ~  hh:mm:ss
    | TimeLiteral
        Time.TimeOfDay
        Word
        -- ^ Precision
    -- | > TimeZone                                 ~  TimeZone
    | TimeZone
    -- | > TimeZoneLiteral (TimeZone ( 60 * _HH + _MM) _ _) ~ +HH:MM
    -- | > TimeZoneLiteral (TimeZone (-60 * _HH + _MM) _ _) ~ -HH:MM
    | TimeZoneLiteral Time.TimeZone
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
    -- | > Record [ (k1, RecordField _ t1)          ~  { k1 : t1, k2 : t1 }
    --   >        , (k2, RecordField _ t2)
    --   >        ]
    | Record    (Map Text (RecordField s a))
    -- | > RecordLit [ (k1, RecordField _ v1)       ~  { k1 = v1, k2 = v2 }
    --   >           , (k2, RecordField _ v2)
    --   >           ]
    | RecordLit (Map Text (RecordField s a))
    -- | > Union        [(k1, Just t1), (k2, Nothing)] ~  < k1 : t1 | k2 >
    | Union     (Map Text (Maybe (Expr s a)))
    -- | > Combine _ Nothing x y                    ~  x ∧ y
    --
    -- The first field is a `Just` when the `Combine` operator is introduced
    -- as a result of desugaring duplicate record fields:
    --
    --   > RecordLit [ ( k                          ~ { k = x, k = y }
    --   >           , RecordField
    --   >              _
    --   >              (Combine (Just k) x y)
    --   >            )]
    | Combine (Maybe CharacterSet) (Maybe Text) (Expr s a) (Expr s a)
    -- | > CombineTypes _ x y                       ~  x ⩓ y
    | CombineTypes (Maybe CharacterSet) (Expr s a) (Expr s a)
    -- | > Prefer _ False x y                       ~  x ⫽ y
    --
    -- The first field is a `True` when the `Prefer` operator is introduced as a
    -- result of desugaring a @with@ expression
    | Prefer (Maybe CharacterSet) (PreferAnnotation s a) (Expr s a) (Expr s a)
    -- | > RecordCompletion x y                     ~  x::y
    | RecordCompletion (Expr s a) (Expr s a)
    -- | > Merge x y (Just t )                      ~  merge x y : t
    --   > Merge x y  Nothing                       ~  merge x y
    | Merge (Expr s a) (Expr s a) (Maybe (Expr s a))
    -- | > ToMap x (Just t)                         ~  toMap x : t
    --   > ToMap x  Nothing                         ~  toMap x
    | ToMap (Expr s a) (Maybe (Expr s a))
    -- | > ShowConstructor x                        ~  showConstructor x
    | ShowConstructor (Expr s a)
    -- | > Field e (FieldSelection _ x _)              ~  e.x
    | Field (Expr s a) (FieldSelection s)
    -- | > Project e (Left xs)                      ~  e.{ xs }
    --   > Project e (Right t)                      ~  e.(t)
    | Project (Expr s a) (Either [Text] (Expr s a))
    -- | > Assert e                                 ~  assert : e
    | Assert (Expr s a)
    -- | > Equivalent _ x y                           ~  x ≡ y
    | Equivalent (Maybe CharacterSet) (Expr s a) (Expr s a)
    -- | > With x y e                               ~  x with y = e
    | With (Expr s a) (NonEmpty WithComponent) (Expr s a)
    -- | > Note s x                                 ~  e
    | Note s (Expr s a)
    -- | > ImportAlt                                ~  e1 ? e2
    | ImportAlt (Expr s a) (Expr s a)
    -- | > Embed import                             ~  import
    | Embed a
    deriving Generic
-- NB: If you add a constructor to Expr, please also update the Arbitrary
-- instance in Dhall.Test.QuickCheck.

instance IsString (Expr s a) where
    fromString str = Var (fromString str)

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
        in  MultiLet (NonEmpty.cons b0 bs) e
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

{-| Internal representation of a directory that stores the path components in
    reverse order

    In other words, the directory @\/foo\/bar\/baz@ is encoded as
    @Directory { components = [ "baz", "bar", "foo" ] }@
-}
newtype Directory = Directory { components :: [Text] }
    deriving Generic

{-| A `File` is a `directory` followed by one additional path component
    representing the `file` name
-}
data File = File
    { directory :: Directory
    , file      :: Text
    } deriving Generic

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
    deriving Generic

-- | The URI scheme
data Scheme = HTTP | HTTPS
    deriving Generic

-- | This type stores all of the components of a remote import
data URL = URL
    { scheme    :: Scheme
    , authority :: Text
    , path      :: File
    , query     :: Maybe Text
    , headers   :: Maybe (Expr Src Import)
    } deriving Generic

-- | The type of import (i.e. local vs. remote vs. environment)
data ImportType
    = Local FilePrefix File
    -- ^ Local path
    | Remote URL
    -- ^ URL of remote resource and optional headers stored in an import
    | Env  Text
    -- ^ Environment variable
    | Missing
    deriving Generic

-- | How to interpret the import's contents (i.e. as Dhall code or raw text)
data ImportMode = Code | RawText | Location
  deriving Generic

-- | A `ImportType` extended with an optional hash for semantic integrity checks
data ImportHashed = ImportHashed
    { hash       :: Maybe Dhall.Crypto.SHA256Digest
    , importType :: ImportType
    } deriving Generic

-- | Reference to an external resource
data Import = Import
    { importHashed :: ImportHashed
    , importMode   :: ImportMode
    } deriving Generic
