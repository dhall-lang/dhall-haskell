{-# LANGUAGE DeriveGeneric #-}

module Dhall.Syntax.Expr
    ( Expr(..)
    ) where

import                Data.List.NonEmpty           (NonEmpty (..))
import                Data.Sequence                (Seq)
import                Data.String                  (IsString (..))
import                Data.Text                    (Text)
import                Data.Traversable             ()
import                Dhall.Map                    (Map)
import {-# SOURCE #-} Dhall.Pretty.Internal        (CharacterSet (..))
import                Dhall.Syntax.Binding
import                Dhall.Syntax.Chunks
import                Dhall.Syntax.Const
import                Dhall.Syntax.FunctionBinding
import                Dhall.Syntax.RecordField
import                Dhall.Syntax.Types
import                Dhall.Syntax.Var
import                GHC.Generics                 (Generic)
import                Numeric.Natural              (Natural)

import qualified Data.Time as Time

{-| Syntax tree for expressions

    The @s@ type parameter is used to track the presence or absence of
    `Dhall.Src.Src` spans:

    * If @s = `Dhall.Src.Src`@ then the code may contains `Dhall.Src.Src` spans
      (either in a `Note` constructor or inline within another constructor, like
      `Let`)
    * If @s = `Data.Void.Void`@ then the code has no `Dhall.Src.Src` spans

    The @a@ type parameter is used to track the presence or absence of imports

    * If @a = `Dhall.Syntax.Import.Import`@ then the code may contain unresolved
      `Dhall.Syntax.Import.Import`s
    * If @a = `Data.Void.Void`@ then the code has no
      `Dhall.Syntax.Import.Import`s
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
    -- See `Dhall.Syntax.MultiLet.MultiLet` for a representation of let-blocks
    -- that mirrors the source code more closely.
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
    -- | > Prefer _ _ x y                           ~  x ⫽ y
    | Prefer (Maybe CharacterSet) PreferAnnotation (Expr s a) (Expr s a)
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
