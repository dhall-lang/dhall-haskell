{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

{-| Eval-apply environment machine with conversion checking and quoting to
    normal forms. Fairly similar to GHCI's STG machine algorithmically, but much
    simpler, with no known call optimization or environment trimming.

    'vApp' forces a 'VLam' argument at the call site when 'whnfCheapType'
    (primitives, function types, and one field-level of those; not lists,
    not nested records). That bang must sit on 'vApp'
    itself: putting it in lazy 'Extend' only wraps a thunk and still leaves
    ChurchEval as @s (s (s z))@ towers. 'Natural/fold' bangs each step with
    'forceAccWHNF' (list spines; record fields that are not lists); the
    @succ acc ≡ acc@ shortcut is only for 'boundedType'. 'forceAccWHNF' is
    'NOINLINE' so that record-field walker does not inflate 'eval' (large5).
    Church-encoded *list* constructors stay lazy: see 'vApp'.

    Potential optimizations without changing Expr:

    * In conversion checking, get non-shadowing variables not by linear
      Env-walking, but by keeping track of Env size, and generating names which
      are known to be illegal as source-level names (to rule out shadowing).

    * Use HashMap Text chunks for large let-definitions blocks. "Large" vs
      "Small" is fairly cheap to determine at evaluation time.

    Potential optimizations with changing Expr:

    * Use actual full de Bruijn indices in Var instead of Text counting indices.
      Then, we'd switch to full de Bruijn levels in Val as well, and use proper
      constant time non-shadowing name generation.
-}

module Dhall.Eval (
    judgmentallyEqual
  , normalize
  , normalizeWithConfig
  , normalizeWithStats
  , normalizeGraph
  , normalizeGraphWith
  , alphaNormalize
  , eval
  , evalInRun
  , quote
  , quoteInRun
  , quoteGraph
  , envNames
  , countNames
  , conv
  , convInRun
  , toVHPi
  , Closure(..)
  , Names(..)
  , Environment(..)
  , skip
  , extend
  , envId
  , Val
  , valId
  , valClosed
  , valF
  , pattern VConst, pattern VVar, pattern VPrimVar, pattern VApp
  , pattern VLam, pattern VHLam, pattern VPi, pattern VHPi, pattern VPiND
  , pattern VBool, pattern VBoolLit, pattern VBoolAnd, pattern VBoolOr
  , pattern VBoolEQ, pattern VBoolNE, pattern VBoolIf
  , pattern VBytes, pattern VBytesLit
  , pattern VNatural, pattern VNaturalLit, pattern VNaturalFold
  , pattern VNaturalBuild, pattern VNaturalIsZero, pattern VNaturalEven
  , pattern VNaturalOdd, pattern VNaturalToInteger, pattern VNaturalShow
  , pattern VNaturalSubtract, pattern VNaturalPlus, pattern VNaturalTimes
  , pattern VInteger, pattern VIntegerLit, pattern VIntegerClamp
  , pattern VIntegerNegate, pattern VIntegerShow, pattern VIntegerToDouble
  , pattern VDouble, pattern VDoubleLit, pattern VDoubleShow
  , pattern VText, pattern VTextLit, pattern VTextAppend, pattern VTextShow
  , pattern VTextReplace
  , pattern VDate, pattern VDateLiteral, pattern VDateShow
  , pattern VTime, pattern VTimeLiteral, pattern VTimeShow
  , pattern VTimeZone, pattern VTimeZoneLiteral, pattern VTimeZoneShow
  , pattern VList, pattern VListLit, pattern VListAppend, pattern VListBuild
  , pattern VListFold, pattern VListLength, pattern VListHead, pattern VListLast
  , pattern VListIndexed, pattern VListReverse
  , pattern VOptional, pattern VSome, pattern VNone
  , pattern VRecord, pattern VRecordLit, pattern VUnion
  , pattern VCombine, pattern VCombineTypes, pattern VPrefer, pattern VMerge
  , pattern VToMap, pattern VShowConstructor, pattern VField, pattern VInject
  , pattern VProject, pattern VAssert, pattern VEquivalent, pattern VWith
  , pattern VEmbed, pattern VPrim
  , (~>)
  , unboundBuiltinTypes
  , EvalConfig(..), EvalStats(..)
  , defaultEvalConfig, cacheOnConfig, cacheOffConfig
  , withEvalRun, withEvalRunIO, readEvalStats, readEvalStatsIO
  , textShow
  , dateShow
  , timeShow
  , timezoneShow
  ) where

import Control.Monad      (foldM)
import Control.Monad.ST   (ST, runST)
import GHC.Clock          (getMonotonicTimeNSec)
import Data.ByteString    (ByteString)
import Data.Foldable      (foldr', toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Sequence      (Seq, ViewL (..), ViewR (..))
import Data.Text          (Text)
import Data.Time          (Day, TimeZone)
import Data.Traversable   (for)
import Data.Void          (Void)
import Data.Word          (Word8)
import Dhall.Map          (Map)
import Dhall.Set          (Set)
import GHC.Natural        (Natural)
import Lens.Micro         (anyOf)
import Prelude            hiding (succ)

import Dhall.Syntax
    ( Binding (..)
    , Chunks (..)
    , Const (..)
    , DhallDouble (..)
    , Expr (..)
    , FunctionBinding (..)
    , PreferAnnotation (..)
    , RecordField (..)
    , Var (..)
    , WithComponent (..)
    )

import qualified Data.Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence as Sequence
import qualified Data.Set
import qualified Data.Text     as Text
import qualified Data.Time     as Time
import qualified Dhall.Map     as Map
import Dhall.Eval.Run
    ( EnvId (..)
    , EvalConfig (..)
    , EvalKey (..)
    , EvalStats (..)
    , ValId (..)
    , cacheOffConfig
    , cacheOnConfig
    , defaultEvalConfig
    , freshEnvId
    , freshValId
    , insertConvMemo
    , insertEvalMemo
    , insertQuoteMemo
    , internBody
    , lookupConvMemo
    , lookupEvalMemo
    , lookupQuoteMemo
    , readEvalStats
    , readEvalStatsIO
    , recordMkVal
    , withEvalRun
    , withEvalRunIO
    )
import Dhall.Core.DAG.Node (NodeId (..))

import qualified Dhall.Core.DAG as DAG
import qualified Dhall.Set
import qualified Dhall.Syntax  as Syntax
import qualified Text.Printf   as Printf

-- | Evaluation environment. The 'Val' in 'Extend' is intentionally lazy:
-- unused @let@-bound values (and unused lambda arguments, once instantiated)
-- are not forced. The environment spine ('Empty'/'Skip'/'Extend') is strict.
data Environment a
    = Empty
    | Skip   !(Environment a) {-# UNPACK #-} !Text {-# UNPACK #-} !EnvId
    | Extend !(Environment a) {-# UNPACK #-} !Text (Val a) {-# UNPACK #-} !EnvId

instance (Show a, Show (Val a -> Val a)) => Show (Environment a) where
    showsPrec _ Empty =
        showString "Empty"
    showsPrec d (Skip env x _) =
        showParen (d > 10) $
            showString "Skip " . showsPrec 11 env . showChar ' ' . showsPrec 11 x
    showsPrec d (Extend env x v _) =
        showParen (d > 10) $
            showString "Extend "
                . showsPrec 11 env . showChar ' '
                . showsPrec 11 x . showChar ' '
                . showsPrec 11 v

-- | Skip a binder in the value environment (fresh 'EnvId').
skip :: Environment a -> Text -> Environment a
skip env x = Skip env x (freshEnvId x)

-- | Extend the value environment with a bound value (fresh 'EnvId').
extend :: Environment a -> Text -> Val a -> Environment a
extend env x v = Extend env x v (freshEnvId v)

envId :: Environment a -> EnvId
envId Empty             = EnvId 0
envId (Skip _ _ i)      = i
envId (Extend _ _ _ i)  = i

errorMsg :: String
errorMsg = unlines
  [ _ERROR <> ": Compiler bug                                                        "
  , "                                                                                "
  , "An ill-typed expression was encountered during normalization.                   "
  , "Explanation: This error message means that there is a bug in the Dhall compiler."
  , "You didn't do anything wrong, but if you would like to see this problem fixed   "
  , "then you should report the bug at:                                              "
  , "                                                                                "
  , "https://github.com/dhall-lang/dhall-haskell/issues                              "
  ]
  where
    _ERROR :: String
    _ERROR = "\ESC[1;31mError\ESC[0m"


data Closure a = Closure
    { cloName   :: !Text
    , cloEnv    :: !(Environment a)
    , cloBody   :: !(Expr Void a)
    , cloBodyId :: !NodeId
    }

instance (Show a, Show (Val a -> Val a)) => Show (Closure a) where
    showsPrec d (Closure x env t _) =
        showParen (d > 10) $
            showString "Closure "
                . showsPrec 11 x . showChar ' '
                . showsPrec 11 env . showChar ' '
                . showsPrec 11 t

data VChunks a = VChunks ![(Text, Val a)] !Text

deriving instance (Show a, Show (Val a -> Val a)) => Show (VChunks a)

instance Semigroup (VChunks a) where
  VChunks xys z <> VChunks [] z' = VChunks xys (z <> z')
  VChunks xys z <> VChunks ((x', y'):xys') z' = VChunks (xys ++ (z <> x', y'):xys') z'

instance Monoid (VChunks a) where
  mempty = VChunks [] mempty

{-| Some information is lost when `eval` converts a `Lam` or a built-in function
    from the `Expr` type to a `VHLam` of the `Val` type and `quote` needs that
    information in order to reconstruct an equivalent `Expr`.  This `HLamInfo`
    type holds that extra information necessary to perform that reconstruction
-}
data HLamInfo a
  = Prim
  -- ^ Don't store any information
  | Typed !Text (Val a)
  -- ^ Store the original name and type of the variable bound by the `Lam`
  | NaturalSubtractZero
  -- ^ The original function was a @Natural/subtract 0@.  We need to preserve
  --   this information in case the @Natural/subtract@ ends up not being fully
  --   saturated, in which case we need to recover the unsaturated built-in
  | TextReplaceEmpty
  -- ^ The original function was a @Text/replace ""@
  | TextReplaceEmptyArgument (Val a)
  -- ^ The original function was a @Text/replace "" replacement@

deriving instance (Show a, Show (Val a -> Val a)) => Show (HLamInfo a)

-- | Whether @V x i@ occurs free in an expression (avoids importing
-- `Dhall.Normalize.freeIn`, which would create an import cycle).
--
-- Used when evaluating @Pi@ to choose @VHPi@ for non-dependent function types
-- so type-checking need not evaluate unused arguments.
boundVarOccurs :: Eq a => Text -> Int -> Expr Void a -> Bool
boundVarOccurs x i = go i
  where
    go !j = \case
        Var (V x' j') ->
            x == x' && j == j'
        Lam _ FunctionBinding{ functionBindingVariable = y
                             , functionBindingAnnotation = a } b ->
            go j a || go (if x == y then j + 1 else j) b
        Pi _ y a b ->
            go j a || go (if x == y then j + 1 else j) b
        Let (Binding _ y _ mA _ a) b ->
               maybe False (\(_, t) -> go j t) mA
            || go j a
            || go (if x == y then j + 1 else j) b
        Note _ e ->
            go j e
        expression ->
            anyOf Syntax.subExpressions (go j) expression
    
data ValF a
    = VFConst !Const
    | VFVar !Text !Int
    | VFPrimVar
    | VFApp (Val a) (Val a)
    | VFLam (Val a) {-# UNPACK #-} !(Closure a)
    | VFHLam (HLamInfo a) !(Val a -> Val a)
    | VFPi (Val a) {-# UNPACK #-} !(Closure a)
    | VFHPi !Text (Val a) !(Val a -> Val a)
    | VFPiND !Text (Val a) (Val a)
    | VFBool
    | VFBoolLit !Bool
    | VFBoolAnd (Val a) (Val a)
    | VFBoolOr (Val a) (Val a)
    | VFBoolEQ (Val a) (Val a)
    | VFBoolNE (Val a) (Val a)
    | VFBoolIf (Val a) (Val a) (Val a)
    | VFBytes
    | VFBytesLit ByteString
    | VFNatural
    | VFNaturalLit !Natural
    | VFNaturalFold (Val a) (Val a) (Val a) (Val a)
    | VFNaturalBuild (Val a)
    | VFNaturalIsZero (Val a)
    | VFNaturalEven (Val a)
    | VFNaturalOdd (Val a)
    | VFNaturalToInteger (Val a)
    | VFNaturalShow (Val a)
    | VFNaturalSubtract (Val a) (Val a)
    | VFNaturalPlus (Val a) (Val a)
    | VFNaturalTimes (Val a) (Val a)
    | VFInteger
    | VFIntegerLit !Integer
    | VFIntegerClamp (Val a)
    | VFIntegerNegate (Val a)
    | VFIntegerShow (Val a)
    | VFIntegerToDouble (Val a)
    | VFDouble
    | VFDoubleLit DhallDouble
    | VFDoubleShow (Val a)
    | VFText
    | VFTextLit (VChunks a)
    | VFTextAppend (Val a) (Val a)
    | VFTextShow (Val a)
    | VFTextReplace (Val a) (Val a) (Val a)
    | VFDate
    | VFDateLiteral Time.Day
    | VFDateShow (Val a)
    | VFTime
    | VFTimeLiteral !Word8 !Word8 !Word8 !Integer !Int
    | VFTimeShow (Val a)
    | VFTimeZone
    | VFTimeZoneLiteral Time.TimeZone
    | VFTimeZoneShow (Val a)
    | VFList (Val a)
    | VFListLit (Maybe (Val a)) (Seq (Val a))
    | VFListAppend (Val a) (Val a)
    | VFListBuild (Val a) (Val a)
    | VFListFold (Val a) (Val a) (Val a) (Val a) (Val a)
    | VFListLength (Val a) (Val a)
    | VFListHead (Val a) (Val a)
    | VFListLast (Val a) (Val a)
    | VFListIndexed (Val a) (Val a)
    | VFListReverse (Val a) (Val a)
    | VFOptional (Val a)
    | VFSome (Val a)
    | VFNone (Val a)
    | VFRecord (Map Text (Val a))
    | VFRecordLit (Map Text (Val a))
    | VFUnion (Map Text (Maybe (Val a)))
    | VFCombine (Maybe Text) (Val a) (Val a)
    | VFCombineTypes (Val a) (Val a)
    | VFPrefer (Val a) (Val a)
    | VFMerge (Val a) (Val a) (Maybe (Val a))
    | VFToMap (Val a) (Maybe (Val a))
    | VFShowConstructor (Val a)
    | VFField (Val a) !Text
    | VFInject (Map Text (Maybe (Val a))) !Text (Maybe (Val a))
    | VFProject (Val a) (Either (Set Text) (Val a))
    | VFAssert (Val a)
    | VFEquivalent (Val a) (Val a)
    | VFWith (Val a) (NonEmpty WithComponent) (Val a)
    | VFEmbed a

deriving instance (Show a, Show (Val a -> Val a)) => Show (ValF a)

data Val a = Val
    { valId     :: {-# UNPACK #-} !ValId
    , valClosed :: !Bool
    , valF      :: !(ValF a)
    }

instance (Show a, Show (Val a -> Val a)) => Show (Val a) where
    showsPrec n (Val _ _ p) = showsPrec n p

mkVal :: Bool -> ValF a -> Val a
mkVal closed payload =
    case recordMkVal payload of
        () -> Val (freshValId payload) closed payload
{-# NOINLINE mkVal #-}

makeClosure :: Eq a => Text -> Environment a -> Expr Void a -> Closure a
makeClosure x env t = Closure x env t (internBody t)

closureClosed :: Closure a -> Bool
closureClosed (Closure _ env _ _) = envClosed env

envClosed :: Environment a -> Bool
envClosed Empty              = True
envClosed (Skip env _ _)     = envClosed env
envClosed (Extend env _ v _) = envClosed env && valClosed v

vChunksClosed :: VChunks a -> Bool
vChunksClosed (VChunks xys _) = all (valClosed . snd) xys

-- Pattern synonyms preserve the existing Val constructor API.
pattern VConst :: Const -> Val a
pattern VConst k <- Val _ _ (VFConst k)
  where
    VConst k = mkVal (True) (VFConst k)

pattern VVar :: Text -> Int -> Val a
pattern VVar x i <- Val _ _ (VFVar x i)
  where
    VVar x i = mkVal (False) (VFVar x i)

pattern VPrimVar :: Val a
pattern VPrimVar <- Val _ _ VFPrimVar
  where
    VPrimVar = mkVal False VFPrimVar

pattern VApp :: (Val a) -> (Val a) -> Val a
pattern VApp t u <- Val _ _ (VFApp t u)
  where
    VApp t u = mkVal (valClosed t && valClosed u) (VFApp t u)

pattern VLam :: (Val a) -> (Closure a) -> Val a
pattern VLam ty c <- Val _ _ (VFLam ty c)
  where
    VLam ty c = mkVal (valClosed ty && closureClosed c) (VFLam ty c)

pattern VHLam :: (HLamInfo a) -> (Val a -> Val a) -> Val a
pattern VHLam i f <- Val _ _ (VFHLam i f)
  where
    VHLam i f = mkVal (False) (VFHLam i f)

pattern VPi :: (Val a) -> (Closure a) -> Val a
pattern VPi ty c <- Val _ _ (VFPi ty c)
  where
    VPi ty c = mkVal (valClosed ty && closureClosed c) (VFPi ty c)

pattern VHPi :: Text -> (Val a) -> (Val a -> Val a) -> Val a
pattern VHPi x ty f <- Val _ _ (VFHPi x ty f)
  where
    VHPi x ty f = mkVal (False) (VFHPi x ty f)

pattern VPiND :: Text -> (Val a) -> (Val a) -> Val a
pattern VPiND x ty b <- Val _ _ (VFPiND x ty b)
  where
    VPiND x ty b = mkVal (valClosed ty && valClosed b) (VFPiND x ty b)

pattern VBool :: Val a
pattern VBool <- Val _ _ VFBool
  where
    VBool = mkVal True VFBool

pattern VBoolLit :: Bool -> Val a
pattern VBoolLit b <- Val _ _ (VFBoolLit b)
  where
    VBoolLit b = mkVal (True) (VFBoolLit b)

pattern VBoolAnd :: (Val a) -> (Val a) -> Val a
pattern VBoolAnd t u <- Val _ _ (VFBoolAnd t u)
  where
    VBoolAnd t u = mkVal (valClosed t && valClosed u) (VFBoolAnd t u)

pattern VBoolOr :: (Val a) -> (Val a) -> Val a
pattern VBoolOr t u <- Val _ _ (VFBoolOr t u)
  where
    VBoolOr t u = mkVal (valClosed t && valClosed u) (VFBoolOr t u)

pattern VBoolEQ :: (Val a) -> (Val a) -> Val a
pattern VBoolEQ t u <- Val _ _ (VFBoolEQ t u)
  where
    VBoolEQ t u = mkVal (valClosed t && valClosed u) (VFBoolEQ t u)

pattern VBoolNE :: (Val a) -> (Val a) -> Val a
pattern VBoolNE t u <- Val _ _ (VFBoolNE t u)
  where
    VBoolNE t u = mkVal (valClosed t && valClosed u) (VFBoolNE t u)

pattern VBoolIf :: (Val a) -> (Val a) -> (Val a) -> Val a
pattern VBoolIf t u v <- Val _ _ (VFBoolIf t u v)
  where
    VBoolIf t u v = mkVal (valClosed t && valClosed u && valClosed v) (VFBoolIf t u v)

pattern VBytes :: Val a
pattern VBytes <- Val _ _ VFBytes
  where
    VBytes = mkVal True VFBytes

pattern VBytesLit :: ByteString -> Val a
pattern VBytesLit b <- Val _ _ (VFBytesLit b)
  where
    VBytesLit b = mkVal (True) (VFBytesLit b)

pattern VNatural :: Val a
pattern VNatural <- Val _ _ VFNatural
  where
    VNatural = mkVal True VFNatural

pattern VNaturalLit :: Natural -> Val a
pattern VNaturalLit n <- Val _ _ (VFNaturalLit n)
  where
    VNaturalLit n = mkVal (True) (VFNaturalLit n)

pattern VNaturalFold :: (Val a) -> (Val a) -> (Val a) -> (Val a) -> Val a
pattern VNaturalFold a t u v <- Val _ _ (VFNaturalFold a t u v)
  where
    VNaturalFold a t u v = mkVal (valClosed a && valClosed t && valClosed u && valClosed v) (VFNaturalFold a t u v)

pattern VNaturalBuild :: (Val a) -> Val a
pattern VNaturalBuild t <- Val _ _ (VFNaturalBuild t)
  where
    VNaturalBuild t = mkVal (valClosed t) (VFNaturalBuild t)

pattern VNaturalIsZero :: (Val a) -> Val a
pattern VNaturalIsZero t <- Val _ _ (VFNaturalIsZero t)
  where
    VNaturalIsZero t = mkVal (valClosed t) (VFNaturalIsZero t)

pattern VNaturalEven :: (Val a) -> Val a
pattern VNaturalEven t <- Val _ _ (VFNaturalEven t)
  where
    VNaturalEven t = mkVal (valClosed t) (VFNaturalEven t)

pattern VNaturalOdd :: (Val a) -> Val a
pattern VNaturalOdd t <- Val _ _ (VFNaturalOdd t)
  where
    VNaturalOdd t = mkVal (valClosed t) (VFNaturalOdd t)

pattern VNaturalToInteger :: (Val a) -> Val a
pattern VNaturalToInteger t <- Val _ _ (VFNaturalToInteger t)
  where
    VNaturalToInteger t = mkVal (valClosed t) (VFNaturalToInteger t)

pattern VNaturalShow :: (Val a) -> Val a
pattern VNaturalShow t <- Val _ _ (VFNaturalShow t)
  where
    VNaturalShow t = mkVal (valClosed t) (VFNaturalShow t)

pattern VNaturalSubtract :: (Val a) -> (Val a) -> Val a
pattern VNaturalSubtract x y <- Val _ _ (VFNaturalSubtract x y)
  where
    VNaturalSubtract x y = mkVal (valClosed x && valClosed y) (VFNaturalSubtract x y)

pattern VNaturalPlus :: (Val a) -> (Val a) -> Val a
pattern VNaturalPlus t u <- Val _ _ (VFNaturalPlus t u)
  where
    VNaturalPlus t u = mkVal (valClosed t && valClosed u) (VFNaturalPlus t u)

pattern VNaturalTimes :: (Val a) -> (Val a) -> Val a
pattern VNaturalTimes t u <- Val _ _ (VFNaturalTimes t u)
  where
    VNaturalTimes t u = mkVal (valClosed t && valClosed u) (VFNaturalTimes t u)

pattern VInteger :: Val a
pattern VInteger <- Val _ _ VFInteger
  where
    VInteger = mkVal True VFInteger

pattern VIntegerLit :: Integer -> Val a
pattern VIntegerLit n <- Val _ _ (VFIntegerLit n)
  where
    VIntegerLit n = mkVal (True) (VFIntegerLit n)

pattern VIntegerClamp :: (Val a) -> Val a
pattern VIntegerClamp t <- Val _ _ (VFIntegerClamp t)
  where
    VIntegerClamp t = mkVal (valClosed t) (VFIntegerClamp t)

pattern VIntegerNegate :: (Val a) -> Val a
pattern VIntegerNegate t <- Val _ _ (VFIntegerNegate t)
  where
    VIntegerNegate t = mkVal (valClosed t) (VFIntegerNegate t)

pattern VIntegerShow :: (Val a) -> Val a
pattern VIntegerShow t <- Val _ _ (VFIntegerShow t)
  where
    VIntegerShow t = mkVal (valClosed t) (VFIntegerShow t)

pattern VIntegerToDouble :: (Val a) -> Val a
pattern VIntegerToDouble t <- Val _ _ (VFIntegerToDouble t)
  where
    VIntegerToDouble t = mkVal (valClosed t) (VFIntegerToDouble t)

pattern VDouble :: Val a
pattern VDouble <- Val _ _ VFDouble
  where
    VDouble = mkVal True VFDouble

pattern VDoubleLit :: DhallDouble -> Val a
pattern VDoubleLit n <- Val _ _ (VFDoubleLit n)
  where
    VDoubleLit n = mkVal (True) (VFDoubleLit n)

pattern VDoubleShow :: (Val a) -> Val a
pattern VDoubleShow t <- Val _ _ (VFDoubleShow t)
  where
    VDoubleShow t = mkVal (valClosed t) (VFDoubleShow t)

pattern VText :: Val a
pattern VText <- Val _ _ VFText
  where
    VText = mkVal True VFText

pattern VTextLit :: (VChunks a) -> Val a
pattern VTextLit cs <- Val _ _ (VFTextLit cs)
  where
    VTextLit cs = mkVal (vChunksClosed cs) (VFTextLit cs)

pattern VTextAppend :: (Val a) -> (Val a) -> Val a
pattern VTextAppend t u <- Val _ _ (VFTextAppend t u)
  where
    VTextAppend t u = mkVal (valClosed t && valClosed u) (VFTextAppend t u)

pattern VTextShow :: (Val a) -> Val a
pattern VTextShow t <- Val _ _ (VFTextShow t)
  where
    VTextShow t = mkVal (valClosed t) (VFTextShow t)

pattern VTextReplace :: (Val a) -> (Val a) -> (Val a) -> Val a
pattern VTextReplace a b c <- Val _ _ (VFTextReplace a b c)
  where
    VTextReplace a b c = mkVal (valClosed a && valClosed b && valClosed c) (VFTextReplace a b c)

pattern VDate :: Val a
pattern VDate <- Val _ _ VFDate
  where
    VDate = mkVal True VFDate

pattern VDateLiteral :: Time.Day -> Val a
pattern VDateLiteral d <- Val _ _ (VFDateLiteral d)
  where
    VDateLiteral d = mkVal (True) (VFDateLiteral d)

pattern VDateShow :: (Val a) -> Val a
pattern VDateShow t <- Val _ _ (VFDateShow t)
  where
    VDateShow t = mkVal (valClosed t) (VFDateShow t)

pattern VTime :: Val a
pattern VTime <- Val _ _ VFTime
  where
    VTime = mkVal True VFTime

pattern VTimeLiteral :: Word8 -> Word8 -> Word8 -> Integer -> Int -> Val a
pattern VTimeLiteral hh mm ss frac p <- Val _ _ (VFTimeLiteral hh mm ss frac p)
  where
    VTimeLiteral hh mm ss frac p = mkVal (True) (VFTimeLiteral hh mm ss frac p)

pattern VTimeShow :: (Val a) -> Val a
pattern VTimeShow t <- Val _ _ (VFTimeShow t)
  where
    VTimeShow t = mkVal (valClosed t) (VFTimeShow t)

pattern VTimeZone :: Val a
pattern VTimeZone <- Val _ _ VFTimeZone
  where
    VTimeZone = mkVal True VFTimeZone

pattern VTimeZoneLiteral :: Time.TimeZone -> Val a
pattern VTimeZoneLiteral z <- Val _ _ (VFTimeZoneLiteral z)
  where
    VTimeZoneLiteral z = mkVal (True) (VFTimeZoneLiteral z)

pattern VTimeZoneShow :: (Val a) -> Val a
pattern VTimeZoneShow t <- Val _ _ (VFTimeZoneShow t)
  where
    VTimeZoneShow t = mkVal (valClosed t) (VFTimeZoneShow t)

pattern VList :: (Val a) -> Val a
pattern VList t <- Val _ _ (VFList t)
  where
    VList t = mkVal (valClosed t) (VFList t)

pattern VListLit :: (Maybe (Val a)) -> (Seq (Val a)) -> Val a
pattern VListLit ma ts <- Val _ _ (VFListLit ma ts)
  where
    VListLit ma ts = mkVal (maybe True valClosed ma && all valClosed ts) (VFListLit ma ts)

pattern VListAppend :: (Val a) -> (Val a) -> Val a
pattern VListAppend t u <- Val _ _ (VFListAppend t u)
  where
    VListAppend t u = mkVal (valClosed t && valClosed u) (VFListAppend t u)

pattern VListBuild :: (Val a) -> (Val a) -> Val a
pattern VListBuild a t <- Val _ _ (VFListBuild a t)
  where
    VListBuild a t = mkVal (valClosed a && valClosed t) (VFListBuild a t)

pattern VListFold :: (Val a) -> (Val a) -> (Val a) -> (Val a) -> (Val a) -> Val a
pattern VListFold a l t u v <- Val _ _ (VFListFold a l t u v)
  where
    VListFold a l t u v = mkVal (valClosed a && valClosed l && valClosed t && valClosed u && valClosed v) (VFListFold a l t u v)

pattern VListLength :: (Val a) -> (Val a) -> Val a
pattern VListLength a t <- Val _ _ (VFListLength a t)
  where
    VListLength a t = mkVal (valClosed a && valClosed t) (VFListLength a t)

pattern VListHead :: (Val a) -> (Val a) -> Val a
pattern VListHead a t <- Val _ _ (VFListHead a t)
  where
    VListHead a t = mkVal (valClosed a && valClosed t) (VFListHead a t)

pattern VListLast :: (Val a) -> (Val a) -> Val a
pattern VListLast a t <- Val _ _ (VFListLast a t)
  where
    VListLast a t = mkVal (valClosed a && valClosed t) (VFListLast a t)

pattern VListIndexed :: (Val a) -> (Val a) -> Val a
pattern VListIndexed a t <- Val _ _ (VFListIndexed a t)
  where
    VListIndexed a t = mkVal (valClosed a && valClosed t) (VFListIndexed a t)

pattern VListReverse :: (Val a) -> (Val a) -> Val a
pattern VListReverse a t <- Val _ _ (VFListReverse a t)
  where
    VListReverse a t = mkVal (valClosed a && valClosed t) (VFListReverse a t)

pattern VOptional :: (Val a) -> Val a
pattern VOptional t <- Val _ _ (VFOptional t)
  where
    VOptional t = mkVal (valClosed t) (VFOptional t)

pattern VSome :: (Val a) -> Val a
pattern VSome t <- Val _ _ (VFSome t)
  where
    VSome t = mkVal (valClosed t) (VFSome t)

pattern VNone :: (Val a) -> Val a
pattern VNone t <- Val _ _ (VFNone t)
  where
    VNone t = mkVal (valClosed t) (VFNone t)

pattern VRecord :: (Map Text (Val a)) -> Val a
pattern VRecord m <- Val _ _ (VFRecord m)
  where
    VRecord m = mkVal (all valClosed m) (VFRecord m)

pattern VRecordLit :: (Map Text (Val a)) -> Val a
pattern VRecordLit m <- Val _ _ (VFRecordLit m)
  where
    VRecordLit m = mkVal (all valClosed m) (VFRecordLit m)

pattern VUnion :: (Map Text (Maybe (Val a))) -> Val a
pattern VUnion m <- Val _ _ (VFUnion m)
  where
    VUnion m = mkVal (all (all valClosed) m) (VFUnion m)

pattern VCombine :: (Maybe Text) -> (Val a) -> (Val a) -> Val a
pattern VCombine mk t u <- Val _ _ (VFCombine mk t u)
  where
    VCombine mk t u = mkVal (valClosed t && valClosed u) (VFCombine mk t u)

pattern VCombineTypes :: (Val a) -> (Val a) -> Val a
pattern VCombineTypes t u <- Val _ _ (VFCombineTypes t u)
  where
    VCombineTypes t u = mkVal (valClosed t && valClosed u) (VFCombineTypes t u)

pattern VPrefer :: (Val a) -> (Val a) -> Val a
pattern VPrefer t u <- Val _ _ (VFPrefer t u)
  where
    VPrefer t u = mkVal (valClosed t && valClosed u) (VFPrefer t u)

pattern VMerge :: (Val a) -> (Val a) -> (Maybe (Val a)) -> Val a
pattern VMerge t u ma <- Val _ _ (VFMerge t u ma)
  where
    VMerge t u ma = mkVal (valClosed t && valClosed u && maybe True valClosed ma) (VFMerge t u ma)

pattern VToMap :: (Val a) -> (Maybe (Val a)) -> Val a
pattern VToMap t ma <- Val _ _ (VFToMap t ma)
  where
    VToMap t ma = mkVal (valClosed t && maybe True valClosed ma) (VFToMap t ma)

pattern VShowConstructor :: (Val a) -> Val a
pattern VShowConstructor t <- Val _ _ (VFShowConstructor t)
  where
    VShowConstructor t = mkVal (valClosed t) (VFShowConstructor t)

pattern VField :: (Val a) -> Text -> Val a
pattern VField t k <- Val _ _ (VFField t k)
  where
    VField t k = mkVal (valClosed t) (VFField t k)

pattern VInject :: (Map Text (Maybe (Val a))) -> Text -> (Maybe (Val a)) -> Val a
pattern VInject m k mt <- Val _ _ (VFInject m k mt)
  where
    VInject m k mt = mkVal (all (all valClosed) m && maybe True valClosed mt) (VFInject m k mt)

pattern VProject :: (Val a) -> (Either (Set Text) (Val a)) -> Val a
pattern VProject t p <- Val _ _ (VFProject t p)
  where
    VProject t p = mkVal (valClosed t && either (const True) valClosed p) (VFProject t p)

pattern VAssert :: (Val a) -> Val a
pattern VAssert t <- Val _ _ (VFAssert t)
  where
    VAssert t = mkVal (valClosed t) (VFAssert t)

pattern VEquivalent :: (Val a) -> (Val a) -> Val a
pattern VEquivalent t u <- Val _ _ (VFEquivalent t u)
  where
    VEquivalent t u = mkVal (valClosed t && valClosed u) (VFEquivalent t u)

pattern VWith :: (Val a) -> (NonEmpty WithComponent) -> (Val a) -> Val a
pattern VWith e ks v <- Val _ _ (VFWith e ks v)
  where
    VWith e ks v = mkVal (valClosed e && valClosed v) (VFWith e ks v)

pattern VEmbed :: a -> Val a
pattern VEmbed x <- Val _ _ (VFEmbed x)
  where
    VEmbed x = mkVal (True) (VFEmbed x)

{-# COMPLETE VConst, VVar, VPrimVar, VApp, VLam, VHLam, VPi, VHPi, VPiND, VBool, VBoolLit, VBoolAnd, VBoolOr, VBoolEQ, VBoolNE, VBoolIf, VBytes, VBytesLit, VNatural, VNaturalLit, VNaturalFold, VNaturalBuild, VNaturalIsZero, VNaturalEven, VNaturalOdd, VNaturalToInteger, VNaturalShow, VNaturalSubtract, VNaturalPlus, VNaturalTimes, VInteger, VIntegerLit, VIntegerClamp, VIntegerNegate, VIntegerShow, VIntegerToDouble, VDouble, VDoubleLit, VDoubleShow, VText, VTextLit, VTextAppend, VTextShow, VTextReplace, VDate, VDateLiteral, VDateShow, VTime, VTimeLiteral, VTimeShow, VTimeZone, VTimeZoneLiteral, VTimeZoneShow, VList, VListLit, VListAppend, VListBuild, VListFold, VListLength, VListHead, VListLast, VListIndexed, VListReverse, VOptional, VSome, VNone, VRecord, VRecordLit, VUnion, VCombine, VCombineTypes, VPrefer, VMerge, VToMap, VShowConstructor, VField, VInject, VProject, VAssert, VEquivalent, VWith, VEmbed #-}

pattern VPrim :: (Val a -> Val a) -> Val a
pattern VPrim f = VHLam Prim f

toVHPi :: Eq a => Val a -> Maybe (Text, Val a, Val a -> Val a)
toVHPi (VPi a b@(Closure x _ _ _)) = Just (x, a, instantiate b)
toVHPi (VPiND x a b)               = Just (x, a, \_ -> b)
toVHPi (VHPi x a b)                = Just (x, a, b)
toVHPi  _                          = Nothing
{-# INLINABLE toVHPi #-}

(~>) :: Val a -> Val a -> Val a
(~>) a b = VPiND "_" a b
{-# INLINE (~>) #-}

infixr 5 ~>

countEnvironment :: Text -> Environment a -> Int
countEnvironment x = go (0 :: Int)
  where
    go !acc Empty               = acc
    go  acc (Skip env x' _)     = go (if x == x' then acc + 1 else acc) env
    go  acc (Extend env x' _ _) = go (if x == x' then acc + 1 else acc) env

-- | Instantiate a user-lambda closure.
--
-- The argument is stored in 'Extend' without an extra bang here. Forcing
-- belongs in 'vApp' (call-site @!u@), not here: 'Extend' is lazy in the
-- value, so a @forceWHNF u@ passed into 'instantiate' is only another thunk.
-- Constructor-like lambdas such as @λ(x : List a) → λ(as : List a) → [x] # as@
-- have a non-cheap binder, so @x@ stays a thunk in the list spine. A blanket
-- @!u@ here would reintroduce the Iterate O(n²) blow-up for that encoding.
--
-- Bodies that inspect the argument (arithmetic, @conv@, @quote@, …) still
-- force it when they pattern-match. 'instantiate' does not check whether the
-- variable occurs in the body (that would be slow); non-dependent @Pi@ types
-- are classified once as 'VHPi' via 'boundVarOccurs'.
instantiate :: Eq a => Closure a -> Val a -> Val a
instantiate (Closure x env t bid) !u =
    let key = EvalKey (unNodeId bid) (unEnvId (envId env)) x (unValId (valId u))
    in  case lookupEvalMemo key of
            Just v  -> v
            Nothing -> insertEvalMemo key (evalImpl (extend env x u) t)
{-# INLINE instantiate #-}

-- Out-of-env variables have negative de Bruijn levels.
-- A leftover index of 0 at Empty is a predefined function when the name is
-- registered below (including after walking past a shadowing binder).
vVar :: Eq a => Environment a -> Var -> Val a
vVar env0 (V x i0) = go env0 i0
  where
    go (Extend env x' v _) i
        | x == x' =
            if i == 0 then v else go env (i - 1)
        | otherwise =
            go env i
    go (Skip env x' _) i
        | x == x' =
            if i == 0 then VVar x (countEnvironment x env) else go env (i - 1)
        | otherwise =
            go env i
    go Empty 0
        | Just builtin <- HashMap.lookup x unboundBuiltinValues =
            builtin
    go Empty i =
        VVar x (negate i - 1)

-- | Apply a function value.
--
-- The function is forced (@!t@) so we can see whether it is a lambda, a
-- builtin, or a neutral.
--
-- [Lazy 'VHLam'] Builtins and other Haskell-encoded functions. Eliminators
-- ('List/length', 'Natural/isZero', …) force the argument themselves by
-- pattern-matching. Constructors must not: 'List/build' synthesizes
--
-- @
-- cons = \\x -> \\as -> vListAppend (VListLit Nothing (pure x)) as
-- @
--
-- which is the same as the β-normal @ [x] # xs @, whose source 'ListLit' is
-- built with lazy @fmap eval@. A bang on @u@ at this call site would force
-- @x@ *before* that Haskell function ran (a lazy @\\ ~x ->@ cannot undo it).
--
-- After delayed β-normalization of unhashed Code imports (#2808), Prelude
-- @List/iterate@ is inlined still containing 'List/build'. Strict 'VHLam'
-- application then forced every @Natural/fold@ while building the spine, so
-- @List/length (iterate n …)@ became O(n²) and OOMed the Iterate benchmark
-- at @n = 300000@ (#2831).
--
-- ['VLam' and 'whnfCheapType'] User lambdas go through 'instantiate'. If the
-- binder type is 'whnfCheapType' ('Natural', 'Bool', function types, a flat
-- record of those, …), the argument is forced to WHNF *in this function*
-- (@let !u' = u@) before 'instantiate'. That is the original @vApp !t !u@
-- behaviour, gated so that @s (s (s z))@ in ChurchEval is a tight loop of
-- 'VNaturalLit's rather than a tower of @eval@ thunks. A @forceWHNF@ stored
-- in lazy 'Extend' does *not* suffice: 'vApp' would still return without
-- entering the inner application. Nested record types are not cheap (see
-- 'whnfCheapTypeNested'): classifying @λ(base : baseSchema.Type)@ must not
-- walk the k8s product on every apply (large4).
--
-- List binders stay lazy, so a user Church @cons@ @λ(x : List a) → [x] # xs@
-- does not force @x@. Do not use 'whnfCheapType' to bang 'VHLam' arguments:
-- 'List/build''s @cons@ is 'Typed' at the element type, which may be
-- 'Natural', but must still park the element without forcing it.
vApp :: Eq a => Val a -> Val a -> Val a
vApp !t u =
    case t of
        VLam ty t'
            | whnfCheapType ty -> let !u' = u in instantiate t' u'
            | otherwise        -> instantiate t' u
        VHLam _ t' -> t' u
        t'        -> VApp t' u
{-# INLINE vApp #-}

vPrefer :: Eq a => Environment a -> Val a -> Val a -> Val a
vPrefer env t u =
    case (t, u) of
        (VRecordLit m, u') | null m ->
            u'
        (t', VRecordLit m) | null m ->
            t'
        (VRecordLit m, VRecordLit m') ->
            VRecordLit (Map.union m' m)
        (t', u') | convImpl env t' u' ->
            t'
        (t', u') ->
            VPrefer t' u'
{-# INLINE vPrefer #-}

vCombine :: Maybe Text -> Val a -> Val a -> Val a
vCombine mk t u =
    case (t, u) of
        (VRecordLit m, u') | null m ->
            u'
        (t', VRecordLit m) | null m ->
            t'
        (VRecordLit m, VRecordLit m') ->
            VRecordLit (Map.unionWith (vCombine Nothing) m m')
        (t', u') ->
            VCombine mk t' u'

vCombineTypes :: Val a -> Val a -> Val a
vCombineTypes t u =
    case (t, u) of
        (VRecord m, u') | null m ->
            u'
        (t', VRecord m) | null m ->
            t'
        (VRecord m, VRecord m') ->
            VRecord (Map.unionWith vCombineTypes m m')
        (t', u') ->
            VCombineTypes t' u'

vListAppend :: Val a -> Val a -> Val a
vListAppend t u =
    case (t, u) of
        (VListLit _ xs, u') | null xs ->
            u'
        (t', VListLit _ ys) | null ys ->
            t'
        (VListLit t' xs, VListLit _ ys) ->
            VListLit t' (xs <> ys)
        (t', u') ->
            VListAppend t' u'
{-# INLINE vListAppend #-}

vNaturalPlus :: Val a -> Val a -> Val a
vNaturalPlus t u =
    case (t, u) of
        (VNaturalLit 0, u') ->
            u'
        (t', VNaturalLit 0) ->
            t'
        (VNaturalLit m, VNaturalLit n) ->
            VNaturalLit (m + n)
        (t', u') ->
            VNaturalPlus t' u'
{-# INLINE vNaturalPlus #-}

vField :: Val a -> Text -> Val a
vField t0 k = go t0
  where
    go = \case
        VUnion m -> case Map.lookup k m of
            Just (Just _) -> VPrim $ \ ~u -> VInject m k (Just u)
            Just Nothing  -> VInject m k Nothing
            _             -> error errorMsg
        VRecordLit m
            | Just v <- Map.lookup k m -> v
            | otherwise -> error errorMsg
        VProject t _ -> go t
        VPrefer (VRecordLit m) r -> case Map.lookup k m of
            Just v -> VField (VPrefer (singletonVRecordLit v) r) k
            Nothing -> go r
        VPrefer l (VRecordLit m) -> case Map.lookup k m of
            Just v -> v
            Nothing -> go l
        VCombine mk (VRecordLit m) r -> case Map.lookup k m of
            Just v -> VField (VCombine mk (singletonVRecordLit v) r) k
            Nothing -> go r
        VCombine mk l (VRecordLit m) -> case Map.lookup k m of
            Just v -> VField (VCombine mk l (singletonVRecordLit v)) k
            Nothing -> go l
        t -> VField t k

    singletonVRecordLit v = VRecordLit (Map.singleton k v)
{-# INLINE vField #-}

vTextReplace :: Text -> Val a -> Text -> VChunks a
vTextReplace needle replacement haystack = go haystack
  where
    go t
        | Text.null suffix = VChunks [] t
        | otherwise =
            let remainder = Text.drop (Text.length needle) suffix

                rest = go remainder

            in  case replacement of
                    VTextLit replacementChunks ->
                        VChunks [] prefix <> replacementChunks <> rest
                    _ ->
                        VChunks [(prefix, replacement)] "" <> rest
      where
        (prefix, suffix) = Text.breakOn needle t

vProjectByFields :: Eq a => Environment a -> Val a -> Set Text -> Val a
vProjectByFields env t ks =
    if null ks
        then VRecordLit mempty
        else case t of
            VRecordLit kvs ->
                let kvs' = Map.restrictKeys kvs (Dhall.Set.toSet ks)
                in  VRecordLit kvs'
            VProject t' _ ->
                vProjectByFields env t' ks
            VPrefer l (VRecordLit kvs) ->
                let ksSet = Dhall.Set.toSet ks

                    kvs' = Map.restrictKeys kvs ksSet

                    ks' =
                        Dhall.Set.fromSet
                            (Data.Set.difference ksSet (Map.keysSet kvs'))

                in  vPrefer env (vProjectByFields env l ks') (VRecordLit kvs')
            t' ->
                VProject t' (Left ks)

vWith :: Val a -> NonEmpty WithComponent -> Val a -> Val a
vWith (VRecordLit kvs) (WithLabel k  :| []     ) v = VRecordLit (Map.insert k  v  kvs)
vWith (VRecordLit kvs) (WithLabel k₀ :| k₁ : ks) v = VRecordLit (Map.insert k₀ e₂ kvs)
  where
    e₁ =
        case Map.lookup k₀ kvs of
            Nothing  -> VRecordLit mempty
            Just e₁' -> e₁'

    e₂ = vWith e₁ (k₁ :| ks) v
vWith (VNone _T) (WithQuestion :| _      ) _ = VNone _T
vWith (VSome  _) (WithQuestion :| []     ) v = VSome v
vWith (VSome  t) (WithQuestion :| k₁ : ks) v = VSome (vWith t (k₁ :| ks) v)
vWith e₀ ks v₀ = VWith e₀ ks v₀

-- | Val-level counterpart of @Dhall.Normalize.boundedType@.
--
-- Decides the 'Natural/fold' *shortcut* (see the @NaturalFold@ case of
-- 'eval'): conversion @succ acc ≡ acc@ is only cheap on small types.
--
-- * 'True' — Natural/Integer/Text/Bool/Double, Optional of those,
--   records/unions of those. The fold may stop early via @succ acc ≡ acc@.
-- * 'False' — lists, functions, and mixed records. The fold still uses
--   'forceAccWHNF' each step, but does not run 'conv'.
boundedType :: Val a -> Bool
boundedType = \case
    VBool       -> True
    VNatural    -> True
    VInteger    -> True
    VDouble     -> True
    VText       -> True
    VList _     -> False
    VOptional t -> boundedType t
    VRecord m   -> all boundedType m
    VUnion m    -> all (all boundedType) m
    _           -> False

-- | Types whose WHNF is a lambda or a small primitive. Used to decide
-- whether 'vApp' bangs a 'VLam' argument (not used by 'Natural/fold').
--
-- Functions are included: WHNF is 'VLam'/'VHLam'. Lists are not: a user
-- Church @cons@ @λ(x : List a) → [x] # xs@ must park @x@ without forcing
-- it.
--
-- Records and unions are cheap only one field-level deep: every immediate
-- field or payload must be 'whnfCheapTypeNested'. That bangs ListBench
-- @λ(p : { next : Natural, rest : list → list })@ without walking nested
-- products such as large4 @baseSchema.Type@ on every apply.
whnfCheapType :: Val a -> Bool
whnfCheapType = \case
    VRecord m   -> all whnfCheapTypeNested m
    VUnion m    -> all (all whnfCheapTypeNested) m
    VOptional t -> whnfCheapType t
    t           -> whnfCheapTypeNested t

-- | Leaves of 'whnfCheapType': primitives, function types, and 'Optional' of
-- those. A nested 'VRecord'/'VUnion' is not cheap — do not recurse.
whnfCheapTypeNested :: Val a -> Bool
whnfCheapTypeNested = \case
    VBool       -> True
    VNatural    -> True
    VInteger    -> True
    VDouble     -> True
    VText       -> True
    VPi _ _     -> True
    VHPi _ _ _  -> True
    VOptional t -> whnfCheapTypeNested t
    _           -> False
{-# INLINE whnfCheapTypeNested #-}

-- | Force a 'Natural/fold' accumulator far enough that the next step does
-- not see a thunk tower, without forcing list *elements* or list-typed
-- record fields.
--
-- Matching a 'VListLit' already bangs the 'Seq' spine (@<>@ is O(log n)).
-- Matching a 'VRecordLit' does *not* force fields: 'Dhall.Map' stores an
-- unbanged inner 'Data.Map'. This walks fields whose *type* is not a list
-- (ListBench @next : Natural@, @rest : list → list@). List-typed fields are
-- left as thunks: IterateAlt's @next : List Natural@ is not needed by
-- @List/length@ of the outer spine, and forcing it built lists of length
-- @1..n@. Does not enter 'VLam'/'VHLam' bodies.
--
-- The first argument is the accumulator type from 'Natural/fold' (so we can
-- skip list fields without matching the value, which would force a 'Seq').
--
-- Not inlined into 'eval': the walker is a 'VRecordLit' fold, and inlining it
-- into the 'NaturalFold' branch bloated 'eval' enough to regress large5
-- (128 copies of a large record, no 'Natural/fold' in the tree).
forceAccWHNF :: Val a -> Val a -> Val a
forceAccWHNF accTy !v =
    case (accTy, v) of
        (VRecord tm, VRecordLit vm) ->
            let !_ = Map.foldMapWithKey (seqField tm) vm
            in  v
        (VOptional t, VSome x) ->
            let !x' = forceAccWHNF t x
            in  VSome x'
        _ ->
            v
{-# NOINLINE forceAccWHNF #-}

-- | @()@ if this record field should stay a thunk ('VList'); otherwise
-- force it with 'forceAccWHNF'. Must not pattern-match a list field: that
-- would bang the 'Seq' spine.
seqField :: Map Text (Val a) -> Text -> Val a -> ()
seqField tm k x =
    case Map.lookup k tm of
        Just fty | skipListField fty -> ()
        Just fty -> forceAccWHNF fty x `seq` ()
        Nothing  -> ()

skipListField :: Val a -> Bool
skipListField = \case
    VList _     -> True
    VOptional t -> skipListField t
    _           -> False
{-# INLINE skipListField #-}

eval :: forall a. Eq a => Environment a -> Expr Void a -> Val a
eval env t0 = withEvalRun defaultEvalConfig (evalImpl env t0)

evalInRun :: forall a. Eq a => Environment a -> Expr Void a -> Val a
evalInRun = evalImpl

-- | Like 'eval' but uses the ambient 'withEvalRun' (no cache reset).
evalImpl :: forall a. Eq a => Environment a -> Expr Void a -> Val a
evalImpl !env t0 =
    case t0 of
        Const k ->
            VConst k
        Var v ->
            vVar env v
        Lam _ (FunctionBinding { functionBindingVariable = x, functionBindingAnnotation = a }) t ->
            VLam (evalImpl env a) (makeClosure x env t)
        Pi _ x a b ->
            -- Classify once: non-dependent Pis become first-order VPiND so
            -- applying the type during type-checking does not force an unused
            -- argument (e.g. @List a → List b@).  Dependent Pis stay as VPi
            -- closures with interned bodies.
            let !a' = evalImpl env a
            in  if boundVarOccurs x 0 b
                then VPi a' (makeClosure x env b)
                else VPiND x a' (evalImpl (skip env x) b)
        App t u ->
            vApp (evalImpl env t) (evalImpl env u)
        Let (Binding _ x _ _mA _ a) b ->
            -- `extend` is lazy in the bound `Val`, so an unused let is not
            -- evaluated. `!env'` only forces the environment spine.
            let !env' = extend env x (evalImpl env a)
            in  evalImpl env' b
        Annot t _ ->
            evalImpl env t
        Bool ->
            VBool
        BoolLit b ->
            VBoolLit b
        BoolAnd t u ->
            case (evalImpl env t, evalImpl env u) of
                (VBoolLit True, u')       -> u'
                (VBoolLit False, _)       -> VBoolLit False
                (t', VBoolLit True)       -> t'
                (_ , VBoolLit False)      -> VBoolLit False
                (t', u') | convImpl env t' u' -> t'
                (t', u')                  -> VBoolAnd t' u'
        BoolOr t u ->
            case (evalImpl env t, evalImpl env u) of
                (VBoolLit False, u')      -> u'
                (VBoolLit True, _)        -> VBoolLit True
                (t', VBoolLit False)      -> t'
                (_ , VBoolLit True)       -> VBoolLit True
                (t', u') | convImpl env t' u' -> t'
                (t', u')                  -> VBoolOr t' u'
        BoolEQ t u ->
            case (evalImpl env t, evalImpl env u) of
                (VBoolLit True, u')       -> u'
                (t', VBoolLit True)       -> t'
                (t', u') | convImpl env t' u' -> VBoolLit True
                (t', u')                  -> VBoolEQ t' u'
        BoolNE t u ->
            case (evalImpl env t, evalImpl env u) of
                (VBoolLit False, u')      -> u'
                (t', VBoolLit False)      -> t'
                (t', u') | convImpl env t' u' -> VBoolLit False
                (t', u')                  -> VBoolNE t' u'
        BoolIf b t f ->
            case (evalImpl env b, evalImpl env t, evalImpl env f) of
                (VBoolLit True,  t', _ )            -> t'
                (VBoolLit False, _ , f')            -> f'
                (b', VBoolLit True, VBoolLit False) -> b'
                (_, t', f') | convImpl env t' f'        -> t'
                (b', t', f')                        -> VBoolIf b' t' f'
        Bytes ->
            VBytes
        BytesLit b ->
            VBytesLit b
        Natural ->
            VNatural
        NaturalLit n ->
            VNaturalLit n
        NaturalPlus t u ->
            vNaturalPlus (evalImpl env t) (evalImpl env u)
        NaturalTimes t u ->
            case (evalImpl env t, evalImpl env u) of
                (VNaturalLit 1, u'           ) -> u'
                (t'           , VNaturalLit 1) -> t'
                (VNaturalLit 0, _            ) -> VNaturalLit 0
                (_            , VNaturalLit 0) -> VNaturalLit 0
                (VNaturalLit m, VNaturalLit n) -> VNaturalLit (m * n)
                (t'           , u'           ) -> VNaturalTimes t' u'
        Integer ->
            VInteger
        IntegerLit n ->
            VIntegerLit n
        Double ->
            VDouble
        DoubleLit n ->
            VDoubleLit n
        Text ->
            VText
        TextLit cs ->
            case evalChunks cs of
                VChunks [("", t)] "" -> t
                vcs                  -> VTextLit vcs
        TextAppend t u ->
            evalImpl env (TextLit (Chunks [("", t), ("", u)] ""))
        Date ->
            VDate
        DateLiteral d ->
            VDateLiteral d
        Time ->
            VTime
        TimeLiteral hh mm ss frac p ->
            VTimeLiteral hh mm ss frac p
        TimeZone ->
            VTimeZone
        TimeZoneLiteral z ->
            VTimeZoneLiteral z
        List ->
            VPrim VList
        ListLit ma ts ->
            VListLit (fmap (evalImpl env) ma) (fmap (evalImpl env) ts)
        ListAppend t u ->
            vListAppend (evalImpl env t) (evalImpl env u)
        Optional ->
            VPrim VOptional
        Some t ->
            VSome (evalImpl env t)
        None ->
            VPrim $ \ ~a -> VNone a
        Record kts ->
            VRecord (Map.sort (evalImpl env . recordFieldValue <$> kts))
        RecordLit kts ->
            VRecordLit (Map.sort (evalImpl env . recordFieldValue <$> kts))
        Union kts ->
            VUnion (Map.sort (fmap (fmap (evalImpl env)) kts))
        Combine _ mk t u ->
            vCombine mk (evalImpl env t) (evalImpl env u)
        CombineTypes _ t u ->
            vCombineTypes (evalImpl env t) (evalImpl env u)
        Prefer _ _ t u ->
            vPrefer env (evalImpl env t) (evalImpl env u)
        RecordCompletion t u ->
            evalImpl env (Annot (Prefer mempty PreferFromCompletion (Field t def) u) (Field t typ))
          where
            def = Syntax.makeFieldSelection "default"
            typ = Syntax.makeFieldSelection "Type"
        Merge x y ma ->
            case (evalImpl env x, evalImpl env y, fmap (evalImpl env) ma) of
                (VRecordLit m, VInject _ k mt, _)
                    | Just f <- Map.lookup k m -> maybe f (vApp f) mt
                    | otherwise                -> error errorMsg
                (VRecordLit m, VSome t, _)
                    | Just f <- Map.lookup "Some" m -> vApp f t
                    | otherwise                     -> error errorMsg
                (VRecordLit m, VNone _, _)
                    | Just t <- Map.lookup "None" m -> t
                    | otherwise                     -> error errorMsg
                (x', y', ma') -> VMerge x' y' ma'
        ToMap x ma ->
            case (evalImpl env x, fmap (evalImpl env) ma) of
                (VRecordLit m, ma'@(Just _)) | null m ->
                    VListLit ma' Sequence.empty
                (VRecordLit m, _) ->
                    let entry (k, v) =
                            VRecordLit
                                (Map.unorderedFromList
                                    [ ("mapKey", VTextLit $ VChunks [] k)
                                    , ("mapValue", v)
                                    ]
                                )

                        s = (Sequence.fromList . map entry . Map.toAscList) m

                    in  VListLit Nothing s
                (x', ma') ->
                    VToMap x' ma'
        ShowConstructor x ->
            case evalImpl env x of
                VInject m k _
                    | Just _ <- Map.lookup k m -> VTextLit (VChunks [] k)
                    | otherwise                -> error errorMsg
                VSome _ -> VTextLit (VChunks [] "Some")
                VNone _ -> VTextLit (VChunks [] "None")
                x' -> VShowConstructor x'
        Field t (Syntax.fieldSelectionLabel -> k) ->
            vField (evalImpl env t) k
        Project t (Left ks) ->
            vProjectByFields env (evalImpl env t) (Dhall.Set.sort (Dhall.Set.fromList ks))
        Project t (Right e) ->
            case evalImpl env e of
                VRecord kts ->
                    vProjectByFields env (evalImpl env t) (Dhall.Set.fromSet (Map.keysSet kts))
                e' ->
                    VProject (evalImpl env t) (Right e')
        Assert t ->
            VAssert (evalImpl env t)
        Equivalent _ t u ->
            VEquivalent (evalImpl env t) (evalImpl env u)
        With e₀ ks v ->
            vWith (evalImpl env e₀) ks (evalImpl env v)
        Note _ e ->
            evalImpl env e
        ImportAlt t _ ->
            evalImpl env t
        Embed a ->
            VEmbed a
  where
    evalChunks :: Chunks Void a -> VChunks a
    evalChunks (Chunks xys z) = foldr' cons nil xys
      where
        cons (x, t) vcs =
            case evalImpl env t of
                VTextLit vcs' -> VChunks [] x <> vcs' <> vcs
                t'            -> VChunks [(x, t')] mempty <> vcs

        nil = VChunks [] z
    {-# INLINE evalChunks #-}

eqListBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqListBy f = go
  where
    go (x:xs) (y:ys) | f x y = go xs ys
    go [] [] = True
    go _  _  = False
{-# INLINE eqListBy #-}

eqMapsBy :: Ord k => (v -> v -> Bool) -> Map k v -> Map k v -> Bool
eqMapsBy f mL mR =
    Map.size mL == Map.size mR
    && eqListBy eq (Map.toAscList mL) (Map.toAscList mR)
  where
    eq (kL, vL) (kR, vR) = kL == kR && f vL vR
{-# INLINE eqMapsBy #-}

eqMaybeBy :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
eqMaybeBy f = go
  where
    go (Just x) (Just y) = f x y
    go Nothing  Nothing  = True
    go _        _        = False
{-# INLINE eqMaybeBy #-}

-- | Utility that powers the @Text/show@ built-in
textShow :: Text -> Text
textShow text = "\"" <> Text.concatMap f text <> "\""
  where
    f '"'  = "\\\""
    f '$'  = "\\u0024"
    f '\\' = "\\\\"
    f '\b' = "\\b"
    f '\n' = "\\n"
    f '\r' = "\\r"
    f '\t' = "\\t"
    f '\f' = "\\f"
    f c | c <= '\x1F' = Text.pack (Printf.printf "\\u%04x" (Data.Char.ord c))
        | otherwise   = Text.singleton c

-- | Utility that powers the @Date/show@ built-in
dateShow :: Day -> Text
dateShow = Text.pack . Time.formatTime Time.defaultTimeLocale "%0Y-%m-%d"

-- | Utility that powers the @Time/show@ built-in
timeShow :: Word8 -> Word8 -> Word8 -> Integer -> Int -> Text
timeShow hh mm ss fraction precision =
    Text.pack (Printf.printf "%02d:%02d:%02d" hh mm ss <> suffix)
  where
    suffix
        | precision == 0 = ""
        | otherwise      = Printf.printf ".%0*d" precision fraction

-- | Utility that powers the @TimeZone/show@ built-in
timezoneShow :: TimeZone -> Text
timezoneShow = Text.pack . Time.formatTime Time.defaultTimeLocale "%Ez"

conv :: forall a. Eq a => Environment a -> Val a -> Val a -> Bool
conv env0 t0 t0' = withEvalRun defaultEvalConfig (convImpl env0 t0 t0')

-- | Conversion using the ambient evaluation run (no cache reset).
convInRun :: forall a. Eq a => Environment a -> Val a -> Val a -> Bool
convInRun = convImpl

convImpl :: forall a. Eq a => Environment a -> Val a -> Val a -> Bool
convImpl !env t0 t0'
    -- 'ValId' 0 is the stub id used when 'assignIds' is off; never treat it as
    -- pointer equality. Distinct values must not share a real id.
    | let vid = valId t0
    , vid /= ValId 0
    , vid == valId t0' =
        True
    | valClosed t0 && valClosed t0'
    , Just cached <- lookupConvMemo (valId t0) (valId t0') =
        cached
    | otherwise =
        let result = convBody env t0 t0'
        in  if valClosed t0 && valClosed t0'
            then insertConvMemo (valId t0) (valId t0') result
            else result

convBody :: forall a. Eq a => Environment a -> Val a -> Val a -> Bool
convBody !env t0 t0' =
    case (t0, t0') of
        (VConst k, VConst k') ->
            k == k'
        (VVar x i, VVar x' i') ->
            x == x' && i == i'
        (VLam _ (freshClosure -> (x, v, t)), VLam _ t' ) ->
            convSkip x (instantiate t v) (instantiate t' v)
        (VLam _ (freshClosure -> (x, v, t)), VHLam _ t') ->
            convSkip x (instantiate t v) (t' v)
        (VLam _ (freshClosure -> (x, v, t)), t'        ) ->
            convSkip x (instantiate t v) (vApp t' v)
        (VHLam _ t, VLam _ (freshClosure -> (x, v, t'))) ->
            convSkip x (t v) (instantiate t' v)
        (VHLam _ t, VHLam _ t'                    ) ->
            let (x, v) = fresh "x" in convSkip x (t v) (t' v)
        (VHLam _ t, t'                            ) ->
            let (x, v) = fresh "x" in convSkip x (t v) (vApp t' v)
        (t, VLam _ (freshClosure -> (x, v, t'))) ->
            convSkip x (vApp t v) (instantiate t' v)
        (t, VHLam _ t'  ) ->
            let (x, v) = fresh "x" in convSkip x (vApp t v) (t' v)
        (VApp t u, VApp t' u') ->
            convImpl env t t' && convImpl env u u'
        (VPi a b, VPi a' (freshClosure -> (x, v, b'))) ->
            convImpl env a a' && convSkip x (instantiate b v) (instantiate b' v)
        (VPi a b, VHPi (fresh -> (x, v)) a' b') ->
            convImpl env a a' && convSkip x (instantiate b v) (b' v)
        (VHPi _ a b, VPi a' (freshClosure -> (x, v, b'))) ->
            convImpl env a a' && convSkip x (b v) (instantiate b' v)
        (VPi a b, VPiND x a' b') ->
            convImpl env a a' && convSkip x (instantiate b v) b'
              where (v) = snd (fresh x)
        (VPiND x a b, VPi a' (freshClosure -> (_, v, b'))) ->
            convImpl env a a' && convSkip x b (instantiate b' v)
        (VPiND _ a b, VPiND x a' b') ->
            convImpl env a a' && convSkip x b b'
        (VPiND x a b, VHPi (fresh -> (_, v)) a' b') ->
            convImpl env a a' && convSkip x b (b' v)
        (VHPi _ a b, VPiND x a' b') ->
            convImpl env a a' && convSkip x (b (snd (fresh x))) b'
        (VHPi _ a b, VHPi (fresh -> (x, v)) a' b') ->
            convImpl env a a' && convSkip x (b v) (b' v)
        (VBool, VBool) ->
            True
        (VBoolLit b, VBoolLit b') ->
            b == b'
        (VBoolAnd t u, VBoolAnd t' u') ->
            convImpl env t t' && convImpl env u u'
        (VBoolOr t u, VBoolOr t' u') ->
            convImpl env t t' && convImpl env u u'
        (VBoolEQ t u, VBoolEQ t' u') ->
            convImpl env t t' && convImpl env u u'
        (VBoolNE t u, VBoolNE t' u') ->
            convImpl env t t' && convImpl env u u'
        (VBoolIf t u v, VBoolIf t' u' v') ->
            convImpl env t t' && convImpl env u u' && convImpl env v v'
        (VBytes, VBytes) ->
            True
        (VBytesLit l, VBytesLit r) ->
            l == r
        (VNatural, VNatural) ->
            True
        (VNaturalLit n, VNaturalLit n') ->
            n == n'
        (VNaturalFold t _ u v, VNaturalFold t' _ u' v') ->
            convImpl env t t' && convImpl env u u' && convImpl env v v'
        (VNaturalBuild t, VNaturalBuild t') ->
            convImpl env t t'
        (VNaturalIsZero t, VNaturalIsZero t') ->
            convImpl env t t'
        (VNaturalEven t, VNaturalEven t') ->
            convImpl env t t'
        (VNaturalOdd t, VNaturalOdd t') ->
            convImpl env t t'
        (VNaturalToInteger t, VNaturalToInteger t') ->
            convImpl env t t'
        (VNaturalShow t, VNaturalShow t') ->
            convImpl env t t'
        (VNaturalSubtract x y, VNaturalSubtract x' y') ->
            convImpl env x x' && convImpl env y y'
        (VNaturalPlus t u, VNaturalPlus t' u') ->
            convImpl env t t' && convImpl env u u'
        (VNaturalTimes t u, VNaturalTimes t' u') ->
            convImpl env t t' && convImpl env u u'
        (VInteger, VInteger) ->
            True
        (VIntegerLit t, VIntegerLit t') ->
            t == t'
        (VIntegerClamp t, VIntegerClamp t') ->
            convImpl env t t'
        (VIntegerNegate t, VIntegerNegate t') ->
            convImpl env t t'
        (VIntegerShow t, VIntegerShow t') ->
            convImpl env t t'
        (VIntegerToDouble t, VIntegerToDouble t') ->
            convImpl env t t'
        (VDouble, VDouble) ->
            True
        (VDoubleLit n, VDoubleLit n') ->
            n == n'
        (VDoubleShow t, VDoubleShow t') ->
            convImpl env t t'
        (VText, VText) ->
            True
        (VTextLit cs, VTextLit cs') ->
            convChunks cs cs'
        (VTextAppend t u, VTextAppend t' u') ->
            convImpl env t t' && convImpl env u u'
        (VTextShow t, VTextShow t') ->
            convImpl env t t'
        (VTextReplace a b c, VTextReplace a' b' c') ->
            convImpl env a a' && convImpl env b b' && convImpl env c c'
        (VDate, VDate) ->
            True
        (VDateLiteral l, VDateLiteral r) ->
            l == r
        (VDateShow t, VDateShow t') ->
            convImpl env t t'
        (VTime, VTime) ->
            True
        (VTimeLiteral hhL mmL ssL fL pL, VTimeLiteral hhR mmR ssR fR pR) ->
            hhL == hhR && mmL == mmR && ssL == ssR && fL == fR && pL == pR
        (VTimeShow t, VTimeShow t') ->
            convImpl env t t'
        (VTimeZone, VTimeZone) ->
            True
        (VTimeZoneLiteral l, VTimeZoneLiteral r) ->
            l == r
        (VTimeZoneShow t, VTimeZoneShow t') ->
            convImpl env t t'
        (VList a, VList a') ->
            convImpl env a a'
        (VListLit _ xs, VListLit _ xs') ->
            eqListBy (convImpl env) (toList xs) (toList xs')
        (VListAppend t u, VListAppend t' u') ->
            convImpl env t t' && convImpl env u u'
        (VListBuild _ t, VListBuild _ t') ->
            convImpl env t t'
        (VListLength a t, VListLength a' t') ->
            convImpl env a a' && convImpl env t t'
        (VListHead _ t, VListHead _ t') ->
            convImpl env t t'
        (VListLast _ t, VListLast _ t') ->
            convImpl env t t'
        (VListIndexed _ t, VListIndexed _ t') ->
            convImpl env t t'
        (VListReverse _ t, VListReverse _ t') ->
            convImpl env t t'
        (VListFold a l _ t u, VListFold a' l' _ t' u') ->
            convImpl env a a' && convImpl env l l' && convImpl env t t' && convImpl env u u'
        (VOptional a, VOptional a') ->
            convImpl env a a'
        (VSome t, VSome t') ->
            convImpl env t t'
        (VNone _, VNone _) ->
            True
        (VRecord m, VRecord m') ->
            eqMapsBy (convImpl env) m m'
        (VRecordLit m, VRecordLit m') ->
            eqMapsBy (convImpl env) m m'
        (VUnion m, VUnion m') ->
            eqMapsBy (eqMaybeBy (convImpl env)) m m'
        (VCombine _ t u, VCombine _ t' u') ->
            convImpl env t t' && convImpl env u u'
        (VCombineTypes t u, VCombineTypes t' u') ->
            convImpl env t t' && convImpl env u u'
        (VPrefer t u, VPrefer t' u') ->
            convImpl env t t' && convImpl env u u'
        (VMerge t u _, VMerge t' u' _) ->
            convImpl env t t' && convImpl env u u'
        (VToMap t _, VToMap t' _) ->
            convImpl env t t'
        (VShowConstructor t, VShowConstructor t') ->
            convImpl env t t'
        (VField t k, VField t' k') ->
            convImpl env t t' && k == k'
        (VProject t (Left ks), VProject t' (Left ks')) ->
            convImpl env t t' && ks == ks'
        (VProject t (Right e), VProject t' (Right e')) ->
            convImpl env t t' && convImpl env e e'
        (VAssert t, VAssert t') ->
            convImpl env t t'
        (VEquivalent t u, VEquivalent t' u') ->
            convImpl env t t' && convImpl env u u'
        (VInject m k mt, VInject m' k' mt') ->
            eqMapsBy (eqMaybeBy (convImpl env)) m m' && k == k' && eqMaybeBy (convImpl env) mt mt'
        (VEmbed a, VEmbed a') ->
            a == a'
        (_, _) ->
            False
  where
    fresh :: Text -> (Text, Val a)
    fresh x = (x, VVar x (countEnvironment x env))
    {-# INLINE fresh #-}

    freshClosure :: Closure a -> (Text, Val a, Closure a)
    freshClosure closure@(Closure x _ _ _) = (x, snd (fresh x), closure)
    {-# INLINE freshClosure #-}

    convChunks :: VChunks a -> VChunks a -> Bool
    convChunks (VChunks xys z) (VChunks xys' z') =
      eqListBy (\(x, y) (x', y') -> x == x' && convImpl env y y') xys xys' && z == z'
    {-# INLINE convChunks #-}

    convSkip :: Text -> Val a -> Val a -> Bool
    convSkip x = convImpl (skip env x)
    {-# INLINE convSkip #-}

judgmentallyEqual :: Eq a => Expr s a -> Expr t a -> Bool
judgmentallyEqual (Syntax.denote -> t) (Syntax.denote -> u) =
    withEvalRun defaultEvalConfig
        (convImpl Empty (evalImpl Empty t) (evalImpl Empty u))
{-# INLINABLE judgmentallyEqual #-}

data Names
  = EmptyNames
  | Bind !Names {-# UNPACK #-} !Text
  deriving Show

envNames :: Environment a -> Names
envNames Empty              = EmptyNames
envNames (Skip   env x _  ) = Bind (envNames env) x
envNames (Extend env x _ _) = Bind (envNames env) x

countNames :: Text -> Names -> Int
countNames x = go 0
  where
    go !acc EmptyNames         = acc
    go  acc (Bind env x') = go (if x == x' then acc + 1 else acc) env

-- | Quote a value by interning 'Dhall.Core.DAG.CoreNode's, then rebuild a
-- physically shared 'Expr'.
quote :: forall a. Eq a => Names -> Val a -> Expr Void a
quote env t0 = withEvalRun defaultEvalConfig $ runST $ do
    b <- DAG.newBuilder DAG.SharingOn
    nid <- quoteGraph b env t0
    g <- DAG.freeze b nid
    return (DAG.toSharedExpr g)

-- | Quote into an existing intern builder. Child nodes are interned before parents.
quoteGraph :: forall s a. Eq a => DAG.Builder s a -> Names -> Val a -> ST s DAG.NodeId
quoteGraph b = go
  where
    internN = DAG.intern b

    go :: Names -> Val a -> ST s DAG.NodeId
    go !env !t0 = do
        let quoteClosed = valClosed t0
        case (quoteClosed, lookupQuoteMemo (valId t0)) of
            (True, Just nid) -> return nid
            _ -> do
                nid <- goUncached env t0
                if quoteClosed
                    then return (insertQuoteMemo (valId t0) nid)
                    else return nid

    goUncached :: Names -> Val a -> ST s DAG.NodeId
    goUncached !env !t0 =
        case t0 of
            VConst k ->
                internN (DAG.CConst k)
            VVar !x !i ->
                internN (DAG.CVar (V x (countNames x env - i - 1)))
            VApp t u -> do
                t' <- go env t
                qApp env t' u
            VLam a (freshClosure env -> (x, v, t)) -> do
                a' <- go env a
                body <- go (Bind env x) (instantiate t v)
                internN (DAG.CLam x a' body)
            VHLam i t ->
                case i of
                    Typed (fresh env -> (x, v)) a -> do
                        a' <- go env a
                        body <- go (Bind env x) (t v)
                        internN (DAG.CLam x a' body)
                    Prim ->
                        go env (t VPrimVar)
                    NaturalSubtractZero -> do
                        f <- primitiveVar env "Natural/subtract"
                        z <- internN (DAG.CNaturalLit 0)
                        internN (DAG.CApp f z)
                    TextReplaceEmpty -> do
                        f <- primitiveVar env "Text/replace"
                        z <- internN (DAG.CTextLit [] "")
                        internN (DAG.CApp f z)
                    TextReplaceEmptyArgument replacement -> do
                        f <- primitiveVar env "Text/replace"
                        z <- internN (DAG.CTextLit [] "")
                        app <- internN (DAG.CApp f z)
                        r <- go env replacement
                        internN (DAG.CApp app r)
            VPi a (freshClosure env -> (x, v, t)) -> do
                a' <- go env a
                body <- go (Bind env x) (instantiate t v)
                internN (DAG.CPi x a' body)
            VHPi (fresh env -> (x, v)) a t -> do
                a' <- go env a
                body <- go (Bind env x) (t v)
                internN (DAG.CPi x a' body)
            VPiND x a t -> do
                a' <- go env a
                body <- go (Bind env x) t
                internN (DAG.CPi x a' body)
            VBool ->
                internN DAG.CBool
            VBoolLit x ->
                internN (DAG.CBoolLit x)
            VBoolAnd t u ->
                bin env DAG.CBoolAnd t u
            VBoolOr t u ->
                bin env DAG.CBoolOr t u
            VBoolEQ t u ->
                bin env DAG.CBoolEQ t u
            VBoolNE t u ->
                bin env DAG.CBoolNE t u
            VBoolIf t u v -> do
                t' <- go env t
                u' <- go env u
                v' <- go env v
                internN (DAG.CBoolIf t' u' v')
            VBytes ->
                internN DAG.CBytes
            VBytesLit x ->
                internN (DAG.CBytesLit x)
            VNatural ->
                internN DAG.CNatural
            VNaturalLit n ->
                internN (DAG.CNaturalLit n)
            VNaturalFold a t u v ->
                primApps env "Natural/fold" [a, t, u, v]
            VNaturalBuild t ->
                primApps env "Natural/build" [t]
            VNaturalIsZero t ->
                primApps env "Natural/isZero" [t]
            VNaturalEven t ->
                primApps env "Natural/even" [t]
            VNaturalOdd t ->
                primApps env "Natural/odd" [t]
            VNaturalToInteger t ->
                primApps env "Natural/toInteger" [t]
            VNaturalShow t ->
                primApps env "Natural/show" [t]
            VNaturalPlus t u ->
                bin env DAG.CNaturalPlus t u
            VNaturalTimes t u ->
                bin env DAG.CNaturalTimes t u
            VNaturalSubtract x y ->
                primApps env "Natural/subtract" [x, y]
            VInteger ->
                internN DAG.CInteger
            VIntegerLit n ->
                internN (DAG.CIntegerLit n)
            VIntegerClamp t ->
                primApps env "Integer/clamp" [t]
            VIntegerNegate t ->
                primApps env "Integer/negate" [t]
            VIntegerShow t ->
                primApps env "Integer/show" [t]
            VIntegerToDouble t ->
                primApps env "Integer/toDouble" [t]
            VDouble ->
                internN DAG.CDouble
            VDoubleLit n ->
                internN (DAG.CDoubleLit n)
            VDoubleShow t ->
                primApps env "Double/show" [t]
            VText ->
                internN DAG.CText
            VTextLit (VChunks xys z) -> do
                chunks <- for xys $ \(t, e) -> (,) t <$> go env e
                internN (DAG.CTextLit chunks z)
            VTextAppend t u ->
                bin env DAG.CTextAppend t u
            VTextShow t ->
                primApps env "Text/show" [t]
            VTextReplace a c d ->
                primApps env "Text/replace" [a, c, d]
            VDate ->
                internN DAG.CDate
            VDateLiteral d ->
                internN (DAG.CDateLiteral d)
            VDateShow t ->
                primApps env "Date/show" [t]
            VTime ->
                internN DAG.CTime
            VTimeLiteral hh mm ss frac p ->
                internN (DAG.CTimeLiteral hh mm ss frac p)
            VTimeShow t ->
                primApps env "Time/show" [t]
            VTimeZone ->
                internN DAG.CTimeZone
            VTimeZoneLiteral z ->
                internN (DAG.CTimeZoneLiteral z)
            VTimeZoneShow t ->
                primApps env "TimeZone/show" [t]
            VList t -> do
                list <- internN DAG.CList
                qApp env list t
            VListLit ma ts -> do
                ma' <- traverse (go env) ma
                ts' <- traverse (go env) (toList ts)
                internN (DAG.CListLit ma' ts')
            VListAppend t u ->
                bin env DAG.CListAppend t u
            VListBuild a t ->
                primApps env "List/build" [a, t]
            VListFold a l t u v ->
                primApps env "List/fold" [a, l, t, u, v]
            VListLength a t ->
                primApps env "List/length" [a, t]
            VListHead a t ->
                primApps env "List/head" [a, t]
            VListLast a t ->
                primApps env "List/last" [a, t]
            VListIndexed a t ->
                primApps env "List/indexed" [a, t]
            VListReverse a t ->
                primApps env "List/reverse" [a, t]
            VOptional a -> do
                opt <- internN DAG.COptional
                qApp env opt a
            VSome t ->
                go env t >>= internN . DAG.CSome
            VNone t -> do
                none <- internN DAG.CNone
                qApp env none t
            VRecord m -> do
                kvs <- for (Map.toList m) $ \(k, v) -> (,) k <$> go env v
                internN (DAG.CRecord kvs)
            VRecordLit m -> do
                kvs <- for (Map.toList m) $ \(k, v) -> (,) k <$> go env v
                internN (DAG.CRecordLit kvs)
            VUnion m -> do
                kvs <- for (Map.toList m) $ \(k, mv) -> (,) k <$> traverse (go env) mv
                internN (DAG.CUnion kvs)
            VCombine mk t u -> do
                t' <- go env t
                u' <- go env u
                internN (DAG.CCombine mk t' u')
            VCombineTypes t u ->
                bin env DAG.CCombineTypes t u
            VPrefer t u -> do
                t' <- go env t
                u' <- go env u
                internN (DAG.CPrefer PreferFromSource t' u')
            VMerge t u ma -> do
                t' <- go env t
                u' <- go env u
                ma' <- traverse (go env) ma
                internN (DAG.CMerge t' u' ma')
            VToMap t ma -> do
                t' <- go env t
                ma' <- traverse (go env) ma
                internN (DAG.CToMap t' ma')
            VShowConstructor t ->
                go env t >>= internN . DAG.CShowConstructor
            VField t k -> do
                t' <- go env t
                internN (DAG.CField t' k)
            VProject t p -> do
                t' <- go env t
                p' <- case p of
                    Left ks -> return (Left (Dhall.Set.toList ks))
                    Right e -> Right <$> go env e
                internN (DAG.CProject t' p')
            VAssert t ->
                go env t >>= internN . DAG.CAssert
            VEquivalent t u ->
                bin env DAG.CEquivalent t u
            VWith e ks v -> do
                e' <- go env e
                v' <- go env v
                internN (DAG.CWith e' ks v')
            VInject m k Nothing -> do
                kvs <- for (Map.toList m) $ \(l, mv) -> (,) l <$> traverse (go env) mv
                union <- internN (DAG.CUnion kvs)
                internN (DAG.CField union k)
            VInject m k (Just t) -> do
                kvs <- for (Map.toList m) $ \(l, mv) -> (,) l <$> traverse (go env) mv
                union <- internN (DAG.CUnion kvs)
                field <- internN (DAG.CField union k)
                qApp env field t
            VEmbed a ->
                internN (DAG.CEmbed a)
            VPrimVar ->
                error errorMsg

    fresh :: Names -> Text -> (Text, Val a)
    fresh env x = (x, VVar x (countNames x env))
    {-# INLINE fresh #-}

    freshClosure :: Names -> Closure a -> (Text, Val a, Closure a)
    freshClosure env closure@(Closure x _ _ _) = (x, snd (fresh env x), closure)
    {-# INLINE freshClosure #-}

    qApp :: Names -> DAG.NodeId -> Val a -> ST s DAG.NodeId
    qApp _ t VPrimVar = return t
    qApp env t u = do
        u' <- go env u
        internN (DAG.CApp t u')
    {-# INLINE qApp #-}

    bin
        :: Names
        -> (DAG.NodeId -> DAG.NodeId -> DAG.CoreNode a)
        -> Val a
        -> Val a
        -> ST s DAG.NodeId
    bin env cons t u = do
        t' <- go env t
        u' <- go env u
        internN (cons t' u')

    primitiveVar :: Names -> Text -> ST s DAG.NodeId
    primitiveVar env name =
        internN (DAG.CVar (V name (countNames name env)))

    primApps :: Names -> Text -> [Val a] -> ST s DAG.NodeId
    primApps env name args = do
        f0 <- primitiveVar env name
        foldM (\f arg -> qApp env f arg) f0 args


-- | Predefined functions implemented as ordinary variables with known names.
-- Parsing, pretty-printing, and CBOR treat them as plain variables.  Evaluation
-- and type-checking recognize a free name (De Bruijn leftover 0) via these
-- tables, including after a binder of the same name is peeled (@x@1).
data BuiltinPrim a = BuiltinPrim
    { primName :: Text
    , primType :: Val a
    , primVal  :: Val a
    }

unboundPrimitives :: Eq a => [BuiltinPrim a]
unboundPrimitives =
    [ BuiltinPrim "Natural/fold"      naturalFoldType      naturalFoldVal
    , BuiltinPrim "Natural/build"     naturalBuildType     naturalBuildVal
    , BuiltinPrim "Natural/isZero"    (VNatural ~> VBool)  naturalIsZeroVal
    , BuiltinPrim "Natural/even"      (VNatural ~> VBool)  naturalEvenVal
    , BuiltinPrim "Natural/odd"       (VNatural ~> VBool)  naturalOddVal
    , BuiltinPrim "Natural/toInteger" (VNatural ~> VInteger) naturalToIntegerVal
    , BuiltinPrim "Natural/show"      (VNatural ~> VText)  naturalShowVal
    , BuiltinPrim "Natural/subtract"  (VNatural ~> VNatural ~> VNatural) naturalSubtractVal
    , BuiltinPrim "Integer/clamp"     (VInteger ~> VNatural) integerClampVal
    , BuiltinPrim "Integer/negate"    (VInteger ~> VInteger) integerNegateVal
    , BuiltinPrim "Integer/show"      (VInteger ~> VText)  integerShowVal
    , BuiltinPrim "Integer/toDouble"  (VInteger ~> VDouble) integerToDoubleVal
    , BuiltinPrim "Double/show"       (VDouble ~> VText)   doubleShowVal
    , BuiltinPrim "Text/replace"      textReplaceType      textReplaceVal
    , BuiltinPrim "Text/show"         (VText ~> VText)     textShowVal
    , BuiltinPrim "Date/show"         (VDate ~> VText)     dateShowVal
    , BuiltinPrim "Time/show"         (VTime ~> VText)     timeShowVal
    , BuiltinPrim "TimeZone/show"     (VTimeZone ~> VText) timeZoneShowVal
    , BuiltinPrim "List/build"        listBuildType        listBuildVal
    , BuiltinPrim "List/fold"         listFoldType         listFoldVal
    , BuiltinPrim "List/length"       listLengthType       listLengthVal
    , BuiltinPrim "List/head"         listHeadType         listHeadVal
    , BuiltinPrim "List/last"         listLastType         listLastVal
    , BuiltinPrim "List/indexed"      listIndexedType      listIndexedVal
    , BuiltinPrim "List/reverse"      listReverseType      listReverseVal
    ]
  where
    naturalFoldType =
            VNatural
        ~>  VHPi "natural" (VConst Type) (\natural ->
                VHPi "succ" (natural ~> natural) (\_succ ->
                    VHPi "zero" natural (\_zero ->
                        natural
                    )
                )
            )

    naturalBuildType =
            VHPi "natural" (VConst Type) (\natural ->
                VHPi "succ" (natural ~> natural) (\_succ ->
                    VHPi "zero" natural (\_zero ->
                        natural
                    )
                )
            )
        ~>  VNatural

    textReplaceType =
        VHPi "needle" VText (\_needle ->
            VHPi "replacement" VText (\_replacement ->
                VHPi "haystack" VText (\_haystack ->
                    VText
                )
            )
        )

    listBuildType =
        VHPi "a" (VConst Type) (\a ->
            VHPi "_"
                (VHPi "list" (VConst Type) (\list ->
                    VHPi "cons" (a ~> list ~> list) (\_ ->
                        VHPi "nil" list (\_ -> list)
                    )
                ))
                (\_ -> VList a)
        )

    listFoldType =
        VHPi "a" (VConst Type) (\a ->
            VHPi "_" (VList a) (\_as ->
                VHPi "list" (VConst Type) (\list ->
                    VHPi "cons" (a ~> list ~> list) (\_ ->
                        VHPi "nil" list (\_ ->
                            list
                        )
                    )
                )
            )
        )

    listLengthType =
        VHPi "a" (VConst Type) (\a -> VList a ~> VNatural)

    listHeadType =
        VHPi "a" (VConst Type) (\a -> VList a ~> VOptional a)

    listLastType =
        VHPi "a" (VConst Type) (\a -> VList a ~> VOptional a)

    listIndexedType =
        VHPi "a" (VConst Type) (\a ->
            VList a
            ~> VList
                (VRecord
                    (Map.fromList
                        [ ("index", VNatural)
                        , ("value", a)
                        ]
                    )
                )
        )

    listReverseType =
        VHPi "a" (VConst Type) (\a -> VList a ~> VList a)

    naturalFoldVal =
        VPrim $ \n ->
        VPrim $ \natural ->
        VPrim $ \succ ->
        VPrim $ \zero ->
        let inert = VNaturalFold n natural succ zero
        in  case zero of
            VPrimVar -> inert
            _ -> case succ of
                VPrimVar -> inert
                _ -> case natural of
                    VPrimVar -> inert
                    _ -> case n of
                        VNaturalLit n' ->
                            -- Use an `Integer` for the loop, due to the
                            -- following issue:
                            --
                            -- https://github.com/ghcjs/ghcjs/issues/782
                            --
                            -- Bang `next` with `forceAccWHNF` at the fold
                            -- type: list spines; non-list record fields
                            -- (ListBench Naturals). List-typed record
                            -- fields stay lazy (IterateAlt inner lists).
                            -- Lazy `VHLam` cons / list-binder `VLam` keep
                            -- Iterate *elements* unforced (#2831). The
                            -- `succ acc ≡ acc` shortcut stays
                            -- `boundedType`-only.
                            let steps = fromIntegral n' :: Integer
                            in  if boundedType natural
                                then shortcutFold steps
                                else strictFold steps
                              where
                                shortcutFold = go zero
                                  where
                                    go !acc 0 = acc
                                    go !acc m =
                                        let !next = forceAccWHNF natural (vApp succ acc)
                                            -- Use structural conversion only: a shared
                                            -- 'ValId' CAF must not stop the loop early.
                                        in  if convBody Empty next acc
                                            then acc
                                            else go next (m - 1)
                                strictFold = go zero
                                  where
                                    go !acc 0 = acc
                                    go !acc m =
                                        let !next = forceAccWHNF natural (vApp succ acc)
                                        in  go next (m - 1)
                        _ -> inert

    naturalBuildVal =
        VPrim $ \case
            VPrimVar ->
                VNaturalBuild VPrimVar
            t ->       t
                `vApp` VNatural
                `vApp` VHLam (Typed "n" VNatural) (\n -> vNaturalPlus n (VNaturalLit 1))
                `vApp` VNaturalLit 0

    naturalIsZeroVal = VPrim $ \case
        VNaturalLit n -> VBoolLit (n == 0)
        n             -> VNaturalIsZero n

    naturalEvenVal = VPrim $ \case
        VNaturalLit n -> VBoolLit (even n)
        n             -> VNaturalEven n

    naturalOddVal = VPrim $ \case
        VNaturalLit n -> VBoolLit (odd n)
        n             -> VNaturalOdd n

    naturalToIntegerVal = VPrim $ \case
        VNaturalLit n -> VIntegerLit (fromIntegral n)
        n             -> VNaturalToInteger n

    naturalShowVal = VPrim $ \case
        VNaturalLit n -> VTextLit (VChunks [] (Text.pack (show n)))
        n             -> VNaturalShow n

    naturalSubtractVal = VPrim $ \case
        VNaturalLit 0 ->
            VHLam NaturalSubtractZero id
        x@(VNaturalLit m) ->
            VPrim $ \case
                VNaturalLit n
                    | n >= m ->
                        -- Use an `Integer` for the subtraction, due to:
                        -- https://github.com/ghcjs/ghcjs/issues/782
                        VNaturalLit (fromIntegral (subtract (fromIntegral m :: Integer) (fromIntegral n :: Integer)))
                    | otherwise -> VNaturalLit 0
                y -> VNaturalSubtract x y
        x ->
            VPrim $ \case
                VNaturalLit 0    -> VNaturalLit 0
                y | convImpl Empty x y -> VNaturalLit 0
                y                  -> VNaturalSubtract x y

    integerClampVal =
        VPrim $ \case
            VIntegerLit n
                | 0 <= n    -> VNaturalLit (fromInteger n)
                | otherwise -> VNaturalLit 0
            n -> VIntegerClamp n

    integerNegateVal =
        VPrim $ \case
            VIntegerLit n -> VIntegerLit (negate n)
            n             -> VIntegerNegate n

    integerShowVal =
        VPrim $ \case
            VIntegerLit n
                | 0 <= n    -> VTextLit (VChunks [] (Text.pack ('+':show n)))
                | otherwise -> VTextLit (VChunks [] (Text.pack (show n)))
            n -> VIntegerShow n

    integerToDoubleVal =
        VPrim $ \case
            VIntegerLit n -> VDoubleLit (DhallDouble (read (show n)))
            -- `(read . show)` is used instead of `fromInteger`
            -- because `read` uses the correct rounding rule.
            -- See https://gitlab.haskell.org/ghc/ghc/issues/17231.
            n             -> VIntegerToDouble n

    doubleShowVal =
        VPrim $ \case
            VDoubleLit (DhallDouble n) -> VTextLit (VChunks [] (Text.pack (show n)))
            n                          -> VDoubleShow n

    textShowVal =
        VPrim $ \case
            VTextLit (VChunks [] x) -> VTextLit (VChunks [] (textShow x))
            t                       -> VTextShow t

    textReplaceVal =
        VPrim $ \needle ->
        let hLamInfo0 = case needle of
                VTextLit (VChunks [] "") -> TextReplaceEmpty
                _                        -> Prim

        in  VHLam hLamInfo0 $ \replacement ->
        let hLamInfo1 = case needle of
                VTextLit (VChunks [] "") ->
                    TextReplaceEmptyArgument replacement
                _ ->
                    Prim
        in  VHLam hLamInfo1 $ \haystack ->
                case needle of
                    VTextLit (VChunks [] "") ->
                        haystack

                    VTextLit (VChunks [] needleText) ->
                        case haystack of
                            VTextLit (VChunks [] haystackText) ->
                                case replacement of
                                    VTextLit (VChunks [] replacementText) ->
                                        VTextLit $ VChunks []
                                            (Text.replace
                                                needleText
                                                replacementText
                                                haystackText
                                            )
                                    _ ->
                                        VTextLit
                                            (vTextReplace
                                                needleText
                                                replacement
                                                haystackText
                                            )
                            _ ->
                                VTextReplace needle replacement haystack
                    _ ->
                        VTextReplace needle replacement haystack

    dateShowVal =
        VPrim $ \case
            VDateLiteral d -> VTextLit (VChunks [] (dateShow d))
            t              -> VDateShow t

    timeShowVal =
        VPrim $ \case
            VTimeLiteral hh mm ss frac p -> VTextLit (VChunks [] (timeShow hh mm ss frac p))
            t                            -> VTimeShow t

    timeZoneShowVal =
        VPrim $ \case
            VTimeZoneLiteral d -> VTextLit (VChunks [] (timezoneShow d))
            t                  -> VTimeZoneShow t

    listBuildVal =
        VPrim $ \a ->
        VPrim $ \case
            VPrimVar ->
                VListBuild a VPrimVar
            t ->       t
                `vApp` VList a
                -- Constructor: must not force `x`. Laziness is `vApp`'s
                -- `VHLam` branch (a `~x` pattern here cannot undo `vApp !u`).
                `vApp` VHLam (Typed "a" a) (\x ->
                       VHLam (Typed "as" (VList a)) (\as ->
                       vListAppend (VListLit Nothing (pure x)) as))
                `vApp` VListLit (Just (VList a)) mempty

    listFoldVal =
        VPrim $ \a ->
        VPrim $ \as ->
        VPrim $ \list ->
        VPrim $ \cons ->
        VPrim $ \nil ->
        let inert = VListFold a as list cons nil
        in  case nil of
            VPrimVar -> inert
            _ -> case cons of
                VPrimVar -> inert
                _ -> case list of
                    VPrimVar -> inert
                    _ -> case a of
                        VPrimVar -> inert
                        _ -> case as of
                            VListLit _ as' ->
                                foldr' (\x b -> cons `vApp` x `vApp` b) nil as'
                            _ -> inert

    listLengthVal =
        VPrim $ \ a ->
        VPrim $ \case
            VListLit _ as -> VNaturalLit (fromIntegral (Sequence.length as))
            as            -> VListLength a as

    listHeadVal =
        VPrim $ \ a ->
        VPrim $ \case
            VListLit _ as ->
                case Sequence.viewl as of
                    y :< _ -> VSome y
                    _      -> VNone a
            as ->
                VListHead a as

    listLastVal =
        VPrim $ \ a ->
        VPrim $ \case
            VListLit _ as ->
                case Sequence.viewr as of
                    _ :> t -> VSome t
                    _      -> VNone a
            as -> VListLast a as

    listIndexedVal =
        VPrim $ \ a ->
        VPrim $ \case
            VListLit _ as ->
                let a' =
                        if null as
                        then Just (VList (VRecord (Map.unorderedFromList [("index", VNatural), ("value", a)])))
                        else Nothing

                    as' =
                        Sequence.mapWithIndex
                            (\i t ->
                                VRecordLit
                                    (Map.unorderedFromList
                                        [ ("index", VNaturalLit (fromIntegral i))
                                        , ("value", t)
                                        ]
                                    )
                            )
                            as

                    in  VListLit a' as'
            t ->
                VListIndexed a t

    listReverseVal =
        VPrim $ \ ~a ->
        VPrim $ \case
            VListLit t as | null as ->
                VListLit t as
            VListLit _ as ->
                VListLit Nothing (Sequence.reverse as)
            t ->
                VListReverse a t

-- | Types of predefined functions when the name is free (empty type context).
unboundBuiltinTypes :: Eq a => HashMap.HashMap Text (Val a)
unboundBuiltinTypes =
    HashMap.fromList
        [ (primName prim, primType prim)
        | prim <- unboundPrimitives
        ]

-- | Values of predefined functions when the name is free (empty environment).
unboundBuiltinValues :: Eq a => HashMap.HashMap Text (Val a)
unboundBuiltinValues =
    HashMap.fromList
        [ (primName prim, primVal prim)
        | prim <- unboundPrimitives
        ]

-- | Normalize an expression in an environment of values. Any variable pointing out of
--   the environment is treated as opaque free variable.
nf :: Eq a => Environment a -> Expr s a -> Expr t a
nf !env e = nfWithConfig defaultEvalConfig env e

nfWithConfig :: Eq a => EvalConfig -> Environment a -> Expr s a -> Expr t a
nfWithConfig cfg !env e = withEvalRun cfg $
    Syntax.renote (quoteImpl (envNames env) (evalImpl env (Syntax.denote e)))

-- | Like 'normalize' with an explicit 'EvalConfig' (A/B and cache oracles).
normalizeWithConfig :: Eq a => EvalConfig -> Expr s a -> Expr t a
normalizeWithConfig cfg = nfWithConfig cfg Empty

-- | Normalize under 'withEvalRunIO', reporting allocation stats and eval/quote
-- wall time. Intended for micro-profiling, not Criterion loops.
normalizeWithStats
    :: Eq a
    => EvalConfig
    -> Expr s a
    -> IO (Expr Void a, EvalStats)
normalizeWithStats cfg e = withEvalRunIO cfg $ do
    let denoted = Syntax.denote e
    t0 <- getMonotonicTimeNSec
    let !v = evalImpl Empty denoted
    t1 <- getMonotonicTimeNSec
    let !nfExpr = quoteImpl EmptyNames v
    t2 <- getMonotonicTimeNSec
    stats <- readEvalStatsIO
    return
        ( nfExpr
        , stats
            { evalNanos  = t1 - t0
            , quoteNanos = t2 - t1
            }
        )

quoteInRun :: forall a. Eq a => Names -> Val a -> Expr Void a
quoteInRun = quoteImpl

quoteImpl :: forall a. Eq a => Names -> Val a -> Expr Void a
quoteImpl env t0 = runST $ do
    b <- DAG.newBuilder DAG.SharingOn
    nid <- quoteGraph b env t0
    g <- DAG.freeze b nid
    return (DAG.toSharedExpr g)

-- | Normalize into a frozen hash-consed graph (sharing on). Compatibility
-- 'normalize' rebuilds a physically shared 'Expr' from this graph.
normalizeGraph :: Eq a => Expr s a -> DAG.CoreGraph a
normalizeGraph = normalizeGraphWith DAG.SharingOn

-- | Same as 'normalizeGraph' with an explicit intern-sharing mode (oracle).
normalizeGraphWith :: Eq a => DAG.SharingMode -> Expr s a -> DAG.CoreGraph a
normalizeGraphWith mode e =
    withEvalRun defaultEvalConfig { internSharing = mode } $ runST $ do
        b <- DAG.newBuilder mode
        nid <- quoteGraph b EmptyNames (evalImpl Empty (Syntax.denote e))
        DAG.freeze b nid

normalize :: Eq a => Expr s a -> Expr t a
normalize = nf Empty
{-# INLINABLE normalize #-}

alphaNormalize :: Expr s a -> Expr s a
alphaNormalize = goEnv EmptyNames
  where
    goVar :: Names -> Text -> Int -> Expr s a
    goVar e topX topI = go 0 e topI
      where
        go !acc (Bind env x) !i
          | x == topX = if i == 0 then Var (V "_" acc) else go (acc + 1) env (i - 1)
          | otherwise = go (acc + 1) env i
        go _ EmptyNames i = Var (V topX i)

    goEnv :: Names -> Expr s a -> Expr s a
    goEnv !e0 t0 =
        case t0 of
            Const k ->
                Const k
            Var (V x i) ->
                goVar e0 x i
            Lam cs (FunctionBinding src0 x src1 src2 t) u ->
                Lam cs (FunctionBinding src0 "_" src1 src2 (go t)) (goBind x u)
            Pi cs x a b ->
                Pi cs "_" (go a) (goBind x b)
            App t u ->
                App (go t) (go u)
            Let (Binding src0 x src1 mA src2 a) b ->
                Let (Binding src0 "_" src1 (fmap (fmap go) mA) src2 (go a)) (goBind x b)
            Annot t u ->
                Annot (go t) (go u)
            Bool ->
                Bool
            BoolLit b ->
                BoolLit b
            BoolAnd t u ->
                BoolAnd (go t) (go u)
            BoolOr t u ->
                BoolOr  (go t) (go u)
            BoolEQ t u ->
                BoolEQ  (go t) (go u)
            BoolNE t u ->
                BoolNE  (go t) (go u)
            BoolIf b t f ->
                BoolIf  (go b) (go t) (go f)
            Bytes ->
                Bytes
            BytesLit b ->
                BytesLit b
            Natural ->
                Natural
            NaturalLit n ->
                NaturalLit n
            NaturalPlus t u ->
                NaturalPlus (go t) (go u)
            NaturalTimes t u ->
                NaturalTimes (go t) (go u)
            Integer ->
                Integer
            IntegerLit n ->
                IntegerLit n
            Double ->
                Double
            DoubleLit n ->
                DoubleLit n
            Text ->
                Text
            TextLit cs ->
                TextLit (goChunks cs)
            TextAppend t u ->
                TextAppend (go t) (go u)
            Date ->
                Date
            DateLiteral d ->
                DateLiteral d
            Time ->
                Time
            TimeLiteral hh mm ss frac p ->
                TimeLiteral hh mm ss frac p
            TimeZone ->
                TimeZone
            TimeZoneLiteral z ->
                TimeZoneLiteral z
            List ->
                List
            ListLit ma ts ->
                ListLit (fmap go ma) (fmap go ts)
            ListAppend t u ->
                ListAppend (go t) (go u)
            Optional ->
                Optional
            Some t ->
                Some (go t)
            None ->
                None
            Record kts ->
                Record (goRecordField <$> kts)
            RecordLit kts ->
                RecordLit (goRecordField <$> kts)
            Union kts ->
                Union (fmap (fmap go) kts)
            Combine cs m t u ->
                Combine cs m (go t) (go u)
            CombineTypes cs t u ->
                CombineTypes cs (go t) (go u)
            Prefer cs b t u ->
                Prefer cs b (go t) (go u)
            RecordCompletion t u ->
                RecordCompletion (go t) (go u)
            Merge x y ma ->
                Merge (go x) (go y) (fmap go ma)
            ToMap x ma ->
                ToMap (go x) (fmap go ma)
            ShowConstructor x ->
                ShowConstructor (go x)
            Field t k ->
                Field (go t) k
            Project t ks ->
                Project (go t) (fmap go ks)
            Assert t ->
                Assert (go t)
            Equivalent cs t u ->
                Equivalent cs (go t) (go u)
            With e k v ->
                With (go e) k (go v)
            Note s e ->
                Note s (go e)
            ImportAlt t u ->
                ImportAlt (go t) (go u)
            Embed a ->
                Embed a
      where
        go                     = goEnv e0
        goBind x               = goEnv (Bind e0 x)
        goChunks (Chunks ts x) = Chunks (fmap (fmap go) ts) x
        goRecordField (RecordField s0 e s1 s2) = RecordField s0 (go e) s1 s2
