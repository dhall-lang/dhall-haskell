
{-| This module contains logic for converting Dhall expressions to CBOR
    expressions which can in turn be converted to a binary representation. The
    purpose of the conversion defined here is purely to be able to compute a
    secure hash from the binary representation.
-}

module Dhall.Binary
    ( -- * Standard versions
      StandardVersion(..)
    , defaultStandardVersion
    , parseStandardVersion
    , renderStandardVersion

    -- * Encoding and decoding
    , ToTerm(..)
    , encodeExpression
    ) where

import Codec.CBOR.Term (Term(..))
import Dhall.Core
    ( Binding(..)
    , Chunks(..)
    , Const(..)
    , Expr(..)
    , Projection(..)
    , X
    , internalError
    , absurd
    )

import Data.Foldable (toList)
import Data.Monoid ((<>))
import Data.Text (Text)
import Options.Applicative (Parser)
import Prelude hiding (exponent)
import GHC.Float (double2Float, float2Double)

import qualified Dhall.Map
import qualified Dhall.Set
import qualified Options.Applicative

-- | Supported version strings
data StandardVersion
    = NoVersion
    -- ^ No version string
    | V_5_0_0
    -- ^ Version "5.0.0"
    | V_4_0_0
    -- ^ Version "4.0.0"
    | V_3_0_0
    -- ^ Version "3.0.0"
    | V_2_0_0
    -- ^ Version "2.0.0"
    | V_1_0_0
    -- ^ Version "1.0.0"
    deriving (Enum, Bounded)

defaultStandardVersion :: StandardVersion
defaultStandardVersion = NoVersion

parseStandardVersion :: Parser StandardVersion
parseStandardVersion =
    Options.Applicative.option readVersion
        (   Options.Applicative.long "standard-version"
        <>  Options.Applicative.metavar "X.Y.Z"
        <>  Options.Applicative.help "The standard version to use"
        <>  Options.Applicative.value defaultStandardVersion
        )
  where
    readVersion = do
        string <- Options.Applicative.str
        case string :: Text of
            "none"  -> return NoVersion
            "1.0.0" -> return V_1_0_0
            "2.0.0" -> return V_2_0_0
            "3.0.0" -> return V_3_0_0
            "4.0.0" -> return V_4_0_0
            "5.0.0" -> return V_5_0_0
            _       -> fail "Unsupported version"

renderStandardVersion :: StandardVersion -> Text
renderStandardVersion NoVersion = "none"
renderStandardVersion V_1_0_0   = "1.0.0"
renderStandardVersion V_2_0_0   = "2.0.0"
renderStandardVersion V_3_0_0   = "3.0.0"
renderStandardVersion V_4_0_0   = "4.0.0"
renderStandardVersion V_5_0_0   = "5.0.0"

{-| Convert a function applied to multiple arguments to the base function and
    the list of arguments
-}
unApply :: Expr a -> (Expr a, [Expr a])
unApply e₀ = (baseFunction₀, diffArguments₀ [])
  where
    ~(baseFunction₀, diffArguments₀) = go e₀

    go (App f a) = (baseFunction, diffArguments . (a :))
      where
        ~(baseFunction, diffArguments) = go f

    go baseFunction = (baseFunction, id)

-- | Types that can be encoded as a CBOR `Term`
class ToTerm a where
    encode :: a -> Term

instance ToTerm Const where
  encode Type = TString "Type"
  encode Kind = TString "Kind"
  encode Sort = TString "Sort"
  {-# INLINE encode #-}

instance ToTerm (Expr X) where
    encode (Var n) =
        TInt n
    encode NaturalBuild =
        TString "Natural/build"
    encode NaturalFold =
        TString "Natural/fold"
    encode NaturalIsZero =
        TString "Natural/isZero"
    encode NaturalEven =
        TString "Natural/even"
    encode NaturalOdd =
        TString "Natural/odd"
    encode NaturalToInteger =
        TString "Natural/toInteger"
    encode NaturalShow =
        TString "Natural/show"
    encode IntegerToDouble =
        TString "Integer/toDouble"
    encode IntegerShow =
        TString "Integer/show"
    encode DoubleShow =
        TString "Double/show"
    encode ListBuild =
        TString "List/build"
    encode ListFold =
        TString "List/fold"
    encode ListLength =
        TString "List/length"
    encode ListHead =
        TString "List/head"
    encode ListLast =
        TString "List/last"
    encode ListIndexed =
        TString "List/indexed"
    encode ListReverse =
        TString "List/reverse"
    encode OptionalFold =
        TString "Optional/fold"
    encode OptionalBuild =
        TString "Optional/build"
    encode Bool =
        TString "Bool"
    encode Optional =
        TString "Optional"
    encode None =
        TString "None"
    encode Natural =
        TString "Natural"
    encode Integer =
        TString "Integer"
    encode Double =
        TString "Double"
    encode Text =
        TString "Text"
    encode TextShow =
        TString "Text/show"
    encode List =
        TString "List"
    encode (Const Type) =
        TString "Type"
    encode (Const Kind) =
        TString "Kind"
    encode (Const Sort) =
        TString "Sort"
    encode e@(App _ _) =
        TList ([ TInt 0, f₁ ] ++ map encode arguments)
      where
        (f₀, arguments) = unApply e

        f₁ = encode f₀
    encode (Lam ("_", _) _A₀ b₀) =
        TList [ TInt 1, _A₁, b₁ ]
      where
        _A₁ = encode _A₀
        b₁  = encode b₀
    encode (Lam (x, _) _A₀ b₀) =
        TList [ TInt 1, TString x, _A₁, b₁ ]
      where
        _A₁ = encode _A₀
        b₁  = encode b₀
    encode (Pi "_" _ _ _A₀ _B₀) =
        TList [ TInt 2, _A₁, _B₁ ]
      where
        _A₁ = encode _A₀
        _B₁ = encode _B₀
    encode (Pi x _ _ _A₀ _B₀) =
        TList [ TInt 2, TString x, _A₁, _B₁ ]
      where
        _A₁ = encode _A₀
        _B₁ = encode _B₀
    encode (BoolOr l₀ r₀) =
        TList [ TInt 3, TInt 0, l₁, r₁ ]
      where
        l₁ = encode l₀
        r₁ = encode r₀
    encode (BoolAnd l₀ r₀) =
        TList [ TInt 3, TInt 1, l₁, r₁ ]
      where
        l₁ = encode l₀
        r₁ = encode r₀
    encode (BoolEQ l₀ r₀) =
        TList [ TInt 3, TInt 2, l₁, r₁ ]
      where
        l₁ = encode l₀
        r₁ = encode r₀
    encode (BoolNE l₀ r₀) =
        TList [ TInt 3, TInt 3, l₁, r₁ ]
      where
        l₁ = encode l₀
        r₁ = encode r₀
    encode (NaturalPlus l₀ r₀) =
        TList [ TInt 3, TInt 4, l₁, r₁ ]
      where
        l₁ = encode l₀
        r₁ = encode r₀
    encode (NaturalTimes l₀ r₀) =
        TList [ TInt 3, TInt 5, l₁, r₁ ]
      where
        l₁ = encode l₀
        r₁ = encode r₀
    encode (TextAppend l₀ r₀) =
        TList [ TInt 3, TInt 6, l₁, r₁ ]
      where
        l₁ = encode l₀
        r₁ = encode r₀
    encode (ListAppend l₀ r₀) =
        TList [ TInt 3, TInt 7, l₁, r₁ ]
      where
        l₁ = encode l₀
        r₁ = encode r₀
    encode (Combine l₀ r₀) =
        TList [ TInt 3, TInt 8, l₁, r₁ ]
      where
        l₁ = encode l₀
        r₁ = encode r₀
    encode (Prefer l₀ r₀) =
        TList [ TInt 3, TInt 9, l₁, r₁ ]
      where
        l₁ = encode l₀
        r₁ = encode r₀
    encode (CombineTypes l₀ r₀) =
        TList [ TInt 3, TInt 10, l₁, r₁ ]
      where
        l₁ = encode l₀
        r₁ = encode r₀
    encode (ImportAlt _l₀ _r₀ _) =
      error $ internalError "unexpected import alternative"
      --   TList [ TInt 3, TInt 11, l₁, r₁ ]
      -- where
      --   l₁ = encode l₀
      --   r₁ = encode r₀
    encode (ListLit _T₀ xs₀)
        | null xs₀  = TList [ TInt label, _T₁ ]
        | otherwise = TList ([ TInt 4, TNull ] ++ xs₁)
      where
        (label, _T₁) = case _T₀ of
            Nothing     -> (4 , TNull)
            Just (t, _) -> (4 , encode t)

        xs₁ = map encode (Data.Foldable.toList xs₀)
    encode (Some t₀) =
        TList [ TInt 5, TNull, t₁ ]
      where
        t₁ = encode t₀
    encode (Merge t₀ u₀ Nothing) =
        TList [ TInt 6, t₁, u₁ ]
      where
        t₁ = encode t₀
        u₁ = encode u₀
    encode (Merge t₀ u₀ (Just (_T₀, _))) =
        TList [ TInt 6, t₁, u₁, _T₁ ]
      where
        t₁  = encode t₀
        u₁  = encode u₀
        _T₁ = encode _T₀
    encode (Record xTs₀) =
        TList [ TInt 7, TMap xTs₁ ]
      where
        xTs₁ = do
            (x₀, _T₀) <- Dhall.Map.toList (Dhall.Map.sort xTs₀)
            let x₁  = TString x₀
            let _T₁ = encode _T₀
            return (x₁, _T₁)
    encode (RecordLit xts₀) =
        TList [ TInt 8, TMap xts₁ ]
      where
        xts₁ = do
            (x₀, t₀) <- Dhall.Map.toList (Dhall.Map.sort xts₀)
            let x₁ = TString x₀
            let t₁ = encode t₀
            return (x₁, t₁)
    encode (Project t₀ (ProjSingle x)) =
        TList [ TInt 9, t₁, TString x ]
      where
        t₁ = encode t₀
    encode (Inject t k _) =
        TList [TInt 9, encode t, TString k]
    encode (Project t₀ (ProjSet xs₀ Nothing)) =
        TList ([ TInt 10, t₁ ] ++ xs₁)
      where
        t₁  = encode t₀
        xs₁ = map TString (Dhall.Set.toList xs₀)
    encode (Project t₀ (ProjSet _ (Just _T₀))) =
        TList [ TInt 10, t₁, TList [ _T₁ ] ]
      where
        _T₁ = encode _T₀
        t₁  = encode t₀
    encode (Union xTs₀) =
        TList [ TInt 11, TMap xTs₁ ]
      where
        xTs₁ = do
            (x₀, mT₀) <- Dhall.Map.toList (Dhall.Map.sort xTs₀)

            let x₁  = TString x₀

            let _T₁ = case mT₀ of
                    Nothing  -> TNull
                    Just _T₀ -> encode _T₀
            return (x₁, _T₁)
    encode (BoolLit b) =
        TBool b
    encode (BoolIf t₀ l₀ r₀) =
        TList [ TInt 14, t₁, l₁, r₁ ]
      where
        t₁ = encode t₀
        l₁ = encode l₀
        r₁ = encode r₀
    encode (NaturalLit n) =
        TList [ TInt 15, TInteger (fromIntegral n) ]
    encode (IntegerLit n) =
        TList [ TInt 16, TInteger n ]
    encode (DoubleLit n64)
        -- cborg always encodes NaN as "7e00"
        | isNaN n64 = THalf n32
        | useHalf   = THalf n32
        | useFloat  = TFloat n32
        | otherwise = TDouble n64
      where
        n32      = double2Float n64
        useFloat = n64 == float2Double n32
        -- the other three cases for Half-floats are 0.0 and the infinities
        useHalf  = or $ fmap (n64 ==) [0.0, infinity, -infinity]
        infinity = 1/0 :: Double
    encode (TextLit (Chunks xys₀ z₀)) =
        TList ([ TInt 18 ] ++ xys₁ ++ [ z₁ ])
      where
        xys₁ = do
            (x₀, y₀) <- xys₀
            let x₁ = TString x₀
            let y₁ = encode y₀
            [ x₁, y₁ ]

        z₁ = TString z₀
    encode (EmbedImport x) = absurd x
    encode (Let as₀ b₀) =
        TList ([ TInt 25 ] ++ as₁ ++ [ b₁ ])
      where
        as₁ = do
            Binding x mA₀ a₀ <- toList as₀

            let mA₁ = case mA₀ of
                    Nothing  -> TNull
                    Just _A₀ -> encode _A₀

            let a₁ = encode a₀

            [ TString x, mA₁, a₁ ]

        b₁ = encode b₀
    encode (Annot t₀ _T₀) =
        TList [ TInt 26, t₁, _T₁ ]
      where
        t₁  = encode t₀
        _T₁ = encode _T₀
    encode (ToMap t₀ Nothing) =
        TList [ TInt 27, t₁ ]
      where
        t₁ = encode t₀
    encode (ToMap t₀ (Just (_T₀, _))) =
        TList [ TInt 27, t₁, _T₁ ]
      where
        t₁  = encode t₀
        _T₁ = encode _T₀

-- | Encode a Dhall expression as a CBOR `Term`
encodeExpression :: Expr X -> Term
encodeExpression = encode
