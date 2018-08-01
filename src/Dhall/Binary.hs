{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

{-| This module contains logic for converting Dhall expressions to and from
    CBOR expressions which can in turn be converted to and from a binary
    representation
-}

module Dhall.Binary
    ( -- * Protocol versions
      ProtocolVersion(..)
    , parseProtocolVersion

    -- * Encoding and decoding
    , encode
    , decode
    ) where

import Codec.CBOR.Term (Term(..))
import Control.Applicative (empty)
import Dhall.Core
    ( Chunks(..)
    , Const(..)
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
    )
import Data.Text (Text)
import Options.Applicative (Parser)
import Prelude hiding (exponent)

import qualified Data.Foldable
import qualified Data.HashMap.Strict.InsOrd
import qualified Data.Scientific
import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Text
import qualified Options.Applicative

-- | Supported protocol version strings
data ProtocolVersion
    = V_1_0
    -- ^ Protocol version string "1.0"

parseProtocolVersion :: Parser ProtocolVersion
parseProtocolVersion =
    Options.Applicative.option readProtocolVersion
        (   Options.Applicative.long "protocol-version"
        <>  Options.Applicative.metavar "X.Y"
        <>  Options.Applicative.value V_1_0
        )
  where
    readProtocolVersion = do
        string <- Options.Applicative.str
        case string :: Text of
            "1.0" -> return V_1_0
            _     -> fail "Unsupported protocol version"

{-| Convert a function applied to multiple arguments to the base function and
    the list of arguments
-}
unApply :: Expr s a -> (Expr s a, [Expr s a])
unApply e = (baseFunction₀, diffArguments₀ [])
  where
    ~(baseFunction₀, diffArguments₀) = go e

    go (App f a) = (baseFunction, diffArguments . (a :))
      where
        ~(baseFunction, diffArguments) = go f
    go baseFunction = (baseFunction, id)

encode_1_0 :: Expr s Import -> Term
encode_1_0 (Var (V "_" n)) =
    TInteger n
encode_1_0 (Var (V x 0)) =
    TString x
encode_1_0 (Var (V x n)) =
    TList [ TString x, TInteger n ]
encode_1_0 NaturalBuild =
    TString "Natural/build"
encode_1_0 NaturalFold =
    TString "Natural/fold"
encode_1_0 NaturalIsZero =
    TString "Natural/isZero"
encode_1_0 NaturalEven =
    TString "Natural/even"
encode_1_0 NaturalOdd =
    TString "Natural/odd"
encode_1_0 NaturalToInteger =
    TString "Natural/toInteger"
encode_1_0 NaturalShow =
    TString "Natural/show"
encode_1_0 IntegerToDouble =
    TString "Integer/toDouble"
encode_1_0 IntegerShow =
    TString "Integer/show"
encode_1_0 DoubleShow =
    TString "Double/show"
encode_1_0 ListBuild =
    TString "List/build"
encode_1_0 ListFold =
    TString "List/fold"
encode_1_0 ListLength =
    TString "List/length"
encode_1_0 ListHead =
    TString "List/head"
encode_1_0 ListLast =
    TString "List/last"
encode_1_0 ListIndexed =
    TString "List/indexed"
encode_1_0 ListReverse =
    TString "List/reverse"
encode_1_0 OptionalFold =
    TString "Optional/fold"
encode_1_0 OptionalBuild =
    TString "Optional/build"
encode_1_0 Bool =
    TString "Bool"
encode_1_0 Optional =
    TString "Optional"
encode_1_0 Natural =
    TString "Natural"
encode_1_0 Integer =
    TString "Integer"
encode_1_0 Double =
    TString "Double"
encode_1_0 Text =
    TString "Text"
encode_1_0 List =
    TString "List"
encode_1_0 (Const Type) =
    TString "Type"
encode_1_0 (Const Kind) =
    TString "Kind"
encode_1_0 e@(App _ _) =
    TList ([ TInt 0, f₁ ] ++ map encode_1_0 arguments)
  where
    (f₀, arguments) = unApply e

    f₁ = encode_1_0 f₀
encode_1_0 (Lam "_" _A₀ b₀) =
    TList [ TInt 1, _A₁, b₁ ]
  where
    _A₁ = encode_1_0 _A₀
    b₁  = encode_1_0 b₀
encode_1_0 (Lam x _A₀ b₀) =
    TList [ TInt 1, TString x, _A₁, b₁ ]
  where
    _A₁ = encode_1_0 _A₀
    b₁  = encode_1_0 b₀
encode_1_0 (Pi "_" _A₀ _B₀) =
    TList [ TInt 2, _A₁, _B₁ ]
  where
    _A₁ = encode_1_0 _A₀
    _B₁ = encode_1_0 _B₀
encode_1_0 (Pi x _A₀ _B₀) =
    TList [ TInt 2, TString x, _A₁, _B₁ ]
  where
    _A₁ = encode_1_0 _A₀
    _B₁ = encode_1_0 _B₀
encode_1_0 (BoolOr l₀ r₀) =
    TList [ TInt 3, TInt 0, l₁, r₁ ]
  where
    l₁ = encode_1_0 l₀
    r₁ = encode_1_0 r₀
encode_1_0 (BoolAnd l₀ r₀) =
    TList [ TInt 3, TInt 1, l₁, r₁ ]
  where
    l₁ = encode_1_0 l₀
    r₁ = encode_1_0 r₀
encode_1_0 (BoolEQ l₀ r₀) =
    TList [ TInt 3, TInt 2, l₁, r₁ ]
  where
    l₁ = encode_1_0 l₀
    r₁ = encode_1_0 r₀
encode_1_0 (BoolNE l₀ r₀) =
    TList [ TInt 3, TInt 3, l₁, r₁ ]
  where
    l₁ = encode_1_0 l₀
    r₁ = encode_1_0 r₀
encode_1_0 (NaturalPlus l₀ r₀) =
    TList [ TInt 3, TInt 4, l₁, r₁ ]
  where
    l₁ = encode_1_0 l₀
    r₁ = encode_1_0 r₀
encode_1_0 (NaturalTimes l₀ r₀) =
    TList [ TInt 3, TInt 5, l₁, r₁ ]
  where
    l₁ = encode_1_0 l₀
    r₁ = encode_1_0 r₀
encode_1_0 (TextAppend l₀ r₀) =
    TList [ TInt 3, TInt 6, l₁, r₁ ]
  where
    l₁ = encode_1_0 l₀
    r₁ = encode_1_0 r₀
encode_1_0 (ListAppend l₀ r₀) =
    TList [ TInt 3, TInt 7, l₁, r₁ ]
  where
    l₁ = encode_1_0 l₀
    r₁ = encode_1_0 r₀
encode_1_0 (Combine l₀ r₀) =
    TList [ TInt 3, TInt 8, l₁, r₁ ]
  where
    l₁ = encode_1_0 l₀
    r₁ = encode_1_0 r₀
encode_1_0 (Prefer l₀ r₀) =
    TList [ TInt 3, TInt 9, l₁, r₁ ]
  where
    l₁ = encode_1_0 l₀
    r₁ = encode_1_0 r₀
encode_1_0 (CombineTypes l₀ r₀) =
    TList [ TInt 3, TInt 10, l₁, r₁ ]
  where
    l₁ = encode_1_0 l₀
    r₁ = encode_1_0 r₀
encode_1_0 (ImportAlt l₀ r₀) =
    TList [ TInt 3, TInt 11, l₁, r₁ ]
  where
    l₁ = encode_1_0 l₀
    r₁ = encode_1_0 r₀
encode_1_0 (ListLit _T₀ xs₀)
    | null xs₀  = TList [ TInt 4, _T₁ ]
    | otherwise = TList ([ TInt 4, TNull ] ++ xs₁)
  where
    _T₁ = case _T₀ of
        Nothing -> TNull
        Just t  -> encode_1_0 t

    xs₁ = map encode_1_0 (Data.Foldable.toList xs₀)
encode_1_0 (OptionalLit _T₀ Nothing) =
    TList [ TInt 5, _T₁ ]
  where
    _T₁ = encode_1_0 _T₀
encode_1_0 (OptionalLit _T₀ (Just t₀)) =
    TList [ TInt 5, _T₁, t₁ ]
  where
    _T₁ = encode_1_0 _T₀
    t₁  = encode_1_0 t₀
encode_1_0 (Merge t₀ u₀ Nothing) =
    TList [ TInt 6, t₁, u₁ ]
  where
    t₁ = encode_1_0 t₀
    u₁ = encode_1_0 u₀
encode_1_0 (Merge t₀ u₀ (Just _T₀)) =
    TList [ TInt 6, t₁, u₁, _T₁ ]
  where
    t₁  = encode_1_0 t₀
    u₁  = encode_1_0 u₀
    _T₁ = encode_1_0 _T₀
encode_1_0 (Record xTs₀) =
    TList [ TInt 7, TMap xTs₁ ]
  where
    xTs₁ = do
        (x₀, _T₀) <- Data.HashMap.Strict.InsOrd.toList xTs₀
        let x₁  = TString x₀
        let _T₁ = encode_1_0 _T₀
        return (x₁, _T₁)
encode_1_0 (RecordLit xts₀) =
    TList [ TInt 8, TMap xts₁ ]
  where
    xts₁ = do
        (x₀, t₀) <- Data.HashMap.Strict.InsOrd.toList xts₀
        let x₁ = TString x₀
        let t₁ = encode_1_0 t₀
        return (x₁, t₁)
encode_1_0 (Field t₀ x) =
    TList [ TInt 9, t₁, TString x ]
  where
    t₁ = encode_1_0 t₀
encode_1_0 (Project t₀ xs₀) =
    TList ([ TInt 10, t₁ ] ++ xs₁)
  where
    t₁  = encode_1_0 t₀
    xs₁ = map TString (Data.Foldable.toList xs₀)
encode_1_0 (Union xTs₀) =
    TList [ TInt 11, TMap xTs₁ ]
  where
    xTs₁ = do
        (x₀, _T₀) <- Data.HashMap.Strict.InsOrd.toList xTs₀
        let x₁  = TString x₀
        let _T₁ = encode_1_0 _T₀
        return (x₁, _T₁)
encode_1_0 (UnionLit x t₀ yTs₀) =
    TList [ TInt 12, TString x, t₁, TMap yTs₁ ]
  where
    t₁ = encode_1_0 t₀

    yTs₁ = do
        (y₀, _T₀) <- Data.HashMap.Strict.InsOrd.toList yTs₀
        let y₁  = TString y₀
        let _T₁ = encode_1_0 _T₀
        return (y₁, _T₁)
encode_1_0 (Constructors u₀) =
    TList [ TInt 13, u₁ ]
  where
    u₁ = encode_1_0 u₀
encode_1_0 (BoolLit b) =
    TBool b
encode_1_0 (BoolIf t₀ l₀ r₀) =
    TList [ TInt 14, t₁, l₁, r₁ ]
  where
    t₁ = encode_1_0 t₀
    l₁ = encode_1_0 l₀
    r₁ = encode_1_0 r₀
encode_1_0 (NaturalLit n) =
    TList [ TInt 15, TInteger (fromIntegral n) ]
encode_1_0 (IntegerLit n) =
    TList [ TInt 16, TInteger n ]
encode_1_0 (DoubleLit n) =
    TList [ TInt 17, TTagged 4 (TList [ TInt exponent, TInteger mantissa ]) ]
  where
    normalized = Data.Scientific.normalize n

    exponent = Data.Scientific.base10Exponent normalized

    mantissa = Data.Scientific.coefficient normalized
encode_1_0 (TextLit (Chunks xys₀ z₀)) =
    TList ([ TInt 18 ] ++ xys₁ ++ [ z₁ ])
  where
    xys₁ = do
        (x₀, y₀) <- xys₀
        let x₁ = TString x₀
        let y₁ = encode_1_0 y₀
        [ x₁, y₁ ]

    z₁ = TString z₀
encode_1_0 (Embed x) =
    importToTerm x
encode_1_0 (Let x Nothing a₀ b₀) =
    TList [ TInt 25, TString x, a₁, b₁ ]
  where
    a₁ = encode_1_0 a₀
    b₁ = encode_1_0 b₀
encode_1_0 (Let x (Just _A₀) a₀ b₀) =
    TList [ TInt 25, TString x, _A₁, a₁, b₁ ]
  where
    a₁  = encode_1_0 a₀
    _A₁ = encode_1_0 _A₀
    b₁  = encode_1_0 b₀
encode_1_0 (Annot t₀ _T₀) =
    TList [ TInt 26, t₁, _T₁ ]
  where
    t₁  = encode_1_0 t₀
    _T₁ = encode_1_0 _T₀
encode_1_0 (Note _ e) =
    encode_1_0 e

importToTerm :: Import -> Term
importToTerm import_ =
    case importType of
        Remote (URL { scheme = scheme₀, ..}) ->
            TList
                (   [ TInt 24, TInt scheme₁, TString authority ]
                ++  map TString (reverse components)
                ++  [ TString file ]
                ++  (case query    of Nothing -> [ TNull ]; Just q -> [ TString q ])
                ++  (case fragment of Nothing -> [ TNull ]; Just f -> [ TString f ])
                )
          where
            scheme₁ = case scheme₀ of
                HTTP  -> 0
                HTTPS -> 1
            File {..} = path

            Directory {..} = directory

        Local prefix₀ path ->
                TList
                    (   [ TInt 24, TInt prefix₁ ]
                    ++  map TString components₁
                    ++  [ TString file ]
                    )
          where
            File {..} = path

            Directory {..} = directory

            (prefix₁, components₁) = case (prefix₀, reverse components) of
                (Absolute, rest       ) -> (2, rest)
                (Here    , ".." : rest) -> (4, rest)
                (Here    , rest       ) -> (3, rest)
                (Home    , rest       ) -> (5, rest)

        Env x ->
            TList [ TInt 24, TInt 6, TString x ]

        Missing ->
            TList [ TInt 24, TInt 7 ]
  where
    Import {..} = import_

    ImportHashed {..} = importHashed

decode_1_0 :: Term -> Maybe (Expr s Import)
decode_1_0 (TInt n) =
    return (Var (V "_" (fromIntegral n)))
decode_1_0 (TInteger n) =
    return (Var (V "_" n))
decode_1_0 (TString "Natural/build") =
    return NaturalBuild
decode_1_0 (TString "Natural/fold") =
    return NaturalFold
decode_1_0 (TString "Natural/isZero") =
    return NaturalIsZero
decode_1_0 (TString "Natural/even") =
    return NaturalEven
decode_1_0 (TString "Natural/odd") =
    return NaturalOdd
decode_1_0 (TString "Natural/toInteger") =
    return NaturalToInteger
decode_1_0 (TString "Natural/show") =
    return NaturalShow
decode_1_0 (TString "Integer/toDouble") =
    return IntegerToDouble
decode_1_0 (TString "Integer/show") =
    return IntegerShow
decode_1_0 (TString "Double/show") =
    return DoubleShow
decode_1_0 (TString "List/build") =
    return ListBuild
decode_1_0 (TString "List/fold") =
    return ListFold
decode_1_0 (TString "List/length") =
    return ListLength
decode_1_0 (TString "List/head") =
    return ListHead
decode_1_0 (TString "List/last") =
    return ListLast
decode_1_0 (TString "List/indexed") =
    return ListIndexed
decode_1_0 (TString "List/reverse") =
    return ListReverse
decode_1_0 (TString "Optional/fold") =
    return OptionalFold
decode_1_0 (TString "Optional/build") =
    return OptionalBuild
decode_1_0 (TString "Bool") =
    return Bool
decode_1_0 (TString "Optional") =
    return Optional
decode_1_0 (TString "Natural") =
    return Natural
decode_1_0 (TString "Integer") =
    return Integer
decode_1_0 (TString "Double") =
    return Double
decode_1_0 (TString "Text") =
    return Text
decode_1_0 (TString "List") =
    return List
decode_1_0 (TString "Type") =
    return (Const Type)
decode_1_0 (TString "Kind") =
    return (Const Kind)
decode_1_0 (TString x) =
    return (Var (V x 0))
decode_1_0 (TList [ TString x, TInt n ]) =
    return (Var (V x (fromIntegral n)))
decode_1_0 (TList [ TString x, TInteger n ]) =
    return (Var (V x n))
decode_1_0 (TList (TInt 0 : f₁ : xs₁)) = do
    f₀  <- decode_1_0 f₁
    xs₀ <- traverse decode_1_0 xs₁
    return (foldl App f₀ xs₀)
decode_1_0 (TList [ TInt 1, _A₁, b₁ ]) = do
    _A₀ <- decode_1_0 _A₁
    b₀  <- decode_1_0 b₁
    return (Lam "_" _A₀ b₀)
decode_1_0 (TList [ TInt 1, TString x, _A₁, b₁ ]) = do
    _A₀ <- decode_1_0 _A₁
    b₀  <- decode_1_0 b₁
    return (Lam x _A₀ b₀)
decode_1_0 (TList [ TInt 2, _A₁, _B₁ ]) = do
    _A₀ <- decode_1_0 _A₁
    _B₀ <- decode_1_0 _B₁
    return (Pi "_" _A₀ _B₀)
decode_1_0 (TList [ TInt 2, TString x, _A₁, _B₁ ]) = do
    _A₀ <- decode_1_0 _A₁
    _B₀ <- decode_1_0 _B₁
    return (Pi x _A₀ _B₀)
decode_1_0 (TList [ TInt 3, TInt n, l₁, r₁ ]) = do
    l₀ <- decode_1_0 l₁
    r₀ <- decode_1_0 r₁
    op <- case n of
            0  -> return BoolOr
            1  -> return BoolAnd
            2  -> return BoolEQ
            3  -> return BoolNE
            4  -> return NaturalPlus
            5  -> return NaturalTimes
            6  -> return TextAppend
            7  -> return ListAppend
            8  -> return Combine
            9  -> return Prefer
            10 -> return CombineTypes
            11 -> return ImportAlt
            _  -> empty
    return (op l₀ r₀)
decode_1_0 (TList [ TInt 4, _T₁ ]) = do
    _T₀ <- decode_1_0 _T₁
    return (ListLit (Just _T₀) empty)
decode_1_0 (TList (TInt 4 : TNull : xs₁ )) = do
    xs₀ <- traverse decode_1_0 xs₁
    return (ListLit Nothing (Data.Sequence.fromList xs₀))
decode_1_0 (TList [ TInt 5, _T₁ ]) = do
    _T₀ <- decode_1_0 _T₁
    return (OptionalLit _T₀ Nothing)
decode_1_0 (TList [ TInt 5, _T₁, t₁ ]) = do
    _T₀ <- decode_1_0 _T₁
    t₀  <- decode_1_0 t₁
    return (OptionalLit _T₀ (Just t₀))
decode_1_0 (TList [ TInt 6, t₁, u₁ ]) = do
    t₀ <- decode_1_0 t₁
    u₀ <- decode_1_0 u₁
    return (Merge t₀ u₀ Nothing)
decode_1_0 (TList [ TInt 6, t₁, u₁, _T₁ ]) = do
    t₀  <- decode_1_0 t₁
    u₀  <- decode_1_0 u₁
    _T₀ <- decode_1_0 _T₁
    return (Merge t₀ u₀ (Just _T₀))
decode_1_0 (TList [ TInt 7, TMap xTs₁ ]) = do
    let process (TString x, _T₁) = do
            _T₀ <- decode_1_0 _T₁

            return (x, _T₀)
        process _ =
            empty

    xTs₀ <- traverse process xTs₁

    return (Record (Data.HashMap.Strict.InsOrd.fromList xTs₀))
decode_1_0 (TList [ TInt 8, TMap xts₁ ]) = do
    let process (TString x, t₁) = do
           t₀ <- decode_1_0 t₁

           return (x, t₀)
        process _ =
            empty

    xts₀ <- traverse process xts₁

    return (RecordLit (Data.HashMap.Strict.InsOrd.fromList xts₀))
decode_1_0 (TList [ TInt 9, t₁, TString x ]) = do
    t₀ <- decode_1_0 t₁

    return (Field t₀ x)
decode_1_0 (TList (TInt 10 : t₁ : xs₁)) = do
    t₀ <- decode_1_0 t₁

    let process (TString x) = return x
        process  _          = empty

    xs₀ <- traverse process xs₁

    return (Project t₀ (Data.Set.fromList xs₀))
decode_1_0 (TList [ TInt 11, TMap xTs₁ ]) = do
    let process (TString x, _T₁) = do
            _T₀ <- decode_1_0 _T₁

            return (x, _T₀)
        process _ =
            empty

    xTs₀ <- traverse process xTs₁

    return (Union (Data.HashMap.Strict.InsOrd.fromList xTs₀))
decode_1_0 (TList [ TInt 12, TString x, t₁, TMap yTs₁ ]) = do
    t₀ <- decode_1_0 t₁

    let process (TString y, _T₁) = do
            _T₀ <- decode_1_0 _T₁

            return (y, _T₀)
        process _ =
            empty

    yTs₀ <- traverse process yTs₁

    return (UnionLit x t₀ (Data.HashMap.Strict.InsOrd.fromList yTs₀))
decode_1_0 (TList [ TInt 13, u₁ ]) = do
    u₀ <- decode_1_0 u₁

    return (Constructors u₀)
decode_1_0 (TBool b) = do
    return (BoolLit b)
decode_1_0 (TList [ TInt 14, t₁, l₁, r₁ ]) = do
    t₀ <- decode_1_0 t₁
    l₀ <- decode_1_0 l₁
    r₀ <- decode_1_0 r₁

    return (BoolIf t₀ l₀ r₀)
decode_1_0 (TList [ TInt 15, TInt n ]) = do
    return (NaturalLit (fromIntegral n))
decode_1_0 (TList [ TInt 15, TInteger n ]) = do
    return (NaturalLit (fromInteger n))
decode_1_0 (TList [ TInt 16, TInt n ]) = do
    return (IntegerLit (fromIntegral n))
decode_1_0 (TList [ TInt 16, TInteger n ]) = do
    return (IntegerLit n)
decode_1_0 (TList [ TInt 17, TTagged 4 (TList [ TInt exponent, TInteger mantissa ]) ]) = do
    return (DoubleLit (Data.Scientific.scientific mantissa exponent))
decode_1_0 (TList [ TInt 17, TTagged 4 (TList [ TInt exponent, TInt mantissa ]) ]) = do
    return (DoubleLit (Data.Scientific.scientific (fromIntegral mantissa) exponent))
decode_1_0 (TList (TInt 18 : xs)) = do
    let process (TString x : y₁ : zs) = do
            y₀ <- decode_1_0 y₁

            ~(xys, z) <- process zs

            return ((x, y₀) : xys, z)
        process [ TString z ] = do
            return ([], z)
        process _ = do
            empty

    (xys, z) <- process xs

    return (TextLit (Chunks xys z))
decode_1_0 (TList (TInt 24 : TInt n : xs)) = do
    let remote scheme = do
            let process [ TString file, q, f ] = do
                    query <- case q of
                        TNull     -> return Nothing
                        TString x -> return (Just x)
                        _         -> empty
                    fragment <- case f of
                        TNull     -> return Nothing
                        TString x -> return (Just x)
                        _         -> empty
                    return ([], file, query, fragment)
                process (TString path : ys) = do
                    (paths, file, query, fragment) <- process ys
                    return (path : paths, file, query, fragment)
                process _ = do
                    empty

            (authority, paths, file, query, fragment) <- case xs of
                TString authority : ys -> do
                    (paths, file, query, fragment) <- process ys
                    return (authority, paths, file, query, fragment)
                _                      -> empty

            let components = reverse paths
            let directory  = Directory {..}
            let path       = File {..}
            let headers    = Nothing

            return (Remote (URL {..}))

    let local prefix = do
            let process [ TString file ] = do
                    return ([], file)
                process (TString path : ys) = do
                    (paths, file) <- process ys
                    return (path : paths, file)
                process _ =
                    empty

            (paths, file) <- process xs

            let finalPaths = case n of
                    4 -> ".." : paths
                    _ -> paths

            let components = reverse finalPaths
            let directory  = Directory {..}

            return (Local prefix (File {..}))

    let env = do
            case xs of
                [ TString x ] -> return (Env x)
                _             -> empty

    let missing = return Missing

    importType <- case n of
        0 -> remote HTTP
        1 -> remote HTTPS
        2 -> local Absolute
        3 -> local Here
        4 -> local Here
        5 -> local Home
        6 -> env
        7 -> missing
        _ -> empty

    let hash         = Nothing
    let importHashed = ImportHashed {..}
    let importMode   = Code
    return (Embed (Import {..}))
decode_1_0 (TList [ TInt 25, TString x, a₁, b₁ ]) = do
    a₀ <- decode_1_0 a₁
    b₀ <- decode_1_0 b₁
    return (Let x Nothing a₀ b₀)
decode_1_0 (TList [ TInt 25, TString x, _A₁, a₁, b₁ ]) = do
    _A₀ <- decode_1_0 _A₁
    a₀  <- decode_1_0 a₁
    b₀  <- decode_1_0 b₁
    return (Let x (Just _A₀) a₀ b₀)
decode_1_0 (TList [ TInt 26, t₁, _T₁ ]) = do
    t₀  <- decode_1_0 t₁
    _T₀ <- decode_1_0 _T₁
    return (Annot t₀ _T₀)
decode_1_0 _ =
    empty

-- | Encode a Dhall expression using protocol version @1.0@
encodeWithVersion_1_0 :: Expr s Import -> Term
encodeWithVersion_1_0 expression =
    TList [ TString "1.0", encode_1_0 expression ]

{-| Decode a Dhall expression

    This auto-detects whiich protocol version to decode based on the included
    protocol version string in the decoded expression
-}
decode :: Term -> Maybe (Expr s Import)
decode term = do
    (version, subTerm) <- case term of
        TList [ TString version, subTerm ] ->
            return (version, subTerm)
        _ ->
            fail ("Cannot decode the version from this decoded CBOR expression: " <> show term)

    maybeExpression <- case version of
        "1.0" -> do
            return (decode_1_0 subTerm)
        _ -> do
            fail ("This decoded version is not supported: " <> Data.Text.unpack version)

    case maybeExpression of
        Nothing ->
            fail ("This decoded CBOR expression does not represent a valid Dhall expression: " <> show subTerm)
        Just expression ->
            return expression

-- | Encode a Dhall expression using the specified `ProtocolVersion`
encode :: ProtocolVersion -> Expr s Import -> Term
encode V_1_0 = encodeWithVersion_1_0
