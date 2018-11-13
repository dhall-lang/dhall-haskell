{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}

{-| This module contains logic for converting Dhall expressions to and from
    CBOR expressions which can in turn be converted to and from a binary
    representation
-}

module Dhall.Binary
    ( -- * Standard versions
      StandardVersion(..)
    , defaultStandardVersion
    , parseStandardVersion

    -- * Encoding and decoding
    , encodeWithVersion
    , decodeWithVersion

    -- * Exceptions
    , DecodingFailure(..)
    ) where

import Codec.CBOR.Term (Term(..))
import Control.Applicative (empty)
import Control.Exception (Exception)
import Dhall.Core
    ( Binding(..)
    , Chunks(..)
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
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid ((<>))
import Data.Text (Text)
import Options.Applicative (Parser)
import Prelude hiding (exponent)
import GHC.Float (double2Float, float2Double)
import Codec.CBOR.Magic (floatToWord16, wordToFloat16)

import qualified Data.Foldable
import qualified Data.Sequence
import qualified Data.Text
import qualified Dhall.Map
import qualified Dhall.Set
import qualified Options.Applicative

-- | Supported version strings
data StandardVersion
    = V_3_0_0
    -- ^ Version "3.0.0"

defaultStandardVersion :: StandardVersion
defaultStandardVersion = V_3_0_0

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
            "3.0.0" -> return V_3_0_0
            _       -> fail "Unsupported version"

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

encode :: Expr s Import -> Term
encode (Var (V "_" n)) =
    TInteger n
encode (Var (V x 0)) =
    TString x
encode (Var (V x n)) =
    TList [ TString x, TInteger n ]
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
encode (Lam "_" _A₀ b₀) =
    TList [ TInt 1, _A₁, b₁ ]
  where
    _A₁ = encode _A₀
    b₁  = encode b₀
encode (Lam x _A₀ b₀) =
    TList [ TInt 1, TString x, _A₁, b₁ ]
  where
    _A₁ = encode _A₀
    b₁  = encode b₀
encode (Pi "_" _A₀ _B₀) =
    TList [ TInt 2, _A₁, _B₁ ]
  where
    _A₁ = encode _A₀
    _B₁ = encode _B₀
encode (Pi x _A₀ _B₀) =
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
encode (ImportAlt l₀ r₀) =
    TList [ TInt 3, TInt 11, l₁, r₁ ]
  where
    l₁ = encode l₀
    r₁ = encode r₀
encode (ListLit _T₀ xs₀)
    | null xs₀  = TList [ TInt 4, _T₁ ]
    | otherwise = TList ([ TInt 4, TNull ] ++ xs₁)
  where
    _T₁ = case _T₀ of
        Nothing -> TNull
        Just t  -> encode t

    xs₁ = map encode (Data.Foldable.toList xs₀)
encode (OptionalLit _T₀ Nothing) =
    TList [ TInt 5, _T₁ ]
  where
    _T₁ = encode _T₀
encode (OptionalLit _T₀ (Just t₀)) =
    TList [ TInt 5, _T₁, t₁ ]
  where
    _T₁ = encode _T₀
    t₁  = encode t₀
encode (Some t₀) =
    TList [ TInt 5, TNull, t₁ ]
  where
    t₁ = encode t₀
encode (Merge t₀ u₀ Nothing) =
    TList [ TInt 6, t₁, u₁ ]
  where
    t₁ = encode t₀
    u₁ = encode u₀
encode (Merge t₀ u₀ (Just _T₀)) =
    TList [ TInt 6, t₁, u₁, _T₁ ]
  where
    t₁  = encode t₀
    u₁  = encode u₀
    _T₁ = encode _T₀
encode (Record xTs₀) =
    TList [ TInt 7, TMap xTs₁ ]
  where
    xTs₁ = do
        (x₀, _T₀) <- Dhall.Map.toList xTs₀
        let x₁  = TString x₀
        let _T₁ = encode _T₀
        return (x₁, _T₁)
encode (RecordLit xts₀) =
    TList [ TInt 8, TMap xts₁ ]
  where
    xts₁ = do
        (x₀, t₀) <- Dhall.Map.toList xts₀
        let x₁ = TString x₀
        let t₁ = encode t₀
        return (x₁, t₁)
encode (Field t₀ x) =
    TList [ TInt 9, t₁, TString x ]
  where
    t₁ = encode t₀
encode (Project t₀ xs₀) =
    TList ([ TInt 10, t₁ ] ++ xs₁)
  where
    t₁  = encode t₀
    xs₁ = map TString (Dhall.Set.toList xs₀)
encode (Union xTs₀) =
    TList [ TInt 11, TMap xTs₁ ]
  where
    xTs₁ = do
        (x₀, _T₀) <- Dhall.Map.toList xTs₀
        let x₁  = TString x₀
        let _T₁ = encode _T₀
        return (x₁, _T₁)
encode (UnionLit x t₀ yTs₀) =
    TList [ TInt 12, TString x, t₁, TMap yTs₁ ]
  where
    t₁ = encode t₀

    yTs₁ = do
        (y₀, _T₀) <- Dhall.Map.toList yTs₀
        let y₁  = TString y₀
        let _T₁ = encode _T₀
        return (y₁, _T₁)
encode (Constructors u₀) =
    TList [ TInt 13, u₁ ]
  where
    u₁ = encode u₀
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
    n16      = floatToWord16 n32
    useFloat = n64 == float2Double n32
    useHalf  = n64 == (float2Double . wordToFloat16 . fromIntegral) n16
encode (TextLit (Chunks xys₀ z₀)) =
    TList ([ TInt 18 ] ++ xys₁ ++ [ z₁ ])
  where
    xys₁ = do
        (x₀, y₀) <- xys₀
        let x₁ = TString x₀
        let y₁ = encode y₀
        [ x₁, y₁ ]

    z₁ = TString z₀
encode (Embed x) =
    importToTerm x
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
encode (Note _ e) =
    encode e

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

decode :: Term -> Maybe (Expr s Import)
decode (TInt n) =
    return (Var (V "_" (fromIntegral n)))
decode (TInteger n) =
    return (Var (V "_" n))
decode (TString "Natural/build") =
    return NaturalBuild
decode (TString "Natural/fold") =
    return NaturalFold
decode (TString "Natural/isZero") =
    return NaturalIsZero
decode (TString "Natural/even") =
    return NaturalEven
decode (TString "Natural/odd") =
    return NaturalOdd
decode (TString "Natural/toInteger") =
    return NaturalToInteger
decode (TString "Natural/show") =
    return NaturalShow
decode (TString "Integer/toDouble") =
    return IntegerToDouble
decode (TString "Integer/show") =
    return IntegerShow
decode (TString "Double/show") =
    return DoubleShow
decode (TString "List/build") =
    return ListBuild
decode (TString "List/fold") =
    return ListFold
decode (TString "List/length") =
    return ListLength
decode (TString "List/head") =
    return ListHead
decode (TString "List/last") =
    return ListLast
decode (TString "List/indexed") =
    return ListIndexed
decode (TString "List/reverse") =
    return ListReverse
decode (TString "Optional/fold") =
    return OptionalFold
decode (TString "Optional/build") =
    return OptionalBuild
decode (TString "Bool") =
    return Bool
decode (TString "Optional") =
    return Optional
decode (TString "None") =
    return None
decode (TString "Natural") =
    return Natural
decode (TString "Integer") =
    return Integer
decode (TString "Double") =
    return Double
decode (TString "Text") =
    return Text
decode (TString "List") =
    return List
decode (TString "Type") =
    return (Const Type)
decode (TString "Kind") =
    return (Const Kind)
decode (TString "Sort") =
    return (Const Sort)
decode (TString x) =
    return (Var (V x 0))
decode (TList [ TString x, TInt n ]) =
    return (Var (V x (fromIntegral n)))
decode (TList [ TString x, TInteger n ]) =
    return (Var (V x n))
decode (TList (TInt 0 : f₁ : xs₁)) = do
    f₀  <- decode f₁
    xs₀ <- traverse decode xs₁
    return (foldl App f₀ xs₀)
decode (TList [ TInt 1, _A₁, b₁ ]) = do
    _A₀ <- decode _A₁
    b₀  <- decode b₁
    return (Lam "_" _A₀ b₀)
decode (TList [ TInt 1, TString x, _A₁, b₁ ]) = do
    _A₀ <- decode _A₁
    b₀  <- decode b₁
    return (Lam x _A₀ b₀)
decode (TList [ TInt 2, _A₁, _B₁ ]) = do
    _A₀ <- decode _A₁
    _B₀ <- decode _B₁
    return (Pi "_" _A₀ _B₀)
decode (TList [ TInt 2, TString x, _A₁, _B₁ ]) = do
    _A₀ <- decode _A₁
    _B₀ <- decode _B₁
    return (Pi x _A₀ _B₀)
decode (TList [ TInt 3, TInt n, l₁, r₁ ]) = do
    l₀ <- decode l₁
    r₀ <- decode r₁
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
decode (TList [ TInt 4, _T₁ ]) = do
    _T₀ <- decode _T₁
    return (ListLit (Just _T₀) empty)
decode (TList (TInt 4 : TNull : xs₁ )) = do
    xs₀ <- traverse decode xs₁
    return (ListLit Nothing (Data.Sequence.fromList xs₀))
decode (TList [ TInt 5, _T₁ ]) = do
    _T₀ <- decode _T₁
    return (OptionalLit _T₀ Nothing)
decode (TList [ TInt 5, TNull, t₁ ]) = do
    t₀ <- decode t₁
    return (Some t₀)
decode (TList [ TInt 5, _T₁, t₁ ]) = do
    _T₀ <- decode _T₁
    t₀  <- decode t₁
    return (OptionalLit _T₀ (Just t₀))
decode (TList [ TInt 6, t₁, u₁ ]) = do
    t₀ <- decode t₁
    u₀ <- decode u₁
    return (Merge t₀ u₀ Nothing)
decode (TList [ TInt 6, t₁, u₁, _T₁ ]) = do
    t₀  <- decode t₁
    u₀  <- decode u₁
    _T₀ <- decode _T₁
    return (Merge t₀ u₀ (Just _T₀))
decode (TList [ TInt 7, TMap xTs₁ ]) = do
    let process (TString x, _T₁) = do
            _T₀ <- decode _T₁

            return (x, _T₀)
        process _ =
            empty

    xTs₀ <- traverse process xTs₁

    return (Record (Dhall.Map.fromList xTs₀))
decode (TList [ TInt 8, TMap xts₁ ]) = do
    let process (TString x, t₁) = do
           t₀ <- decode t₁

           return (x, t₀)
        process _ =
            empty

    xts₀ <- traverse process xts₁

    return (RecordLit (Dhall.Map.fromList xts₀))
decode (TList [ TInt 9, t₁, TString x ]) = do
    t₀ <- decode t₁

    return (Field t₀ x)
decode (TList (TInt 10 : t₁ : xs₁)) = do
    t₀ <- decode t₁

    let process (TString x) = return x
        process  _          = empty

    xs₀ <- traverse process xs₁

    return (Project t₀ (Dhall.Set.fromList xs₀))
decode (TList [ TInt 11, TMap xTs₁ ]) = do
    let process (TString x, _T₁) = do
            _T₀ <- decode _T₁

            return (x, _T₀)
        process _ =
            empty

    xTs₀ <- traverse process xTs₁

    return (Union (Dhall.Map.fromList xTs₀))
decode (TList [ TInt 12, TString x, t₁, TMap yTs₁ ]) = do
    t₀ <- decode t₁

    let process (TString y, _T₁) = do
            _T₀ <- decode _T₁

            return (y, _T₀)
        process _ =
            empty

    yTs₀ <- traverse process yTs₁

    return (UnionLit x t₀ (Dhall.Map.fromList yTs₀))
decode (TList [ TInt 13, u₁ ]) = do
    u₀ <- decode u₁

    return (Constructors u₀)
decode (TBool b) = do
    return (BoolLit b)
decode (TList [ TInt 14, t₁, l₁, r₁ ]) = do
    t₀ <- decode t₁
    l₀ <- decode l₁
    r₀ <- decode r₁

    return (BoolIf t₀ l₀ r₀)
decode (TList [ TInt 15, TInt n ]) = do
    return (NaturalLit (fromIntegral n))
decode (TList [ TInt 15, TInteger n ]) = do
    return (NaturalLit (fromInteger n))
decode (TList [ TInt 16, TInt n ]) = do
    return (IntegerLit (fromIntegral n))
decode (TList [ TInt 16, TInteger n ]) = do
    return (IntegerLit n)
decode (THalf n) = do
    return (DoubleLit (float2Double n))
decode (TFloat n) = do
    return (DoubleLit (float2Double n))
decode (TDouble n) = do
    return (DoubleLit n)
decode (TList (TInt 18 : xs)) = do
    let process (TString x : y₁ : zs) = do
            y₀ <- decode y₁

            ~(xys, z) <- process zs

            return ((x, y₀) : xys, z)
        process [ TString z ] = do
            return ([], z)
        process _ = do
            empty

    (xys, z) <- process xs

    return (TextLit (Chunks xys z))
decode (TList (TInt 24 : TInt n : xs)) = do
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
decode (TList (TInt 25 : xs)) = do
    let process (TString x : _A₁ : a₁ : ls₁) = do
            mA₀ <- case _A₁ of
                TNull -> return Nothing
                _     -> fmap Just (decode _A₁)

            a₀  <- decode a₁

            let binding = Binding x mA₀ a₀

            case ls₁ of
                [ b₁ ] -> do
                    b₀ <- decode b₁

                    return (Let (binding :| []) b₀)
                _ -> do
                    Let (l₀ :| ls₀) b₀ <- process ls₁

                    return (Let (binding :| (l₀ : ls₀)) b₀)
        process _ = do
            empty

    process xs
decode (TList [ TInt 26, t₁, _T₁ ]) = do
    t₀  <- decode t₁
    _T₀ <- decode _T₁
    return (Annot t₀ _T₀)
decode _ =
    empty

{-| Decode a Dhall expression

    This auto-detects which standard version to decode based on the included
    standard version string in the decoded expression
-}
decodeWithVersion :: Term -> Either DecodingFailure (Expr s Import)
decodeWithVersion term = do
    (version, subTerm) <- case term of
        TList [ TString version, subTerm ] ->
            return (version, subTerm)
        _ ->
            fail ("Cannot decode the version from this decoded CBOR expression: " <> show term)

    case version of
        "3.0.0" -> do
            return ()
        _ -> do
            fail ("This decoded version is not supported: " <> Data.Text.unpack version)

    case decode subTerm of
        Nothing ->
            fail ("This decoded CBOR expression does not represent a valid Dhall expression: " <> show subTerm)
        Just expression ->
            return expression

-- | Encode a Dhall expression using the specified `Version`
encodeWithVersion :: StandardVersion -> Expr s Import -> Term
encodeWithVersion V_3_0_0 expression =
    TList [ TString "3.0.0", encode expression ]

data DecodingFailure
    = CannotDecodeVersionString Term
    | UnsupportedVersionString Text
    | CBORIsNotDhall Term
    deriving (Eq)

instance Exception DecodingFailure

_ERROR :: String
_ERROR = "\ESC[1;31mError\ESC[0m"

instance Show DecodingFailure where
    show (CannotDecodeVersionString term) =
            _ERROR <> ": Cannot decode version string\n"
        <>  "\n"
        <>  "This CBOR expression does not contain a version string in any\n"
        <>  "recognizable format\n"
        <>  "\n"
        <>  "↳ " <> show term <> "\n"
    show (UnsupportedVersionString version) =
            _ERROR <> ": Unsupported version string\n"
        <>  "\n"
        <>  "The encoded Dhall expression was tagged with a version string of:\n"
        <>  "\n"
        <>  "↳ " <> show version <> "\n"
        <>  "\n"
        <>  "... but this implementation cannot decode that version\n"
        <>  "\n"
        <>  "Some common reasons why you might get this error:\n"
        <>  "\n"
        <>  "● You are using an old version of the interpreter and need to upgrade\n"
    show (CBORIsNotDhall term) =
            _ERROR <> ": Cannot decode CBOR to Dhall\n"
        <>  "\n"
        <>  "The following CBOR expression does not encode a valid Dhall expression\n"
        <>  "\n"
        <>  "↳ " <> show term <> "\n"
