{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| This module contains logic for converting Dhall expressions to and from
    CBOR expressions which can in turn be converted to and from a binary
    representation
-}

module Dhall.Binary
    ( -- * Standard versions
      StandardVersion(..)
    , renderStandardVersion

    -- * Encoding and decoding
    , encodeExpression
    , decodeExpression

    -- * Exceptions
    , DecodingFailure(..)
    ) where

import Codec.CBOR.Decoding (Decoder, TokenType(..))
import Codec.CBOR.Encoding (Encoding)
import Codec.Serialise (Serialise(encode, decode))
import Control.Applicative (empty, (<|>))
import Control.Exception (Exception)
import Data.ByteString.Lazy (ByteString)
import Dhall.Syntax
    ( Binding(..)
    , Chunks(..)
    , Const(..)
    , Directory(..)
    , DhallDouble(..)
    , Expr(..)
    , File(..)
    , FilePrefix(..)
    , Import(..)
    , ImportHashed(..)
    , ImportMode(..)
    , ImportType(..)
    , MultiLet(..)
    , Scheme(..)
    , URL(..)
    , Var(..)
    )

import Data.Foldable (toList, foldl')
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Void (Void, absurd)
import GHC.Float (double2Float, float2Double)

import qualified Codec.CBOR.Decoding  as Decoding
import qualified Codec.CBOR.Encoding  as Encoding
import qualified Codec.CBOR.Read      as Read
import qualified Codec.Serialise      as Serialise
import qualified Control.Monad        as Monad
import qualified Data.ByteArray
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Text            as Text
import qualified DhallList
import qualified Dhall.Syntax
import qualified Dhall.Crypto
import qualified Dhall.Map
import qualified Dhall.Set
import qualified Text.Printf          as Printf

{-| Supported version strings

    This exists primarily for backwards compatibility for expressions encoded
    before Dhall removed version tags from the binary encoding
-}
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

-- | Render a `StandardVersion` as `Text`
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
unApply :: Expr s a -> (Expr s a, [Expr s a])
unApply e₀ = (baseFunction₀, diffArguments₀ [])
  where
    ~(baseFunction₀, diffArguments₀) = go e₀

    go (App f a) = (baseFunction, diffArguments . (a :))
      where
        ~(baseFunction, diffArguments) = go f

    go (Note _ e) = go e

    go baseFunction = (baseFunction, id)

decodeExpressionInternal :: (Int -> Decoder s a) -> Decoder s (Expr t a)
decodeExpressionInternal decodeEmbed = go
  where
    go = do
        let die message = fail ("Dhall.Binary.decodeExpressionInternal: " <> message)

        tokenType₀ <- Decoding.peekTokenType

        case tokenType₀ of
            TypeUInt -> do
                !n <- Decoding.decodeWord

                return (Var (V "_" (fromIntegral n)))

            TypeUInt64 -> do
                !n <- Decoding.decodeWord64

                return (Var (V "_" (fromIntegral n)))

            TypeFloat16 -> do
                !n <- Decoding.decodeFloat

                return (DoubleLit (DhallDouble (float2Double n)))

            TypeFloat32 -> do
                !n <- Decoding.decodeFloat

                return (DoubleLit (DhallDouble (float2Double n)))

            TypeFloat64 -> do
                !n <- Decoding.decodeDouble

                return (DoubleLit (DhallDouble n))

            TypeBool -> do
                !b <- Decoding.decodeBool

                return (BoolLit b)

            TypeString -> do
                s <- Decoding.decodeString

                case s of
                    "Natural/build"     -> return NaturalBuild
                    "Natural/fold"      -> return NaturalFold
                    "Natural/isZero"    -> return NaturalIsZero
                    "Natural/even"      -> return NaturalEven
                    "Natural/odd"       -> return NaturalOdd
                    "Natural/toInteger" -> return NaturalToInteger
                    "Natural/show"      -> return NaturalShow
                    "Natural/subtract"  -> return NaturalSubtract
                    "Integer/toDouble"  -> return IntegerToDouble
                    "Integer/clamp"     -> return IntegerClamp
                    "Integer/negate"    -> return IntegerNegate
                    "Integer/show"      -> return IntegerShow
                    "Double/show"       -> return DoubleShow
                    "List/build"        -> return ListBuild
                    "List/fold"         -> return ListFold
                    "List/length"       -> return ListLength
                    "List/head"         -> return ListHead
                    "List/last"         -> return ListLast
                    "List/indexed"      -> return ListIndexed
                    "List/reverse"      -> return ListReverse
                    "Optional/fold"     -> return OptionalFold
                    "Optional/build"    -> return OptionalBuild
                    "Bool"              -> return Bool
                    "Optional"          -> return Optional
                    "None"              -> return None
                    "Natural"           -> return Natural
                    "Integer"           -> return Integer
                    "Double"            -> return Double
                    "Text"              -> return Text
                    "Text/show"         -> return TextShow
                    "List"              -> return List
                    "Type"              -> return (Const Type)
                    "Kind"              -> return (Const Kind)
                    "Sort"              -> return (Const Sort)
                    _                   -> die ("Unrecognized built-in: " <> Text.unpack s)

            TypeListLen -> do
                len <- Decoding.decodeListLen

                case len of
                    0 -> die "Missing tag"
                    _ -> return ()

                tokenType₁ <- Decoding.peekTokenType

                case tokenType₁ of
                    TypeString -> do
                        x <- Decoding.decodeString

                        if x == "_"
                            then die "Non-standard encoding of an α-normalized variable"
                            else return ()

                        tokenType₂ <- Decoding.peekTokenType

                        case tokenType₂ of
                            TypeUInt -> do
                                !n <- Decoding.decodeWord

                                return (Var (V x (fromIntegral n)))

                            TypeUInt64 -> do
                                !n <- Decoding.decodeWord64

                                return (Var (V x (fromIntegral n)))

                            _ -> do
                                die ("Unexpected token type for variable index: " <> show tokenType₂)

                    TypeUInt -> do
                        tag <- Decoding.decodeWord

                        case tag of
                            0 -> do
                                f <- go

                                xs <- Monad.replicateM (len - 2) go

                                if null xs
                                    then die "Non-standard encoding of a function with no arguments"
                                    else return ()

                                return (foldl' App f xs)

                            1 -> do
                                case len of
                                    3 -> do
                                        _A <- go

                                        b <- go

                                        return (Lam "_" _A b)

                                    4 -> do
                                        x <- Decoding.decodeString

                                        if x == "_"
                                            then die "Non-standard encoding of a λ expression"
                                            else return ()

                                        _A <- go

                                        b <- go

                                        return (Lam x _A b)

                                    _ -> do
                                        die ("Incorrect number of tokens used to encode a λ expression: " <> show len)

                            2 -> do
                                case len of
                                    3 -> do
                                        _A <- go

                                        _B <- go

                                        return (Pi "_" _A _B)

                                    4 -> do
                                        x <- Decoding.decodeString

                                        if x == "_"
                                            then die "Non-standard encoding of a ∀ expression"
                                            else return ()

                                        _A <- go

                                        _B <- go

                                        return (Pi x _A _B)

                                    _ -> do
                                        die ("Incorrect number of tokens used to encode a ∀ expression: " <> show len)

                            3 -> do
                                opcode <- Decoding.decodeWord

                                op <- case opcode of
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
                                    12 -> return Equivalent
                                    13 -> return RecordCompletion
                                    _  -> die ("Unrecognized operator code: " <> show opcode)

                                l <- go

                                r <- go

                                return (op l r)

                            4 -> do
                                case len of
                                    2 -> do
                                        _T <- go

                                        return (ListLit (Just (App List _T)) empty)

                                    _ -> do
                                        Decoding.decodeNull

                                        xs <- DhallList.replicateM (len - 2) go
                                        return (ListLit Nothing xs)

                            5 -> do
                                Decoding.decodeNull

                                t <- go

                                return (Some t)

                            6 -> do
                                t <- go

                                u <- go

                                case len of
                                    3 -> do
                                        return (Merge t u Nothing)

                                    4 -> do
                                        _T <- go

                                        return (Merge t u (Just _T))

                                    _ -> do
                                        die ("Incorrect number of tokens used to encode a `merge` expression: " <> show len)

                            7 -> do
                                mapLength <- Decoding.decodeMapLen

                                xTs <- Monad.replicateM mapLength $ do
                                    x <- Decoding.decodeString

                                    _T <- go

                                    return (x, _T)

                                return (Record (Dhall.Map.fromList xTs))

                            8 -> do
                                mapLength <- Decoding.decodeMapLen

                                xts <- Monad.replicateM mapLength $ do
                                    x <- Decoding.decodeString

                                    t <- go

                                    return (x, t)

                                return (RecordLit (Dhall.Map.fromList xts))

                            9 -> do
                                t <- go

                                x <- Decoding.decodeString

                                return (Field t x)

                            10 -> do
                                t <- go

                                xs <- case len of
                                    3 -> do
                                        tokenType₂ <- Decoding.peekTokenType

                                        case tokenType₂ of
                                            TypeListLen -> do
                                                _ <- Decoding.decodeListLen

                                                _T <- go

                                                return (Right _T)

                                            TypeString -> do
                                                x <- Decoding.decodeString
                                                return (Left (Dhall.Set.fromList [x]))

                                            _ -> do
                                                die ("Unexpected token type for projection: " <> show tokenType₂)

                                    _ -> do
                                        xs <- Monad.replicateM (len - 2) Decoding.decodeString

                                        return (Left (Dhall.Set.fromList xs))

                                return (Project t xs)

                            11 -> do
                                mapLength <- Decoding.decodeMapLen

                                xTs <- Monad.replicateM mapLength $ do
                                    x <- Decoding.decodeString

                                    tokenType₂ <- Decoding.peekTokenType

                                    mT <- case tokenType₂ of
                                        TypeNull -> do
                                            Decoding.decodeNull

                                            return Nothing

                                        _ -> do
                                            _T <- go

                                            return (Just _T)

                                    return (x, mT)

                                return (Union (Dhall.Map.fromList xTs))

                            14 -> do
                                t <- go

                                l <- go

                                r <- go

                                return (BoolIf t l r)

                            15 -> do
                                tokenType₂ <- Decoding.peekTokenType

                                case tokenType₂ of
                                    TypeUInt -> do
                                        n <- Decoding.decodeWord

                                        return (NaturalLit (fromIntegral n))

                                    TypeUInt64 -> do
                                        n <- Decoding.decodeWord64

                                        return (NaturalLit (fromIntegral n))

                                    TypeInteger -> do
                                        n <- Decoding.decodeInteger
                                        return (NaturalLit (fromIntegral n))

                                    _ -> do
                                        die ("Unexpected token type for Natural literal: " <> show tokenType₂)

                            16 -> do
                                tokenType₂ <- Decoding.peekTokenType

                                case tokenType₂ of
                                    TypeUInt -> do
                                        n <- Decoding.decodeWord

                                        return (IntegerLit (fromIntegral n))

                                    TypeUInt64 -> do
                                        n <- Decoding.decodeWord64

                                        return (IntegerLit (fromIntegral n))

                                    TypeNInt -> do
                                        n <- Decoding.decodeNegWord

                                        return (IntegerLit (-1 - fromIntegral n))

                                    TypeNInt64 -> do
                                        n <- Decoding.decodeNegWord64

                                        return (IntegerLit (-1 - fromIntegral n))
                                    TypeInteger -> do
                                        n <- Decoding.decodeInteger
                                        return (IntegerLit n)

                                    _ -> do
                                        die ("Unexpected token type for Integer literal: " <> show tokenType₂)

                            18 -> do
                                xys <- Monad.replicateM ((len - 2) `quot` 2) $ do
                                    x <- Decoding.decodeString

                                    y <- go

                                    return (x, y)

                                z <- Decoding.decodeString

                                return (TextLit (Chunks xys z))

                            19 -> do
                                t <- go

                                return (Assert t)

                            24 -> do
                                fmap Embed (decodeEmbed len)

                            25 -> do
                                bindings <- Monad.replicateM ((len - 2) `quot` 3) $ do
                                    x <- Decoding.decodeString

                                    tokenType₂ <- Decoding.peekTokenType

                                    mA <- case tokenType₂ of
                                        TypeNull -> do
                                            Decoding.decodeNull

                                            return Nothing

                                        _ -> do
                                            _A <- go

                                            return (Just (Nothing, _A))

                                    a <- go

                                    return (Binding Nothing x Nothing mA Nothing a)

                                b <- go

                                return (foldr Let b bindings)

                            26 -> do
                                t <- go

                                _T <- go

                                return (Annot t _T)

                            27 -> do
                                t <- go

                                mT <- case len of
                                    2 -> do
                                        return Nothing

                                    3 -> do
                                        _T <- go

                                        return (Just _T)

                                    _ -> do
                                        die ("Incorrect number of tokens used to encode a type annotation: " <> show len)

                                return (ToMap t mT)

                            28 -> do
                                _T <- go

                                return (ListLit (Just _T) empty)

                            _ -> do
                                die ("Unexpected tag: " <> show tag)

                    _ -> do
                        die ("Unexpected tag type: " <> show tokenType₁)

            _ -> do
                die ("Unexpected initial token: " <> show tokenType₀)

encodeExpressionInternal :: (a -> Encoding) -> Expr Void a -> Encoding
encodeExpressionInternal encodeEmbed = go
  where
    go e = case e of
        Var (V "_" n) ->
            Encoding.encodeInt n

        Var (V x n) ->
                Encoding.encodeListLen 2
            <>  Encoding.encodeString x
            <>  Encoding.encodeInt n

        NaturalBuild ->
            Encoding.encodeString "Natural/build"

        NaturalFold ->
            Encoding.encodeString "Natural/fold"

        NaturalIsZero ->
            Encoding.encodeString "Natural/isZero"

        NaturalEven ->
            Encoding.encodeString "Natural/even"

        NaturalOdd ->
            Encoding.encodeString "Natural/odd"

        NaturalToInteger ->
            Encoding.encodeString "Natural/toInteger"

        NaturalShow ->
            Encoding.encodeString "Natural/show"

        NaturalSubtract ->
            Encoding.encodeString "Natural/subtract"

        IntegerToDouble ->
            Encoding.encodeString "Integer/toDouble"

        IntegerClamp ->
            Encoding.encodeString "Integer/clamp"

        IntegerNegate ->
            Encoding.encodeString "Integer/negate"

        IntegerShow ->
            Encoding.encodeString "Integer/show"

        DoubleShow ->
            Encoding.encodeString "Double/show"

        ListBuild ->
            Encoding.encodeString "List/build"

        ListFold ->
            Encoding.encodeString "List/fold"

        ListLength ->
            Encoding.encodeString "List/length"

        ListHead ->
            Encoding.encodeString "List/head"

        ListLast ->
            Encoding.encodeString "List/last"

        ListIndexed ->
            Encoding.encodeString "List/indexed"

        ListReverse ->
            Encoding.encodeString "List/reverse"

        OptionalFold ->
            Encoding.encodeString "Optional/fold"

        OptionalBuild ->
            Encoding.encodeString "Optional/build"

        Bool ->
            Encoding.encodeString "Bool"

        Optional ->
            Encoding.encodeString "Optional"

        None ->
            Encoding.encodeString "None"

        Natural ->
            Encoding.encodeString "Natural"

        Integer ->
            Encoding.encodeString "Integer"

        Double ->
            Encoding.encodeString "Double"

        Text ->
            Encoding.encodeString "Text"

        TextShow ->
            Encoding.encodeString "Text/show"

        List ->
            Encoding.encodeString "List"

        Const Type ->
            Encoding.encodeString "Type"

        Const Kind ->
            Encoding.encodeString "Kind"

        Const Sort ->
            Encoding.encodeString "Sort"

        a@App{} ->
            encodeList
                ( Encoding.encodeInt 0
                : go function
                : map go arguments
                )
          where
            (function, arguments) = unApply a

        Lam "_" _A b ->
            encodeList3
                (Encoding.encodeInt 1)
                (go _A)
                (go b)

        Lam x _A b ->
            encodeList4
                (Encoding.encodeInt 1)
                (Encoding.encodeString x)
                (go _A)
                (go b)

        Pi "_" _A _B ->
            encodeList3
                (Encoding.encodeInt 2)
                (go _A)
                (go _B)

        Pi x _A _B ->
            encodeList4
                (Encoding.encodeInt 2)
                (Encoding.encodeString x)
                (go _A)
                (go _B)

        BoolOr l r ->
            encodeOperator 0 l r

        BoolAnd l r ->
            encodeOperator 1 l r

        BoolEQ l r ->
            encodeOperator 2 l r

        BoolNE l r ->
            encodeOperator 3 l r

        NaturalPlus l r ->
            encodeOperator 4 l r

        NaturalTimes l r ->
            encodeOperator 5 l r

        TextAppend l r ->
            encodeOperator 6 l r

        ListAppend l r ->
            encodeOperator 7 l r

        Combine l r ->
            encodeOperator 8 l r

        Prefer l r ->
            encodeOperator 9 l r

        CombineTypes l r ->
            encodeOperator 10 l r

        ImportAlt l r ->
            encodeOperator 11 l r

        Equivalent l r ->
            encodeOperator 12 l r

        RecordCompletion l r ->
            encodeOperator 13 l r

        ListLit _T₀ xs
            | null xs ->
                encodeList2 (Encoding.encodeInt label) _T₁
            | otherwise ->
                encodeList -- TODO
                    ( Encoding.encodeInt 4
                    : Encoding.encodeNull
                    : map go (Data.Foldable.toList xs)
                    )
          where
            (label, _T₁) = case _T₀ of
                Nothing           -> (4 , Encoding.encodeNull)
                Just (App List t) -> (4 , go t               )
                Just  t           -> (28, go t               )

        Some t ->
            encodeList3
                (Encoding.encodeInt 5)
                Encoding.encodeNull
                (go t)

        Merge t u Nothing ->
            encodeList3
                (Encoding.encodeInt 6)
                (go t)
                (go u)

        Merge t u (Just _T) ->
            encodeList4
                (Encoding.encodeInt 6)
                (go t)
                (go u)
                (go _T)

        Record xTs ->
            encodeList2
                (Encoding.encodeInt 7)
                (encodeMapWith go xTs)

        RecordLit xts ->
            encodeList2
                (Encoding.encodeInt 8)
                (encodeMapWith go xts)

        Field t x ->
            encodeList3
                (Encoding.encodeInt 9)
                (go t)
                (Encoding.encodeString x)

        Project t (Left xs) ->
            encodeList
                ( Encoding.encodeInt 10
                : go t
                : map Encoding.encodeString (Dhall.Set.toList xs)
                )

        Project t (Right _T) ->
            encodeList3
                (Encoding.encodeInt 10)
                (go t)
                (encodeList1 (go _T))

        Union xTs ->
            encodeList2
                (Encoding.encodeInt 11)
                (encodeMapWith encodeValue xTs)
          where
            encodeValue  Nothing  = Encoding.encodeNull
            encodeValue (Just _T) = go _T

        BoolLit b ->
            Encoding.encodeBool b

        BoolIf t l r ->
            encodeList4
                (Encoding.encodeInt 14)
                (go t)
                (go l)
                (go r)

        NaturalLit n ->
            encodeList2
                (Encoding.encodeInt 15)
                (Encoding.encodeInteger (fromIntegral n))

        IntegerLit n ->
            encodeList2
                (Encoding.encodeInt 16)
                (Encoding.encodeInteger (fromIntegral n))

        DoubleLit (DhallDouble n64)
            | useHalf   -> Encoding.encodeFloat16 n32
            | useFloat  -> Encoding.encodeFloat n32
            | otherwise -> Encoding.encodeDouble n64
          where
            n32 = double2Float n64

            useFloat = n64 == float2Double n32

            useHalf = n64 == 0.0 || n64 == infinity || n64 == -infinity

            infinity = 1/0 :: Double

        -- Fast path for the common case of an uninterpolated string
        TextLit (Chunks [] z) ->
            encodeList2
                (Encoding.encodeInt 18)
                (Encoding.encodeString z)

        TextLit (Chunks xys z) ->
            encodeList
                ( Encoding.encodeInt 18
                : concatMap encodePair xys ++ [ Encoding.encodeString z ]
                )
          where
            encodePair (x, y) = [ Encoding.encodeString x, go y ]

        Assert t ->
            encodeList2
                (Encoding.encodeInt 19)
                (go t)

        Embed x ->
            encodeEmbed x

        Let a₀ b₀ ->
            encodeList
                ( Encoding.encodeInt 25
                : concatMap encodeBinding (toList as) ++ [ go b₁ ]
                )
          where
            MultiLet as b₁ = Dhall.Syntax.multiLet a₀ b₀

            encodeBinding (Binding _ x _ mA₀ _ a) =
                [ Encoding.encodeString x
                , mA₁
                , go a
                ]
              where
                mA₁ = case mA₀ of
                    Nothing      -> Encoding.encodeNull
                    Just (_, _A) -> go _A

        Annot t _T ->
            encodeList3
                (Encoding.encodeInt 26)
                (go t)
                (go _T)

        ToMap t Nothing ->
            encodeList2
                (Encoding.encodeInt 27)
                (go t)

        ToMap t (Just _T) ->
            encodeList3
                (Encoding.encodeInt 27)
                (go t)
                (go _T)

        Note _ b ->
            go b

    encodeOperator n l r =
        encodeList4
            (Encoding.encodeInt 3)
            (Encoding.encodeInt n)
            (go l)
            (go r)

    encodeMapWith encodeValue m =
            Encoding.encodeMapLen (fromIntegral (Dhall.Map.size m))
        <>  foldMap encodeKeyValue (Dhall.Map.toList (Dhall.Map.sort m))
      where
        encodeKeyValue (k, v) = Encoding.encodeString k <> encodeValue v

encodeList1 :: Encoding -> Encoding
encodeList1 a = Encoding.encodeListLen 1 <> a
{-# INLINE encodeList1 #-}

encodeList2 :: Encoding -> Encoding -> Encoding
encodeList2 a b = Encoding.encodeListLen 2 <> a <> b
{-# INLINE encodeList2 #-}

encodeList3 :: Encoding -> Encoding -> Encoding -> Encoding
encodeList3 a b c = Encoding.encodeListLen 3 <> a <> b <> c
{-# INLINE encodeList3 #-}

encodeList4 :: Encoding -> Encoding -> Encoding -> Encoding -> Encoding
encodeList4 a b c d = Encoding.encodeListLen 4 <> a <> b <> c <> d
{-# INLINE encodeList4 #-}

encodeList :: [ Encoding ] -> Encoding
encodeList xs =
    Encoding.encodeListLen (fromIntegral (length xs)) <> mconcat xs
{-# INLINE encodeList #-}

decodeImport :: Int -> Decoder s Import
decodeImport len = do
    let die message = fail ("Dhall.Binary.decodeImport: " <> message)

    tokenType₀ <- Decoding.peekTokenType

    hash <- case tokenType₀ of
        TypeNull -> do
            Decoding.decodeNull

            return Nothing

        TypeBytes -> do
            bytes <- Decoding.decodeBytes

            let (prefix, suffix) = Data.ByteString.splitAt 2 bytes

            case prefix of
                "\x12\x20" -> return ()
                _          -> die ("Unrecognized multihash prefix: " <> show prefix)
            case Dhall.Crypto.sha256DigestFromByteString suffix of
                Nothing     -> die ("Invalid sha256 digest: " <> show bytes)
                Just digest -> return (Just digest)

        _ -> do
            die ("Unexpected hash token: " <> show tokenType₀)

    m <- Decoding.decodeWord

    importMode <- case m of
        0 -> return Code
        1 -> return RawText
        2 -> return Location
        _ -> die ("Unexpected code for import mode: " <> show m)

    let remote scheme = do
            tokenType₁ <- Decoding.peekTokenType

            headers <- case tokenType₁ of
                TypeNull -> do
                    Decoding.decodeNull
                    return Nothing

                _ -> do
                    headers <- decodeExpressionInternal decodeImport

                    return (Just headers)

            authority <- Decoding.decodeString

            paths <- Monad.replicateM (len - 8) Decoding.decodeString

            file <- Decoding.decodeString

            tokenType₂ <- Decoding.peekTokenType

            query <- case tokenType₂ of
                TypeNull -> do
                    Decoding.decodeNull
                    return Nothing
                _ -> do
                    fmap Just Decoding.decodeString

            let components = reverse paths
            let directory  = Directory {..}
            let path       = File {..}

            return (Remote (URL {..}))

    let local prefix = do
            paths <- Monad.replicateM (len - 5) Decoding.decodeString

            file <- Decoding.decodeString

            let components = reverse paths
            let directory  = Directory {..}

            return (Local prefix (File {..}))

    let missing = return Missing

    let env = do
            x <- Decoding.decodeString

            return (Env x)

    n <- Decoding.decodeWord

    importType <- case n of
        0 -> remote HTTP
        1 -> remote HTTPS
        2 -> local Absolute
        3 -> local Here
        4 -> local Parent
        5 -> local Home
        6 -> env
        7 -> missing
        _ -> fail ("Unrecognized import type code: " <> show n)

    let importHashed = ImportHashed {..}

    return (Import {..})

encodeImport :: Import -> Encoding
encodeImport import_ =
    case importType of
        Remote (URL { scheme = scheme₀, .. }) ->
            encodeList
                (   prefix
                ++  [ Encoding.encodeInt scheme₁
                    , using
                    , Encoding.encodeString authority
                    ]
                ++  map Encoding.encodeString (reverse components)
                ++  [ Encoding.encodeString file ]
                ++  [ case query of
                         Nothing -> Encoding.encodeNull
                         Just q  -> Encoding.encodeString q
                    ]
                )
          where
            using = case headers of
                Nothing ->
                    Encoding.encodeNull
                Just h ->
                    encodeExpressionInternal encodeImport (Dhall.Syntax.denote h)

            scheme₁ = case scheme₀ of
                HTTP  -> 0
                HTTPS -> 1

            File{..} = path

            Directory {..} = directory

        Local prefix₀ path ->
            encodeList
                (   prefix
                ++  [ Encoding.encodeInt prefix₁ ]
                ++  map Encoding.encodeString components₁
                ++  [ Encoding.encodeString file ]
                )
          where
            File{..} = path

            Directory{..} = directory

            prefix₁ = case prefix₀ of
                Absolute -> 2
                Here     -> 3
                Parent   -> 4
                Home     -> 5

            components₁ = reverse components

        Env x ->
            encodeList
                (prefix ++ [ Encoding.encodeInt 6, Encoding.encodeString x ])

        Missing ->
            encodeList (prefix ++ [ Encoding.encodeInt 7 ])
  where
    prefix = [ Encoding.encodeInt 24, h, m ]
      where
        h = case hash of
            Nothing ->
                Encoding.encodeNull

            Just digest ->
                Encoding.encodeBytes ("\x12\x20" <> Data.ByteArray.convert digest)

        m = Encoding.encodeInt (case importMode of Code -> 0; RawText -> 1; Location -> 2;)

    Import{..} = import_

    ImportHashed{..} = importHashed

decodeVoid :: Int -> Decoder s Void
decodeVoid _ = fail "Dhall.Binary.decodeVoid: Cannot decode an uninhabited type"

encodeVoid :: Void -> Encoding
encodeVoid = absurd

instance Serialise (Expr Void Void) where
    encode = encodeExpressionInternal encodeVoid

    decode = decodeExpressionInternal decodeVoid

instance Serialise (Expr Void Import) where
    encode = encodeExpressionInternal encodeImport

    decode = decodeExpressionInternal decodeImport

-- | Encode a Dhall expression as a CBOR-encoded `ByteString`
encodeExpression :: Expr Void Import -> ByteString
encodeExpression = Serialise.serialise

-- | Decode a Dhall expression from a CBOR `Term`
decodeExpression
    :: Serialise (Expr s a) => ByteString -> Either DecodingFailure (Expr s a)
decodeExpression bytes =
    case decodeWithoutVersion <|> decodeWithVersion of
        Just expression -> Right expression
        Nothing         -> Left (CBORIsNotDhall bytes)
  where
    adapt (Right ("", x)) = Just x
    adapt  _              = Nothing

    decode' = decodeWith55799Tag decode

    -- This is the behavior specified by the standard
    decodeWithoutVersion = adapt (Read.deserialiseFromBytes decode' bytes)

    -- tag to ease the migration
    decodeWithVersion = adapt (Read.deserialiseFromBytes decodeWithTag bytes)
      where
        decodeWithTag = do
            2 <- Decoding.decodeListLen

            version <- Decoding.decodeString


            -- "_" has never been a valid version string, and this ensures that
            -- we don't interpret `[ "_", 0 ]` as the expression `_` (encoded as
            -- `0`) tagged with a version string of `"_"`
            if (version == "_")
                then fail "Dhall.Binary.decodeExpression: \"_\" is not a valid version string"
                else return ()

            decode'

decodeWith55799Tag :: Decoder s a -> Decoder s a
decodeWith55799Tag decoder = do
    tokenType <- Decoding.peekTokenType

    case tokenType of
        TypeTag -> do
            w <- Decoding.decodeTag

            if w /= 55799
                then fail ("Dhall.Binary.decodeWith55799Tag: Unexpected tag: " <> show w)
                else return ()

            decoder
        _ -> do
            decoder

{-| This indicates that a given CBOR-encoded `ByteString` did not correspond to
    a valid Dhall expression
-}
newtype DecodingFailure = CBORIsNotDhall ByteString
    deriving (Eq)

instance Exception DecodingFailure

_ERROR :: String
_ERROR = "\ESC[1;31mError\ESC[0m"

instance Show DecodingFailure where
    show (CBORIsNotDhall bytes) =
            _ERROR <> ": Cannot decode CBOR to Dhall\n"
        <>  "\n"
        <>  "The following bytes do not encode a valid Dhall expression\n"
        <>  "\n"
        <>  "↳ 0x" <> concatMap toHex (Data.ByteString.Lazy.unpack bytes) <> "\n"
      where
        toHex = Printf.printf "%02x "
