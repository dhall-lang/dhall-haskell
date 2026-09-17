{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Experimental DAG CBOR (Phase 7). Default semantic hash remains the
-- legacy tree encoding until the coordinated Phase 8 switch.
--
-- Envelope: @["Dhall-DAG", 1, nodes, root]@. Each node is encoded with the
-- existing expression CBOR labels, with child edges as @Var \"_\" n@ referring
-- to @nodes[n]@. Canonical node order is depth-first postorder after a
-- sharing-on re-intern, so bytes do not depend on arena allocation order.
module Dhall.Core.DAG.CBOR
    ( encodeGraph
    , decodeGraph
    , hashGraphLegacy
    ) where

import Codec.Serialise (Serialise (decode, encode))
import Control.Monad   (unless, when)
import Control.Monad.ST (ST, runST)
import Data.ByteString (ByteString)
import Data.Foldable   (foldl', toList)
import Data.Functor.Identity (Identity (..))
import Data.STRef      (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Void       (Void)
import Data.Word       (Word32)
import Dhall.Core.DAG
    ( Builder
    , CoreGraph
    , CoreNode (..)
    , NodeId (..)
    , SharingMode (..)
    , childIds
    , freeze
    , intern
    , internFrom
    , lookupNode
    , newBuilder
    , rootId
    , toSharedExpr
    , traverseIds
    )
import Dhall.Crypto    (SHA256Digest, sha256Hash)
import Dhall.Pretty.Internal (ChooseCharacterSet (..))
import Dhall.Syntax
    ( Chunks (..)
    , Expr (..)
    , RecordField (..)
    , Var (..)
    , makeFieldSelection
    , makeFunctionBinding
    , makeRecordField
    )

import qualified Codec.CBOR.Decoding  as Decoding
import qualified Codec.CBOR.Encoding  as Encoding
import qualified Codec.CBOR.Read      as Read
import qualified Codec.CBOR.Write     as Write
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Sequence        as Seq
import qualified Data.Vector          as V
import qualified Dhall.Binary
import qualified Dhall.Map
import qualified Dhall.Syntax         as Syntax

-- | SHA-256 of the current (legacy tree) CBOR of a graph. Unchanged goldens.
hashGraphLegacy :: CoreGraph Void -> SHA256Digest
hashGraphLegacy =
    sha256Hash . Lazy.toStrict . Dhall.Binary.encodeExpression . toSharedExpr

-- Hard cap so a compact malicious envelope cannot allocate unbounded nodes.
maxDagNodes :: Int
maxDagNodes = 1000000

-- | Canonical DAG CBOR: @["Dhall-DAG", 1, nodes, root]@.
encodeGraph :: CoreGraph Void -> ByteString
encodeGraph g0 =
    Write.toStrictByteString encoding
  where
    (nodes, root) = canonicalize g0
    encoding =
            Encoding.encodeListLen 4
        <>  Encoding.encodeString "Dhall-DAG"
        <>  Encoding.encodeInt 1
        <>  Encoding.encodeListLen (fromIntegral (length nodes))
        <>  mconcat (map (encode . nodeToPlaceholder) nodes)
        <>  Encoding.encodeInt (fromIntegral root)

decodeGraph :: ByteString -> Either String (CoreGraph Void)
decodeGraph bytes =
    case Read.deserialiseFromBytes decodeEnvelope (Lazy.fromStrict bytes) of
        Left err        -> Left (show err)
        Right ("", g)   -> Right g
        Right _         -> Left "Dhall.Core.DAG.CBOR.decodeGraph: trailing data"

decodeEnvelope :: Decoding.Decoder s (CoreGraph Void)
decodeEnvelope = do
    n <- Decoding.decodeListLen
    unless (n == 4) (fail "DAG CBOR envelope must have 4 elements")
    tag <- Decoding.decodeString
    unless (tag == "Dhall-DAG") (fail "expected Dhall-DAG tag")
    ver <- Decoding.decodeInt
    unless (ver == 1) (fail "unsupported DAG CBOR version")
    len <- Decoding.decodeListLen
    when (len < 1) (fail "DAG CBOR node table is empty")
    when (len > maxDagNodes) (fail "DAG CBOR node count exceeds limit")
    nodes <- mapM (\_ -> decode) [1 .. len]
    root <- Decoding.decodeInt
    case internPlaceholders nodes (fromIntegral root) of
        Left err -> fail err
        Right g  -> return g

-- Re-intern with sharing on, then number nodes by DFS postorder over
-- canonical child order. The root is last.
canonicalize :: CoreGraph Void -> ([CoreNode Void], Word32)
canonicalize g0 = runST $ do
    b <- newBuilder SharingOn
    r <- internFrom b g0 (rootId g0)
    g <- freeze b r
    let order = postorder g
        oldToNew = HashMap.fromList (zip order [0 :: Word32 ..])
        remap (NodeId i) = NodeId (oldToNew HashMap.! i)
        remapNode node = runIdentity (traverseIds (Identity . remap) node)
        nodes = [ remapNode (lookupNode g (NodeId i)) | i <- order ]
        root' = oldToNew HashMap.! unNodeId (rootId g)
    return (nodes, root')

postorder :: CoreGraph Void -> [Word32]
postorder g = toList (snd (go (rootId g) (HashMap.empty, Seq.empty)))
  where
    go (NodeId i) (seen, acc) =
        case HashMap.lookup i seen of
            Just () -> (seen, acc)
            Nothing ->
                let seen1 = HashMap.insert i () seen
                    kids = childIds (lookupNode g (NodeId i))
                    (seen2, acc2) = foldl' (\st c -> go c st) (seen1, acc) kids
                in  (seen2, acc2 Seq.|> i)

internPlaceholders :: [Expr Void Void] -> Word32 -> Either String (CoreGraph Void)
internPlaceholders exprs root = runST $ do
    let bound = length exprs
    if fromIntegral root >= bound
        then return (Left "root index out of range")
        else do
            b <- newBuilder SharingOn
            vecRef <- newSTRef V.empty
            result <- internAll b vecRef 0 exprs
            case result of
                Left err -> return (Left err)
                Right () -> do
                    ids <- readSTRef vecRef
                    Right <$> freeze b (ids V.! fromIntegral root)

internAll
    :: Builder s Void
    -> STRef s (V.Vector NodeId)
    -> Int
    -> [Expr Void Void]
    -> ST s (Either String ())
internAll _ _ _ [] = return (Right ())
internAll b vecRef i (e:es) = do
    prev <- readSTRef vecRef
    r <- internOne b prev i e
    case r of
        Left err -> return (Left err)
        Right nid -> do
            writeSTRef vecRef (V.snoc prev nid)
            internAll b vecRef (i + 1) es

-- | Resolve child @Var \"_\" n@ against already-interned nodes @0..i-1@.
-- The top-level node is always interned, even if it is itself a @Var@.
internOne
    :: Builder s Void
    -> V.Vector NodeId
    -> Int
    -> Expr Void Void
    -> ST s (Either String NodeId)
internOne b prev i e =
    case exprToNode (resolveChild prev i) e of
        Left err   -> return (Left err)
        Right node -> Right <$> intern b node

resolveChild :: V.Vector NodeId -> Int -> Expr Void Void -> Either String NodeId
resolveChild prev i = \case
    Var (V "_" n)
        | n >= 0 && fromIntegral n < i && fromIntegral n < V.length prev ->
            Right (prev V.! fromIntegral n)
        | otherwise ->
            Left "DAG CBOR child reference is out of range or not child-before-parent"
    _ ->
        Left "DAG CBOR child field is not a node reference"

exprToNode
    :: (Expr Void Void -> Either String NodeId)
    -> Expr Void Void
    -> Either String (CoreNode Void)
exprToNode go = \case
    Const c -> Right (CConst c)
    Var v -> Right (CVar v)
    Lam _ fb body ->
        CLam (Syntax.functionBindingVariable fb) <$> go (Syntax.functionBindingAnnotation fb) <*> go body
    Pi _ x a c -> CPi x <$> go a <*> go c
    App f x -> CApp <$> go f <*> go x
    Let (Syntax.Binding _ x _ mAnn _ v) body ->
        CLet x <$> traverse (go . snd) mAnn <*> go v <*> go body
    Annot x t -> CAnnot <$> go x <*> go t
    Bool -> Right CBool
    BoolLit x -> Right (CBoolLit x)
    BoolAnd x y -> CBoolAnd <$> go x <*> go y
    BoolOr x y -> CBoolOr <$> go x <*> go y
    BoolEQ x y -> CBoolEQ <$> go x <*> go y
    BoolNE x y -> CBoolNE <$> go x <*> go y
    BoolIf x y z -> CBoolIf <$> go x <*> go y <*> go z
    Bytes -> Right CBytes
    BytesLit x -> Right (CBytesLit x)
    Natural -> Right CNatural
    NaturalLit n -> Right (CNaturalLit n)
    NaturalPlus x y -> CNaturalPlus <$> go x <*> go y
    NaturalTimes x y -> CNaturalTimes <$> go x <*> go y
    Integer -> Right CInteger
    IntegerLit n -> Right (CIntegerLit n)
    Double -> Right CDouble
    DoubleLit n -> Right (CDoubleLit n)
    Text -> Right CText
    TextLit (Chunks cs z) -> CTextLit <$> traverse (\(t, e) -> (,) t <$> go e) cs <*> pure z
    TextAppend x y -> CTextAppend <$> go x <*> go y
    Date -> Right CDate
    DateLiteral d -> Right (CDateLiteral d)
    Time -> Right CTime
    TimeLiteral a b c d e -> Right (CTimeLiteral a b c d e)
    TimeZone -> Right CTimeZone
    TimeZoneLiteral z -> Right (CTimeZoneLiteral z)
    List -> Right CList
    ListLit m xs -> CListLit <$> traverse go m <*> traverse go (toList xs)
    ListAppend x y -> CListAppend <$> go x <*> go y
    Optional -> Right COptional
    Some x -> CSome <$> go x
    None -> Right CNone
    Record kts ->
        CRecord <$> traverse (\(k, rf) -> (,) k <$> go (recordFieldValue rf)) (Dhall.Map.toList kts)
    RecordLit kts ->
        CRecordLit <$> traverse (\(k, rf) -> (,) k <$> go (recordFieldValue rf)) (Dhall.Map.toList kts)
    Union kts ->
        CUnion <$> traverse (\(k, m) -> (,) k <$> traverse go m) (Dhall.Map.toList kts)
    Combine _ m x y -> CCombine m <$> go x <*> go y
    CombineTypes _ x y -> CCombineTypes <$> go x <*> go y
    Prefer _ p x y -> CPrefer p <$> go x <*> go y
    RecordCompletion x y -> CRecordCompletion <$> go x <*> go y
    Merge x y m -> CMerge <$> go x <*> go y <*> traverse go m
    ToMap x m -> CToMap <$> go x <*> traverse go m
    ShowConstructor x -> CShowConstructor <$> go x
    Field x l -> CField <$> go x <*> pure (Syntax.fieldSelectionLabel l)
    Project x e -> CProject <$> go x <*> traverse go e
    Assert x -> CAssert <$> go x
    Equivalent _ x y -> CEquivalent <$> go x <*> go y
    With x p y -> CWith <$> go x <*> pure p <*> go y
    ImportAlt x y -> CImportAlt <$> go x <*> go y
    Embed a -> Right (CEmbed a)
    Note _ e -> exprToNode go e

nodeToPlaceholder :: CoreNode Void -> Expr Void Void
nodeToPlaceholder = \case
    CConst c -> Const c
    CVar v -> Var v
    CLam x t b -> Lam AutoInferCharSet (makeFunctionBinding x (ref t)) (ref b)
    CPi x a b -> Pi AutoInferCharSet x (ref a) (ref b)
    CApp f x -> App (ref f) (ref x)
    CLet x mA v b ->
        let ann = fmap (\t -> (Nothing, ref t)) mA
        in  Let (Syntax.Binding Nothing x Nothing ann Nothing (ref v)) (ref b)
    CAnnot x t -> Annot (ref x) (ref t)
    CBool -> Bool
    CBoolLit x -> BoolLit x
    CBoolAnd x y -> BoolAnd (ref x) (ref y)
    CBoolOr x y -> BoolOr (ref x) (ref y)
    CBoolEQ x y -> BoolEQ (ref x) (ref y)
    CBoolNE x y -> BoolNE (ref x) (ref y)
    CBoolIf x y z -> BoolIf (ref x) (ref y) (ref z)
    CBytes -> Bytes
    CBytesLit x -> BytesLit x
    CNatural -> Natural
    CNaturalLit n -> NaturalLit n
    CNaturalPlus x y -> NaturalPlus (ref x) (ref y)
    CNaturalTimes x y -> NaturalTimes (ref x) (ref y)
    CInteger -> Integer
    CIntegerLit n -> IntegerLit n
    CDouble -> Double
    CDoubleLit n -> DoubleLit n
    CText -> Text
    CTextLit cs z -> TextLit (Chunks [ (t, ref n) | (t, n) <- cs ] z)
    CTextAppend x y -> TextAppend (ref x) (ref y)
    CDate -> Date
    CDateLiteral d -> DateLiteral d
    CTime -> Time
    CTimeLiteral a b c d e -> TimeLiteral a b c d e
    CTimeZone -> TimeZone
    CTimeZoneLiteral z -> TimeZoneLiteral z
    CList -> List
    CListLit m xs -> ListLit (fmap ref m) (Seq.fromList (map ref xs))
    CListAppend x y -> ListAppend (ref x) (ref y)
    COptional -> Optional
    CSome x -> Some (ref x)
    CNone -> None
    CRecord kvs -> Record (Dhall.Map.fromList [ (k, makeRecordField (ref n)) | (k, n) <- kvs ])
    CRecordLit kvs -> RecordLit (Dhall.Map.fromList [ (k, makeRecordField (ref n)) | (k, n) <- kvs ])
    CUnion kts -> Union (Dhall.Map.fromList [ (k, fmap ref m) | (k, m) <- kts ])
    CCombine m x y -> Combine AutoInferCharSet m (ref x) (ref y)
    CPrefer p x y -> Prefer AutoInferCharSet p (ref x) (ref y)
    CCombineTypes x y -> CombineTypes AutoInferCharSet (ref x) (ref y)
    CRecordCompletion x y -> RecordCompletion (ref x) (ref y)
    CMerge x y m -> Merge (ref x) (ref y) (fmap ref m)
    CToMap x m -> ToMap (ref x) (fmap ref m)
    CShowConstructor x -> ShowConstructor (ref x)
    CField x l -> Field (ref x) (makeFieldSelection l)
    CProject x e -> Project (ref x) (fmap ref e)
    CAssert x -> Assert (ref x)
    CEquivalent x y -> Equivalent AutoInferCharSet (ref x) (ref y)
    CWith x p y -> With (ref x) p (ref y)
    CImportAlt x y -> ImportAlt (ref x) (ref y)
    CEmbed a -> Embed a
  where
    ref (NodeId i) = Var (V "_" (fromIntegral i))
