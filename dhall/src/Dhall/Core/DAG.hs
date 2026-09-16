{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal hash-consed DAG of denoted Dhall terms.
--
-- 'NodeId's are scoped to an owning 'CoreGraph'. Interning uses a non-cryptographic
-- hash only as a table index; shallow equality decides identity.
module Dhall.Core.DAG
    ( -- * Graph
      NodeId(..)
    , CoreNode(..)
    , childIds
    , CoreGraph
    , SharingMode(..)
    , GraphMetrics(..)
      -- * Construction
    , fromExpr
    , fromExprWith
    , toSharedExpr
    , compact
    , validate
      -- * Folds and metrics
    , lookupNode
    , rootId
    , nodeCount
    , graphMetrics
    , foldUnique
    , foldOccurrences
      -- * Graph operations
    , shiftGraph
    , substGraph
    , alphaNormalizeGraph
      -- * Builder (Phase 2 quote)
    , Builder
    , newBuilder
    , intern
    , internExpr
    , internGraph
    , internFrom
    , subgraph
    , traverseIds
    , freeze
    , prettyGraph
    ) where

import Control.Monad.ST (ST, runST)
import Data.Foldable    (toList)
import Data.STRef       (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Text        (Text)
import Data.Void        (Void)
import Data.Word        (Word32, Word64)
import Dhall.Core.DAG.Node
    ( CoreNode (..)
    , NodeId (..)
    , childIds
    , edgeCount
    )
import Dhall.Pretty.Internal (ChooseCharacterSet (..), pretty_)
import Dhall.Syntax
    ( Binding (..)
    , Chunks (..)
    , Expr (..)
    , FieldSelection (..)
    , FunctionBinding (..)
    , RecordField (..)
    , Var (..)
    , denote
    , makeFieldSelection
    , makeFunctionBinding
    , makeRecordField
    )
import Prettyprinter (Pretty)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence       as Seq
import qualified Data.Vector         as V
import qualified Dhall.Map
import qualified Dhall.Syntax        as Syntax
import qualified Lens.Micro          as Lens

-- | Whether interned nodes are maximally shared.
data SharingMode = SharingOn | SharingOff
    deriving (Eq, Show)

data BuilderState a = BuilderState
    { bsNodes   :: !(Seq.Seq (CoreNode a))
    , bsTable   :: !(HashMap.HashMap (CoreNode a) Word32)
    , bsSharing :: !Bool
    , bsHits    :: !Word64
    , bsMisses  :: !Word64
    }

-- | Mutable intern table + arena, scoped to one 'ST' run.
newtype Builder s a = Builder (STRef s (BuilderState a))

-- | Frozen DAG: root plus node vector. Intern table is dropped.
data CoreGraph a = CoreGraph
    { graphRoot    :: !NodeId
    , graphNodes   :: !(V.Vector (CoreNode a))
    , graphHits    :: !Word64
    , graphMisses  :: !Word64
    }

-- | Unique-node sizing and intern counters for this graph.
data GraphMetrics = GraphMetrics
    { expandedOccurrences :: !Int
    , uniqueNodes         :: !Int
    , uniqueEdges         :: !Int
    , sharingFactor       :: !Double
    , largestFanOut       :: !Int
    , largestFanIn        :: !Int
    , internHits          :: !Word64
    , internMisses        :: !Word64
    } deriving (Eq, Show)

rootId :: CoreGraph a -> NodeId
rootId = graphRoot

nodeCount :: CoreGraph a -> Int
nodeCount = V.length . graphNodes

lookupNode :: CoreGraph a -> NodeId -> CoreNode a
lookupNode g (NodeId i) = graphNodes g V.! fromIntegral i

newBuilder :: SharingMode -> ST s (Builder s a)
newBuilder mode = do
    ref <- newSTRef BuilderState
        { bsNodes   = Seq.empty
        , bsTable   = HashMap.empty
        , bsSharing = mode == SharingOn
        , bsHits    = 0
        , bsMisses  = 0
        }
    return (Builder ref)

intern :: (Eq a) => Builder s a -> CoreNode a -> ST s NodeId
intern (Builder ref) !node = do
    st@BuilderState{..} <- readSTRef ref
    if bsSharing then
        case HashMap.lookup node bsTable of
            Just i -> do
                writeSTRef ref st { bsHits = bsHits + 1 }
                return (NodeId i)
            Nothing -> append st
    else
        append st
  where
    append st@BuilderState{..} = do
        let i = fromIntegral (Seq.length bsNodes)
        writeSTRef ref st
            { bsNodes  = bsNodes Seq.|> node
            , bsTable  = if bsSharing then HashMap.insert node i bsTable else bsTable
            , bsMisses = bsMisses + 1
            }
        return (NodeId i)

freeze :: Builder s a -> NodeId -> ST s (CoreGraph a)
freeze (Builder ref) root = do
    BuilderState{..} <- readSTRef ref
    let nodes = V.fromList (toList bsNodes)
    return CoreGraph
        { graphRoot  = root
        , graphNodes = nodes
        , graphHits  = bsHits
        , graphMisses = bsMisses
        }

-- | Reintern every reachable node of an existing graph into this builder.
internGraph :: (Eq a) => Builder s a -> CoreGraph a -> ST s NodeId
internGraph b g = internFrom b g (graphRoot g)

-- | Reintern the subgraph reachable from a given node.
internFrom :: (Eq a) => Builder s a -> CoreGraph a -> NodeId -> ST s NodeId
internFrom b g start = do
    memo <- newSTRef (HashMap.empty :: HashMap.HashMap Word32 NodeId)
    let go (NodeId i) = do
            m <- readSTRef memo
            case HashMap.lookup i m of
                Just nid -> return nid
                Nothing -> do
                    let node = lookupNode g (NodeId i)
                    node' <- traverseIds go node
                    nid <- intern b node'
                    modifySTRef' memo (HashMap.insert i nid)
                    return nid
    go start

-- | Frozen copy of the subgraph reachable from 'NodeId'.
subgraph :: (Eq a) => CoreGraph a -> NodeId -> CoreGraph a
subgraph g nid = runST $ do
    b <- newBuilder SharingOn
    root <- internFrom b g nid
    freeze b root

traverseIds :: Monad m => (NodeId -> m NodeId) -> CoreNode a -> m (CoreNode a)
traverseIds f = \case
    CConst c -> pure (CConst c)
    CVar v -> pure (CVar v)
    CLam x t b -> CLam x <$> f t <*> f b
    CPi x a b -> CPi x <$> f a <*> f b
    CApp x y -> CApp <$> f x <*> f y
    CLet x mA v b -> CLet x <$> traverse f mA <*> f v <*> f b
    CAnnot x y -> CAnnot <$> f x <*> f y
    CBool -> pure CBool
    CBoolLit x -> pure (CBoolLit x)
    CBoolAnd x y -> CBoolAnd <$> f x <*> f y
    CBoolOr x y -> CBoolOr <$> f x <*> f y
    CBoolEQ x y -> CBoolEQ <$> f x <*> f y
    CBoolNE x y -> CBoolNE <$> f x <*> f y
    CBoolIf x y z -> CBoolIf <$> f x <*> f y <*> f z
    CBytes -> pure CBytes
    CBytesLit x -> pure (CBytesLit x)
    CNatural -> pure CNatural
    CNaturalLit x -> pure (CNaturalLit x)
    CNaturalPlus x y -> CNaturalPlus <$> f x <*> f y
    CNaturalTimes x y -> CNaturalTimes <$> f x <*> f y
    CInteger -> pure CInteger
    CIntegerLit x -> pure (CIntegerLit x)
    CDouble -> pure CDouble
    CDoubleLit x -> pure (CDoubleLit x)
    CText -> pure CText
    CTextLit cs z -> CTextLit <$> traverse (\(t, n) -> (,) t <$> f n) cs <*> pure z
    CTextAppend x y -> CTextAppend <$> f x <*> f y
    CDate -> pure CDate
    CDateLiteral x -> pure (CDateLiteral x)
    CTime -> pure CTime
    CTimeLiteral a b c d e -> pure (CTimeLiteral a b c d e)
    CTimeZone -> pure CTimeZone
    CTimeZoneLiteral x -> pure (CTimeZoneLiteral x)
    CList -> pure CList
    CListLit m xs -> CListLit <$> traverse f m <*> traverse f xs
    CListAppend x y -> CListAppend <$> f x <*> f y
    COptional -> pure COptional
    CSome x -> CSome <$> f x
    CNone -> pure CNone
    CRecord kvs -> CRecord <$> traverse (\(k, n) -> (,) k <$> f n) kvs
    CRecordLit kvs -> CRecordLit <$> traverse (\(k, n) -> (,) k <$> f n) kvs
    CUnion kts -> CUnion <$> traverse (\(k, m) -> (,) k <$> traverse f m) kts
    CCombine m x y -> CCombine m <$> f x <*> f y
    CCombineTypes x y -> CCombineTypes <$> f x <*> f y
    CPrefer p x y -> CPrefer p <$> f x <*> f y
    CRecordCompletion x y -> CRecordCompletion <$> f x <*> f y
    CMerge x y m -> CMerge <$> f x <*> f y <*> traverse f m
    CToMap x m -> CToMap <$> f x <*> traverse f m
    CShowConstructor x -> CShowConstructor <$> f x
    CField x l -> CField <$> f x <*> pure l
    CProject x e -> CProject <$> f x <*> traverse f e
    CAssert x -> CAssert <$> f x
    CEquivalent x y -> CEquivalent <$> f x <*> f y
    CWith x p y -> CWith <$> f x <*> pure p <*> f y
    CImportAlt x y -> CImportAlt <$> f x <*> f y
    CEmbed x -> pure (CEmbed x)

fromExpr :: (Eq a) => Expr s a -> CoreGraph a
fromExpr = fromExprWith SharingOn

fromExprWith :: (Eq a) => SharingMode -> Expr s a -> CoreGraph a
fromExprWith mode expression = runST $ do
    b <- newBuilder mode
    root <- internExpr b (denote expression)
    freeze b root

internExpr :: (Eq a) => Builder s a -> Expr t a -> ST s NodeId
internExpr b = go
  where
    go = \case
        Note _ e -> go e
        Const c -> intern b (CConst c)
        Var v -> intern b (CVar v)
        Lam _ FunctionBinding{functionBindingVariable = x, functionBindingAnnotation = t} body -> do
            t' <- go t
            body' <- go body
            intern b (CLam x t' body')
        Pi _ x a c -> do
            a' <- go a
            c' <- go c
            intern b (CPi x a' c')
        App f x -> do
            f' <- go f
            x' <- go x
            intern b (CApp f' x')
        Let (Binding _ x _ mAnn _ v) body -> do
            mAnn' <- traverse (go . snd) mAnn
            v' <- go v
            body' <- go body
            intern b (CLet x mAnn' v' body')
        Annot x t -> do
            x' <- go x
            t' <- go t
            intern b (CAnnot x' t')
        Bool -> intern b CBool
        BoolLit x -> intern b (CBoolLit x)
        BoolAnd x y -> bin CBoolAnd x y
        BoolOr x y -> bin CBoolOr x y
        BoolEQ x y -> bin CBoolEQ x y
        BoolNE x y -> bin CBoolNE x y
        BoolIf x y z -> do
            x' <- go x
            y' <- go y
            z' <- go z
            intern b (CBoolIf x' y' z')
        Bytes -> intern b CBytes
        BytesLit x -> intern b (CBytesLit x)
        Natural -> intern b CNatural
        NaturalLit n -> intern b (CNaturalLit n)
        NaturalPlus x y -> bin CNaturalPlus x y
        NaturalTimes x y -> bin CNaturalTimes x y
        Integer -> intern b CInteger
        IntegerLit n -> intern b (CIntegerLit n)
        Double -> intern b CDouble
        DoubleLit n -> intern b (CDoubleLit n)
        Text -> intern b CText
        TextLit (Chunks chunks z) -> do
            chunks' <- traverse (\(t, e) -> (,) t <$> go e) chunks
            intern b (CTextLit chunks' z)
        TextAppend x y -> bin CTextAppend x y
        Date -> intern b CDate
        DateLiteral d -> intern b (CDateLiteral d)
        Time -> intern b CTime
        TimeLiteral hh mm ss frac p -> intern b (CTimeLiteral hh mm ss frac p)
        TimeZone -> intern b CTimeZone
        TimeZoneLiteral z -> intern b (CTimeZoneLiteral z)
        List -> intern b CList
        ListLit m xs -> do
            m' <- traverse go m
            xs' <- traverse go (toList xs)
            intern b (CListLit m' xs')
        ListAppend x y -> bin CListAppend x y
        Optional -> intern b COptional
        Some x -> go x >>= intern b . CSome
        None -> intern b CNone
        Record kts -> do
            kvs <- traverseRecord kts
            intern b (CRecord kvs)
        RecordLit kvs0 -> do
            kvs <- traverseRecord kvs0
            intern b (CRecordLit kvs)
        Union kts -> do
            kvs <- traverse (\(k, m) -> (,) k <$> traverse go m) (Dhall.Map.toList kts)
            intern b (CUnion kvs)
        Combine _ mk x y -> do
            x' <- go x
            y' <- go y
            intern b (CCombine mk x' y')
        CombineTypes _ x y -> bin CCombineTypes x y
        Prefer _ p x y -> do
            x' <- go x
            y' <- go y
            intern b (CPrefer p x' y')
        RecordCompletion x y -> bin CRecordCompletion x y
        Merge x y m -> do
            x' <- go x
            y' <- go y
            m' <- traverse go m
            intern b (CMerge x' y' m')
        ToMap x m -> do
            x' <- go x
            m' <- traverse go m
            intern b (CToMap x' m')
        ShowConstructor x -> go x >>= intern b . CShowConstructor
        Field x (FieldSelection _ l _) -> do
            x' <- go x
            intern b (CField x' l)
        Project x e -> do
            x' <- go x
            e' <- traverse go e
            intern b (CProject x' e')
        Assert x -> go x >>= intern b . CAssert
        Equivalent _ x y -> bin CEquivalent x y
        With x p y -> do
            x' <- go x
            y' <- go y
            intern b (CWith x' p y')
        ImportAlt x y -> bin CImportAlt x y
        Embed a -> intern b (CEmbed a)

    bin cons x y = do
        x' <- go x
        y' <- go y
        intern b (cons x' y')

    traverseRecord kts =
        traverse (\(k, RecordField _ e _ _) -> (,) k <$> go e) (Dhall.Map.toList kts)

-- | Reconstruct a physically shared 'Expr' in time proportional to unique nodes.
toSharedExpr :: CoreGraph a -> Expr Void a
toSharedExpr g =
    decoded V.! fromIntegral (unNodeId (graphRoot g))
  where
    decoded = V.generate (V.length (graphNodes g)) $ \i ->
        decode (graphNodes g V.! i)

    at (NodeId n) = decoded V.! fromIntegral n

    decode = \case
        CConst c -> Const c
        CVar v -> Var v
        CLam x t b -> Lam AutoInferCharSet (makeFunctionBinding x (at t)) (at b)
        CPi x a b -> Pi AutoInferCharSet x (at a) (at b)
        CApp f x -> App (at f) (at x)
        CLet x mA v b ->
            let ann = fmap (\t -> (Nothing, at t)) mA
            in  Let (Binding Nothing x Nothing ann Nothing (at v)) (at b)
        CAnnot x t -> Annot (at x) (at t)
        CBool -> Bool
        CBoolLit x -> BoolLit x
        CBoolAnd x y -> BoolAnd (at x) (at y)
        CBoolOr x y -> BoolOr (at x) (at y)
        CBoolEQ x y -> BoolEQ (at x) (at y)
        CBoolNE x y -> BoolNE (at x) (at y)
        CBoolIf x y z -> BoolIf (at x) (at y) (at z)
        CBytes -> Bytes
        CBytesLit x -> BytesLit x
        CNatural -> Natural
        CNaturalLit n -> NaturalLit n
        CNaturalPlus x y -> NaturalPlus (at x) (at y)
        CNaturalTimes x y -> NaturalTimes (at x) (at y)
        CInteger -> Integer
        CIntegerLit n -> IntegerLit n
        CDouble -> Double
        CDoubleLit n -> DoubleLit n
        CText -> Text
        CTextLit cs z -> TextLit (Chunks [ (t, at n) | (t, n) <- cs ] z)
        CTextAppend x y -> TextAppend (at x) (at y)
        CDate -> Date
        CDateLiteral d -> DateLiteral d
        CTime -> Time
        CTimeLiteral hh mm ss frac p -> TimeLiteral hh mm ss frac p
        CTimeZone -> TimeZone
        CTimeZoneLiteral z -> TimeZoneLiteral z
        CList -> List
        CListLit m xs -> ListLit (fmap at m) (Seq.fromList (map at xs))
        CListAppend x y -> ListAppend (at x) (at y)
        COptional -> Optional
        CSome x -> Some (at x)
        CNone -> None
        CRecord kvs -> Record (fromFields kvs)
        CRecordLit kvs -> RecordLit (fromFields kvs)
        CUnion kts -> Union (Dhall.Map.fromList [ (k, fmap at m) | (k, m) <- kts ])
        CCombine m x y -> Combine AutoInferCharSet m (at x) (at y)
        CCombineTypes x y -> CombineTypes AutoInferCharSet (at x) (at y)
        CPrefer p x y -> Prefer AutoInferCharSet p (at x) (at y)
        CRecordCompletion x y -> RecordCompletion (at x) (at y)
        CMerge x y m -> Merge (at x) (at y) (fmap at m)
        CToMap x m -> ToMap (at x) (fmap at m)
        CShowConstructor x -> ShowConstructor (at x)
        CField x l -> Field (at x) (makeFieldSelection l)
        CProject x e -> Project (at x) (fmap at e)
        CAssert x -> Assert (at x)
        CEquivalent x y -> Equivalent AutoInferCharSet (at x) (at y)
        CWith x p y -> With (at x) p (at y)
        CImportAlt x y -> ImportAlt (at x) (at y)
        CEmbed a -> Embed a

    fromFields kvs =
        Dhall.Map.fromList [ (k, makeRecordField (at n)) | (k, n) <- kvs ]

-- | Render a graph as Dhall text. The 'Expr' skeleton is physically shared;
-- layout still emits repeated source text for repeated occurrences.
prettyGraph :: Pretty a => CoreGraph a -> Text
prettyGraph = pretty_ . toSharedExpr

-- | Drop unreachable nodes and renumber densely from 0 in child-before-parent order.
compact :: (Eq a) => CoreGraph a -> CoreGraph a
compact g = runST $ do
    b <- newBuilder SharingOn
    root <- internGraph b g
    freeze b root

validate :: CoreGraph a -> Either String ()
validate g = do
    let n = nodeCount g
        NodeId root = graphRoot g
    if fromIntegral root >= n then
        Left "root id out of range"
    else
        mapM_ checkNode [0 .. n - 1]
  where
    checkNode i = do
        let nid = NodeId (fromIntegral i)
            node = lookupNode g nid
        mapM_ (checkChild i) (childIds node)
    checkChild parent (NodeId c) = do
        let n = nodeCount g
        if fromIntegral c >= n then
            Left ("child id out of range at node " <> show parent)
        else if fromIntegral c >= parent then
            Left ("child id not before parent at node " <> show parent)
        else
            Right ()

foldUnique :: Monoid m => (CoreNode a -> m) -> CoreGraph a -> m
foldUnique f g = foldMap f (graphNodes g)

-- | Occurrence-sensitive fold: follows every edge, like a tree 'Foldable'.
foldOccurrences :: Monoid m => (CoreNode a -> m) -> CoreGraph a -> m
foldOccurrences f g = go (graphRoot g)
  where
    go nid =
        let node = lookupNode g nid
        in  f node <> foldMap go (childIds node)

graphMetrics :: CoreGraph a -> GraphMetrics
graphMetrics g =
    GraphMetrics
        { expandedOccurrences = occ
        , uniqueNodes = u
        , uniqueEdges = e
        , sharingFactor = if occ == 0 then 1 else fromIntegral occ / fromIntegral u
        , largestFanOut = maxOut
        , largestFanIn = maxIn
        , internHits = graphHits g
        , internMisses = graphMisses g
        }
  where
    u = nodeCount g
    e = V.sum (V.map edgeCount (graphNodes g))
    occ = getSum (foldOccurrences (const (Sum 1)) g)
    maxOut =
        if u == 0 then 0 else V.maximum (V.map edgeCount (graphNodes g))
    fanIn =
        V.accum (+) (V.replicate u 0)
            [ (fromIntegral (unNodeId c), 1)
            | node <- V.toList (graphNodes g)
            , c <- childIds node
            ]
    maxIn = if u == 0 then 0 else V.maximum fanIn

-- Newtype for occurrence counting without importing Data.Monoid in the API.
newtype Sum = Sum { getSum :: Int }
instance Semigroup Sum where
    Sum a <> Sum b = Sum (a + b)
instance Monoid Sum where
    mempty = Sum 0

shiftGraph :: (Eq a) => Int -> Var -> CoreGraph a -> CoreGraph a
shiftGraph d (V x n0) g = runST $ do
    b <- newBuilder SharingOn
    memo <- newSTRef (HashMap.empty :: HashMap.HashMap (Word32, Int) NodeId)
    let go !n nid = do
            m <- readSTRef memo
            let key = (unNodeId nid, n)
            case HashMap.lookup key m of
                Just r -> return r
                Nothing -> do
                    node' <- case lookupNode g nid of
                        CVar (V x' n') ->
                            let n'' = if x == x' && n <= n' then n' + d else n'
                            in  intern b (CVar (V x' n''))
                        CLam y t body -> do
                            t' <- go n t
                            body' <- go (if x == y then n + 1 else n) body
                            intern b (CLam y t' body')
                        CPi y a c -> do
                            a' <- go n a
                            c' <- go (if x == y then n + 1 else n) c
                            intern b (CPi y a' c')
                        CLet y mA v body -> do
                            mA' <- traverse (go n) mA
                            v' <- go n v
                            body' <- go (if x == y then n + 1 else n) body
                            intern b (CLet y mA' v' body')
                        node -> do
                            mapped <- traverseIds (go n) node
                            intern b mapped
                    modifySTRef' memo (HashMap.insert key node')
                    return node'
    root <- go n0 (graphRoot g)
    freeze b root

-- | Graph substitution. Binders in the body still shift the replacement the
-- same way as tree 'subst'; the result is re-interned.
substGraph :: Eq a => Var -> CoreGraph a -> CoreGraph a -> CoreGraph a
substGraph var replacement body =
    fromExpr (substExpr var (toSharedExpr replacement) (toSharedExpr body))

substExpr :: Var -> Expr s a -> Expr s a -> Expr s a
substExpr _ _ (Const a) = Const a
substExpr (V x n) e (Lam cs (FunctionBinding src0 y src1 src2 _A) b) =
    Lam cs (FunctionBinding src0 y src1 src2 _A') b'
  where
    _A' = substExpr (V x n ) e _A
    b'  = substExpr (V x n') (Syntax.shift 1 (V y 0) e) b
    n'  = if x == y then n + 1 else n
substExpr (V x n) e (Pi cs y _A _B) = Pi cs y _A' _B'
  where
    _A' = substExpr (V x n ) e _A
    _B' = substExpr (V x n') (Syntax.shift 1 (V y 0) e) _B
    n'  = if x == y then n + 1 else n
substExpr v e (Var v') = if v == v' then e else Var v'
substExpr (V x n) e (Let (Binding src0 f src1 mt src2 r) b) =
    Let (Binding src0 f src1 mt' src2 r') b'
  where
    b' = substExpr (V x n') (Syntax.shift 1 (V f 0) e) b
      where
        n' = if x == f then n + 1 else n
    mt' = fmap (fmap (substExpr (V x n) e)) mt
    r'  = substExpr (V x n) e r
substExpr x e expression = Lens.over Syntax.subExpressions (substExpr x e) expression

alphaNormalizeGraph :: (Eq a) => CoreGraph a -> CoreGraph a
alphaNormalizeGraph g = runST $ do
    b <- newBuilder SharingOn
    memo <- newSTRef (HashMap.empty :: HashMap.HashMap (Word32, [Text]) NodeId)
    let goVar env topX topI = goCount 0 env topI
          where
            goCount !acc (y:ys) !i
                | y == topX = if i == 0 then intern b (CVar (V "_" acc)) else goCount (acc + 1) ys (i - 1)
                | otherwise = goCount (acc + 1) ys i
            goCount _ [] i = intern b (CVar (V topX i))
    let go env nid = do
            m <- readSTRef memo
            let key = (unNodeId nid, env)
            case HashMap.lookup key m of
                Just r -> return r
                Nothing -> do
                    r <- case lookupNode g nid of
                        CVar (V x i) -> goVar env x i
                        CLam x t body -> do
                            t' <- go env t
                            body' <- go (x:env) body
                            intern b (CLam "_" t' body')
                        CPi x a c -> do
                            a' <- go env a
                            c' <- go (x:env) c
                            intern b (CPi "_" a' c')
                        CLet x mA v body -> do
                            mA' <- traverse (go env) mA
                            v' <- go env v
                            body' <- go (x:env) body
                            intern b (CLet "_" mA' v' body')
                        node -> do
                            mapped <- traverseIds (go env) node
                            intern b mapped
                    modifySTRef' memo (HashMap.insert key r)
                    return r
    root <- go [] (graphRoot g)
    freeze b root
