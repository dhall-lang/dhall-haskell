{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Opt-in common-subexpression elimination for compact Dhall text.
--
-- This is a presentation transformation: the result is semantically equivalent
-- but not a beta-normal form. It must not be used for 'Dhall.Core.normalize',
-- semantic hashing, or canonical CBOR.
module Dhall.Core.CSE
    ( CseOptions(..)
    , defaultCseOptions
    , cse
    , cseWith
    , cseGraph
    , cseGraphWith
    ) where

import Control.Monad.ST (runST)
import Data.Hashable    (Hashable)
import Data.List        (sortOn)
import Data.Maybe       (fromMaybe, mapMaybe)
import Data.STRef       (modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Text        (Text)
import Data.Void        (Void)
import Data.Word        (Word32)
import Dhall.Core.DAG
    ( CoreGraph
    , CoreNode (..)
    , NodeId (..)
    , SharingMode (..)
    , childIds
    , freeze
    , intern
    , internFrom
    , lookupNode
    , newBuilder
    , nodeCount
    , prettyGraph
    , rootId
    , subgraph
    , traverseIds
    )
import Dhall.Syntax
    ( Expr
    , Var (..)
    , denote
    , reservedIdentifiers
    )
import GHC.Generics     (Generic)
import Prettyprinter    (Pretty)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet        as HashSet
import qualified Data.Map.Strict     as Map
import qualified Data.Ord            as Ord
import qualified Data.Text           as Text
import qualified Dhall.Core.DAG      as DAG

-- | Tunables for candidate selection. Defaults require a strictly positive
-- pretty-printed saving. Tests may lower 'cseMinSaving' to force structure.
data CseOptions = CseOptions
    { cseMinCount  :: Int
    , cseMinSaving :: Int
    } deriving (Eq, Show)

defaultCseOptions :: CseOptions
defaultCseOptions = CseOptions
    { cseMinCount  = 2
    , cseMinSaving = 1
    }

-- | Factor repeated denoted subexpressions into @let@ bindings.
cse :: (Eq a, Pretty a) => Expr s a -> Expr Void a
cse = cseWith defaultCseOptions

cseWith :: (Eq a, Pretty a) => CseOptions -> Expr s a -> Expr Void a
cseWith opts = DAG.toSharedExpr . cseGraphWith opts . DAG.fromExpr . denote

cseGraph :: (Eq a, Pretty a) => CoreGraph a -> CoreGraph a
cseGraph = cseGraphWith defaultCseOptions

cseGraphWith :: (Eq a, Pretty a) => CseOptions -> CoreGraph a -> CoreGraph a
cseGraphWith opts g0 = loop (nodeCount g0 + 1) (usedNames g0) g0
  where
    loop n used g
        | n <= 0 = g
        | otherwise =
            case pickCandidate opts g used of
                Nothing ->
                    g
                Just (name, nid, cap) ->
                    loop (n - 1) (HashSet.insert name used) (extractOne g name nid cap)

-- | A free variable of a node, resolved in the ambient binder stack.
data Resolved
    = RBound Int
    | RFree Text Int
    deriving (Eq, Ord, Show, Generic)

instance Hashable Resolved

type Binds = [(Text, Int)]

type Cap = [Resolved]

type CtxKey = (Word32, Int)

usedNames :: CoreGraph a -> HashSet.HashSet Text
usedNames g =
    HashSet.union reservedIdentifiers (go 0)
  where
    n = nodeCount g
    go i
        | i >= n = HashSet.empty
        | otherwise = namesOf (lookupNode g (NodeId (fromIntegral i))) <> go (i + 1)

namesOf :: CoreNode a -> HashSet.HashSet Text
namesOf = \case
    CVar (V x _) -> HashSet.singleton x
    CLam x _ _ -> HashSet.singleton x
    CPi x _ _ -> HashSet.singleton x
    CLet x _ _ _ -> HashSet.singleton x
    _ -> HashSet.empty

freshName :: HashSet.HashSet Text -> Text
freshName used =
    head
        [ name
        | i <- [1 :: Int ..]
        , let name = "_" <> Text.pack (show i)
        , not (HashSet.member name used)
        ]

isTrivial :: CoreNode a -> Bool
isTrivial = \case
    CVar _ -> True
    CConst _ -> True
    CBool -> True
    CBoolLit _ -> True
    CBytes -> True
    CBytesLit _ -> True
    CNatural -> True
    CNaturalLit _ -> True
    CInteger -> True
    CIntegerLit _ -> True
    CDouble -> True
    CDoubleLit _ -> True
    CText -> True
    CTextLit [] _ -> True
    CDate -> True
    CDateLiteral _ -> True
    CTime -> True
    CTimeLiteral {} -> True
    CTimeZone -> True
    CTimeZoneLiteral _ -> True
    CList -> True
    COptional -> True
    CNone -> True
    CEmbed _ -> True
    _ -> False

dropBound :: Text -> [Var] -> [Var]
dropBound x = mapMaybe strip
  where
    strip (V y n)
        | x == y = if n == 0 then Nothing else Just (V y (n - 1))
        | otherwise = Just (V y n)

freeVarsOf :: CoreGraph a -> NodeId -> [Var]
freeVarsOf g = (table HashMap.!) . unNodeId
  where
    table = HashMap.fromList
        [ (fromIntegral i, fv (NodeId (fromIntegral i)))
        | i <- [0 .. nodeCount g - 1]
        ]
    fv nid =
        case lookupNode g nid of
            CVar v -> [v]
            CLam x t b -> fv t ++ dropBound x (fv b)
            CPi x a b -> fv a ++ dropBound x (fv b)
            CLet x mA v b ->
                maybe [] fv mA ++ fv v ++ dropBound x (fv b)
            node -> concatMap fv (childIds node)

resolveVar :: Binds -> Var -> Resolved
resolveVar binds (V x n) = go n binds
  where
    go k ((y, bid) : rest)
        | x == y = if k == 0 then RBound bid else go (k - 1) rest
        | otherwise = go k rest
    go k [] = RFree x k

innermostBound :: Cap -> Maybe Int
innermostBound cap =
    case [ i | RBound i <- cap ] of
        [] -> Nothing
        xs -> Just (maximum xs)

isClosedCap :: Cap -> Bool
isClosedCap = all (\case RFree _ _ -> True; RBound _ -> False)

pickCandidate
    :: (Eq a, Pretty a)
    => CseOptions
    -> CoreGraph a
    -> HashSet.HashSet Text
    -> Maybe (Text, NodeId, Cap)
pickCandidate CseOptions{..} g used =
    case ranked of
        [] -> Nothing
        (nid, cap) : _ -> Just (freshName used, nid, cap)
  where
    ranked =
        map snd
        . sortOn fst
        $
        [ (key, (nid, cap))
        | ((nid, cap), count) <- Map.toList (occurrenceCounts g)
        , count >= cseMinCount
        , not (isTrivial (lookupNode g nid))
        , nid /= rootId g
        , let size = prettySize g nid
              name = freshName used
              overhead = Text.length ("let " <> name <> " =  in ")
              saving = (count - 1) * size - overhead - count * Text.length name
        , saving >= cseMinSaving
        , let key = (Ord.Down size, Ord.Down saving, unNodeId nid, cap)
        ]

prettySize :: (Eq a, Pretty a) => CoreGraph a -> NodeId -> Int
prettySize g nid = Text.length (prettyGraph (subgraph g nid))

occurrenceCounts :: CoreGraph a -> Map.Map (NodeId, Cap) Int
occurrenceCounts g = runST $ do
    let fv = freeVarsOf g
    envTable <- newSTRef (HashMap.empty :: HashMap.HashMap Binds Int)
    nextEnv <- newSTRef (0 :: Int)
    edges <- newSTRef (HashMap.empty :: HashMap.HashMap CtxKey (HashMap.HashMap CtxKey Int))
    caps <- newSTRef (HashMap.empty :: HashMap.HashMap CtxKey (NodeId, Cap))
    seen <- newSTRef (HashSet.empty :: HashSet.HashSet CtxKey)

    let internEnv binds = do
            m <- readSTRef envTable
            case HashMap.lookup binds m of
                Just i -> return i
                Nothing -> do
                    i <- readSTRef nextEnv
                    writeSTRef nextEnv (i + 1)
                    writeSTRef envTable (HashMap.insert binds i m)
                    return i

        addEdge parent child = modifySTRef' edges $
            HashMap.insertWith (HashMap.unionWith (+)) child (HashMap.singleton parent 1)

        visit binds next nid = do
            eid <- internEnv binds
            let key = (unNodeId nid, eid)
            already <- HashSet.member key <$> readSTRef seen
            if already then
                return key
            else do
                modifySTRef' seen (HashSet.insert key)
                let cap = map (resolveVar binds) (fv nid)
                modifySTRef' caps (HashMap.insert key (nid, cap))
                case lookupNode g nid of
                    CLam x t b -> do
                        tK <- visit binds next t
                        addEdge key tK
                        bK <- visit ((x, next) : binds) (next + 1) b
                        addEdge key bK
                    CPi x a b -> do
                        aK <- visit binds next a
                        addEdge key aK
                        bK <- visit ((x, next) : binds) (next + 1) b
                        addEdge key bK
                    CLet x mA v b -> do
                        mapM_ (\t -> visit binds next t >>= addEdge key) mA
                        vK <- visit binds next v
                        addEdge key vK
                        bK <- visit ((x, next) : binds) (next + 1) b
                        addEdge key bK
                    node ->
                        mapM_ (\c -> visit binds next c >>= addEdge key)
                            (childIds node)
                return key

    rootKey <- visit [] 0 (rootId g)
    edgeMap <- readSTRef edges
    capMap <- readSTRef caps

    pcMemo <- newSTRef (HashMap.empty :: HashMap.HashMap CtxKey Int)
    let pathCount key = do
            m <- readSTRef pcMemo
            case HashMap.lookup key m of
                Just n -> return n
                Nothing -> do
                    n <- if key == rootKey then
                            return 1
                         else do
                            let parents = HashMap.toList (fromMaybe HashMap.empty (HashMap.lookup key edgeMap))
                            acc <- mapM (\(p, w) -> (w *) <$> pathCount p) parents
                            return (sum acc)
                    modifySTRef' pcMemo (HashMap.insert key n)
                    return n

    counts <- newSTRef (Map.empty :: Map.Map (NodeId, Cap) Int)
    mapM_ (\key -> do
        n <- pathCount key
        let ident = capMap HashMap.! key
        modifySTRef' counts (Map.insertWith (+) ident n)
        ) (HashMap.keys capMap)
    readSTRef counts

extractOne :: Eq a => CoreGraph a -> Text -> NodeId -> Cap -> CoreGraph a
extractOne g name target cap = runST $ do
    b <- newBuilder SharingOn
    rhs <- internFrom b g target
    let fv = freeVarsOf g
        wrapIf bid body =
            if innermostBound cap == Just bid
            then intern b (CLet name Nothing rhs body)
            else return body
        go binds next nid = do
            let cap' = map (resolveVar binds) (fv nid)
            if nid == target && cap' == cap then
                intern b (CVar (V name 0))
            else
                case lookupNode g nid of
                    CLam x t bodyN -> do
                        t' <- go binds next t
                        let bid = next
                        body' <- go ((x, bid) : binds) (next + 1) bodyN
                        body'' <- wrapIf bid body'
                        intern b (CLam x t' body'')
                    CPi x a bodyN -> do
                        a' <- go binds next a
                        let bid = next
                        body' <- go ((x, bid) : binds) (next + 1) bodyN
                        body'' <- wrapIf bid body'
                        intern b (CPi x a' body'')
                    CLet x mA v bodyN -> do
                        mA' <- traverse (go binds next) mA
                        v' <- go binds next v
                        let bid = next
                        body' <- go ((x, bid) : binds) (next + 1) bodyN
                        body'' <- wrapIf bid body'
                        intern b (CLet x mA' v' body'')
                    node -> do
                        node' <- traverseIds (go binds next) node
                        intern b node'
    root' <- go [] 0 (rootId g)
    root'' <- if isClosedCap cap
        then intern b (CLet name Nothing rhs root')
        else return root'
    freeze b root''
