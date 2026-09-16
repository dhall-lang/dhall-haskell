{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Annotation-free hash-cons payload: exact denoted syntax with child 'NodeId's.
module Dhall.Core.DAG.Node
    ( NodeId(..)
    , CoreNode(..)
    , childIds
    , edgeCount
    ) where

import Data.ByteString              (ByteString)
import Data.Hashable                (Hashable (..))
import Data.List.NonEmpty           (NonEmpty)
import Data.Text                    (Text)
import Data.Word                    (Word32, Word8)
import Dhall.Syntax.Const           (Const)
import Dhall.Syntax.Instances.Eq    ()
import Dhall.Syntax.Instances.Show  ()
import Dhall.Syntax.Types           (DhallDouble (..), PreferAnnotation, WithComponent)
import Dhall.Syntax.Var             (Var)
import GHC.Generics                 (Generic)
import Numeric.Natural              (Natural)

import qualified Data.Time as Time

-- | Dense index into an owning 'Dhall.Core.DAG.CoreGraph'. Not portable across graphs.
newtype NodeId = NodeId { unNodeId :: Word32 }
    deriving stock (Eq, Ord, Show)
    deriving newtype (Hashable)

-- | One interned node. Recursive children are ids, never nested payloads.
data CoreNode a
    = CConst !Const
    | CVar !Var
    | CLam !Text !NodeId !NodeId
      -- ^ binder, annotation, body
    | CPi !Text !NodeId !NodeId
      -- ^ binder, domain, codomain
    | CApp !NodeId !NodeId
    | CLet !Text !(Maybe NodeId) !NodeId !NodeId
      -- ^ binder, optional type, value, body
    | CAnnot !NodeId !NodeId
    | CBool
    | CBoolLit !Bool
    | CBoolAnd !NodeId !NodeId
    | CBoolOr !NodeId !NodeId
    | CBoolEQ !NodeId !NodeId
    | CBoolNE !NodeId !NodeId
    | CBoolIf !NodeId !NodeId !NodeId
    | CBytes
    | CBytesLit !ByteString
    | CNatural
    | CNaturalLit !Natural
    | CNaturalPlus !NodeId !NodeId
    | CNaturalTimes !NodeId !NodeId
    | CInteger
    | CIntegerLit !Integer
    | CDouble
    | CDoubleLit !DhallDouble
    | CText
    | CTextLit ![(Text, NodeId)] !Text
    | CTextAppend !NodeId !NodeId
    | CDate
    | CDateLiteral !Time.Day
    | CTime
    | CTimeLiteral !Word8 !Word8 !Word8 !Integer !Int
    | CTimeZone
    | CTimeZoneLiteral !Time.TimeZone
    | CList
    | CListLit !(Maybe NodeId) ![NodeId]
    | CListAppend !NodeId !NodeId
    | COptional
    | CSome !NodeId
    | CNone
    | CRecord ![(Text, NodeId)]
    | CRecordLit ![(Text, NodeId)]
    | CUnion ![(Text, Maybe NodeId)]
    | CCombine !(Maybe Text) !NodeId !NodeId
    | CCombineTypes !NodeId !NodeId
    | CPrefer !PreferAnnotation !NodeId !NodeId
    | CRecordCompletion !NodeId !NodeId
    | CMerge !NodeId !NodeId !(Maybe NodeId)
    | CToMap !NodeId !(Maybe NodeId)
    | CShowConstructor !NodeId
    | CField !NodeId !Text
    | CProject !NodeId !(Either [Text] NodeId)
    | CAssert !NodeId
    | CEquivalent !NodeId !NodeId
    | CWith !NodeId !(NonEmpty WithComponent) !NodeId
    | CImportAlt !NodeId !NodeId
    | CEmbed a
    deriving stock (Eq, Generic, Show)

instance Hashable Const
instance Hashable Var
instance Hashable PreferAnnotation
instance Hashable WithComponent

instance Hashable DhallDouble where
    hashWithSalt salt (DhallDouble d)
        | isNaN d =
            salt `hashWithSalt` (0 :: Int) `hashWithSalt` True
        | otherwise =
            salt `hashWithSalt` decodeFloat d `hashWithSalt` isNegativeZero d

instance Hashable Time.Day where
    hashWithSalt salt day =
        hashWithSalt salt (Time.toModifiedJulianDay day)

instance Hashable Time.TimeZone where
    hashWithSalt salt (Time.TimeZone minutes summerOnly name) =
        salt `hashWithSalt` minutes `hashWithSalt` summerOnly `hashWithSalt` name

-- Hash the denoted shape only. 'CEmbed' payloads are distinguished by 'Eq'.
instance Eq a => Hashable (CoreNode a) where
    hashWithSalt s = \case
        CConst c -> s `hashWithSalt` (0 :: Int) `hashWithSalt` c
        CVar v -> s `hashWithSalt` (1 :: Int) `hashWithSalt` v
        CLam x t b -> s `hashWithSalt` (2 :: Int) `hashWithSalt` x `hashWithSalt` t `hashWithSalt` b
        CPi x a b -> s `hashWithSalt` (3 :: Int) `hashWithSalt` x `hashWithSalt` a `hashWithSalt` b
        CApp f x -> s `hashWithSalt` (4 :: Int) `hashWithSalt` f `hashWithSalt` x
        CLet x mA v b -> s `hashWithSalt` (5 :: Int) `hashWithSalt` x `hashWithSalt` mA `hashWithSalt` v `hashWithSalt` b
        CAnnot a b -> s `hashWithSalt` (6 :: Int) `hashWithSalt` a `hashWithSalt` b
        CBool -> s `hashWithSalt` (7 :: Int)
        CBoolLit x -> s `hashWithSalt` (8 :: Int) `hashWithSalt` x
        CBoolAnd a b -> s `hashWithSalt` (9 :: Int) `hashWithSalt` a `hashWithSalt` b
        CBoolOr a b -> s `hashWithSalt` (10 :: Int) `hashWithSalt` a `hashWithSalt` b
        CBoolEQ a b -> s `hashWithSalt` (11 :: Int) `hashWithSalt` a `hashWithSalt` b
        CBoolNE a b -> s `hashWithSalt` (12 :: Int) `hashWithSalt` a `hashWithSalt` b
        CBoolIf a b c -> s `hashWithSalt` (13 :: Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c
        CBytes -> s `hashWithSalt` (14 :: Int)
        CBytesLit x -> s `hashWithSalt` (15 :: Int) `hashWithSalt` x
        CNatural -> s `hashWithSalt` (16 :: Int)
        CNaturalLit n -> s `hashWithSalt` (17 :: Int) `hashWithSalt` n
        CNaturalPlus a b -> s `hashWithSalt` (18 :: Int) `hashWithSalt` a `hashWithSalt` b
        CNaturalTimes a b -> s `hashWithSalt` (19 :: Int) `hashWithSalt` a `hashWithSalt` b
        CInteger -> s `hashWithSalt` (20 :: Int)
        CIntegerLit n -> s `hashWithSalt` (21 :: Int) `hashWithSalt` n
        CDouble -> s `hashWithSalt` (22 :: Int)
        CDoubleLit n -> s `hashWithSalt` (23 :: Int) `hashWithSalt` n
        CText -> s `hashWithSalt` (24 :: Int)
        CTextLit cs z -> s `hashWithSalt` (25 :: Int) `hashWithSalt` cs `hashWithSalt` z
        CTextAppend a b -> s `hashWithSalt` (26 :: Int) `hashWithSalt` a `hashWithSalt` b
        CDate -> s `hashWithSalt` (27 :: Int)
        CDateLiteral d -> s `hashWithSalt` (28 :: Int) `hashWithSalt` d
        CTime -> s `hashWithSalt` (29 :: Int)
        CTimeLiteral a b c d e -> s `hashWithSalt` (30 :: Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e
        CTimeZone -> s `hashWithSalt` (31 :: Int)
        CTimeZoneLiteral z -> s `hashWithSalt` (32 :: Int) `hashWithSalt` z
        CList -> s `hashWithSalt` (33 :: Int)
        CListLit m xs -> s `hashWithSalt` (34 :: Int) `hashWithSalt` m `hashWithSalt` xs
        CListAppend a b -> s `hashWithSalt` (35 :: Int) `hashWithSalt` a `hashWithSalt` b
        COptional -> s `hashWithSalt` (36 :: Int)
        CSome a -> s `hashWithSalt` (37 :: Int) `hashWithSalt` a
        CNone -> s `hashWithSalt` (38 :: Int)
        CRecord kvs -> s `hashWithSalt` (39 :: Int) `hashWithSalt` kvs
        CRecordLit kvs -> s `hashWithSalt` (40 :: Int) `hashWithSalt` kvs
        CUnion kts -> s `hashWithSalt` (41 :: Int) `hashWithSalt` kts
        CCombine m a b -> s `hashWithSalt` (42 :: Int) `hashWithSalt` m `hashWithSalt` a `hashWithSalt` b
        CCombineTypes a b -> s `hashWithSalt` (43 :: Int) `hashWithSalt` a `hashWithSalt` b
        CPrefer p a b -> s `hashWithSalt` (44 :: Int) `hashWithSalt` p `hashWithSalt` a `hashWithSalt` b
        CRecordCompletion a b -> s `hashWithSalt` (45 :: Int) `hashWithSalt` a `hashWithSalt` b
        CMerge a b m -> s `hashWithSalt` (46 :: Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` m
        CToMap a m -> s `hashWithSalt` (47 :: Int) `hashWithSalt` a `hashWithSalt` m
        CShowConstructor a -> s `hashWithSalt` (48 :: Int) `hashWithSalt` a
        CField a l -> s `hashWithSalt` (49 :: Int) `hashWithSalt` a `hashWithSalt` l
        CProject a e -> s `hashWithSalt` (50 :: Int) `hashWithSalt` a `hashWithSalt` e
        CAssert a -> s `hashWithSalt` (51 :: Int) `hashWithSalt` a
        CEquivalent a b -> s `hashWithSalt` (52 :: Int) `hashWithSalt` a `hashWithSalt` b
        CWith a p b -> s `hashWithSalt` (53 :: Int) `hashWithSalt` a `hashWithSalt` p `hashWithSalt` b
        CImportAlt a b -> s `hashWithSalt` (54 :: Int) `hashWithSalt` a `hashWithSalt` b
        CEmbed _ -> s `hashWithSalt` (55 :: Int)

-- | Immediate child ids, left-to-right, matching constructor field order.
childIds :: CoreNode a -> [NodeId]
childIds = \case
    CConst _ -> []
    CVar _ -> []
    CLam _ t b -> [t, b]
    CPi _ a b -> [a, b]
    CApp f x -> [f, x]
    CLet _ mA v b -> maybe id (:) mA [v, b]
    CAnnot a b -> [a, b]
    CBool -> []
    CBoolLit _ -> []
    CBoolAnd a b -> [a, b]
    CBoolOr a b -> [a, b]
    CBoolEQ a b -> [a, b]
    CBoolNE a b -> [a, b]
    CBoolIf a b c -> [a, b, c]
    CBytes -> []
    CBytesLit _ -> []
    CNatural -> []
    CNaturalLit _ -> []
    CNaturalPlus a b -> [a, b]
    CNaturalTimes a b -> [a, b]
    CInteger -> []
    CIntegerLit _ -> []
    CDouble -> []
    CDoubleLit _ -> []
    CText -> []
    CTextLit chunks _ -> map snd chunks
    CTextAppend a b -> [a, b]
    CDate -> []
    CDateLiteral _ -> []
    CTime -> []
    CTimeLiteral {} -> []
    CTimeZone -> []
    CTimeZoneLiteral _ -> []
    CList -> []
    CListLit m xs -> maybe id (:) m xs
    CListAppend a b -> [a, b]
    COptional -> []
    CSome a -> [a]
    CNone -> []
    CRecord kvs -> map snd kvs
    CRecordLit kvs -> map snd kvs
    CUnion kts -> [ n | (_, Just n) <- kts ]
    CCombine _ a b -> [a, b]
    CCombineTypes a b -> [a, b]
    CPrefer _ a b -> [a, b]
    CRecordCompletion a b -> [a, b]
    CMerge a b m -> maybe id (flip (++) . pure) m [a, b]
    CToMap a m -> maybe id (flip (++) . pure) m [a]
    CShowConstructor a -> [a]
    CField a _ -> [a]
    CProject a e -> a : either (const []) pure e
    CAssert a -> [a]
    CEquivalent a b -> [a, b]
    CWith a _ b -> [a, b]
    CImportAlt a b -> [a, b]
    CEmbed _ -> []

-- | Number of child edges on this node.
edgeCount :: CoreNode a -> Int
edgeCount = length . childIds
