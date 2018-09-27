{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Unboxed vector as megaparsec's Stream.
module Dhall.Parser.Vector (
    UVectorChar,
    uvectorToText,
    uvectorFromText,
    ) where

import Data.Proxy (Proxy (..))
import Text.Megaparsec (SourcePos (..), PosState (..), Token, Tokens, pos1, mkPos, unPos, takeWhile_)

import qualified Text.Megaparsec
import qualified Data.Text
import qualified Data.Vector.Unboxed as U

type UVectorChar =  U.Vector Char

uvectorToText :: UVectorChar -> Data.Text.Text
uvectorToText = Data.Text.pack . U.toList

uvectorFromText :: Data.Text.Text -> UVectorChar
uvectorFromText = U.fromList . Data.Text.unpack

instance c ~ Char => Text.Megaparsec.Stream (U.Vector c) where
    type Token (U.Vector c) = c
    type Tokens (U.Vector c) = U.Vector c

    tokenToChunk _  = U.singleton
    tokensToChunk _ = U.fromList
    chunkToTokens _ = U.toList
    chunkLength _   = U.length
    chunkEmpty _    = U.null

    take1_ xs
        | U.null xs = Nothing
        | otherwise = Just (U.unsafeHead xs, U.unsafeTail xs)

    takeN_ n xs
        | n <= 0           = Just (U.empty, xs)
        | U.null xs        = Nothing
        | U.length xs <= n = Just (xs, U.empty)
        | otherwise        = Just (U.splitAt n xs)

    takeWhile_ = U.span
    showTokens _ = Text.Megaparsec.showTokens (Proxy :: Proxy Data.Text.Text)

    reachOffset o pst =
        reachOffset' U.splitAt U.foldl' U.toList id ('\n', '\t') o pst
    reachOffsetNoLine o pst =
        reachOffsetNoLine' U.splitAt U.foldl' ('\n', '\t') o pst

----------------------------------------------------------------------------
-- Helpers from megaparsec

-- | An internal helper state type combining a difference 'String' and an
-- unboxed 'SourcePos'.

data St = St SourcePos ShowS

-- {-# UNPACK #-} -- TODO do we need to unpack or not?

-- | A helper definition to facilitate defining 'reachOffset' for various
-- stream types.

reachOffset'
  :: forall s. Text.Megaparsec.Stream s
  => (Int -> s -> (Tokens s, s))
     -- ^ How to split input stream at given offset
  -> (forall b. (b -> Token s -> b) -> b -> Tokens s -> b)
     -- ^ How to fold over input stream
  -> (Tokens s -> String)
     -- ^ How to convert chunk of input stream into a 'String'
  -> (Token s -> Char)
     -- ^ How to convert a token into a 'Char'
  -> (Token s, Token s)
     -- ^ Newline token and tab token
  -> Int
     -- ^ Offset to reach
  -> PosState s
     -- ^ Initial 'PosState' to use
  -> (SourcePos, String, PosState s)
     -- ^ Reached 'SourcePos', line at which 'SourcePos' is located, updated
     -- 'PosState'
reachOffset' splitAt'
             foldl''
             fromToks
             fromTok
             (newlineTok, tabTok)
             o
             PosState {..} =
  ( spos
  , case expandTab pstateTabWidth
           . addPrefix
           . f
           . fromToks
           . fst
           $ takeWhile_ (/= newlineTok) post of
      "" -> "<empty line>"
      xs -> xs
  , PosState
      { pstateInput = post
      , pstateOffset = max pstateOffset o
      , pstateSourcePos = spos
      , pstateTabWidth = pstateTabWidth
      , pstateLinePrefix =
          if sameLine
            -- NOTE We don't use difference lists here because it's
            -- desirable for 'PosState' to be an instance of 'Eq' and
            -- 'Show'. So we just do appending here. Fortunately several
            -- parse errors on the same line should be relatively rare.
            then pstateLinePrefix ++ f ""
            else f ""
      }
  )
  where
    addPrefix xs =
      if sameLine
        then pstateLinePrefix ++ xs
        else xs
    sameLine = sourceLine spos == sourceLine pstateSourcePos
    (pre, post) = splitAt' (o - pstateOffset) pstateInput
    St spos f = foldl'' go (St pstateSourcePos id) pre
    go (St apos g) ch =
      let SourcePos n l c = apos
          c' = unPos c
          w  = unPos pstateTabWidth
      in if | ch == newlineTok ->
                St (SourcePos n (l <> pos1) pos1)
                   id
            | ch == tabTok ->
                St (SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w)))
                   (g . (fromTok ch :))
            | otherwise ->
                St (SourcePos n l (c <> pos1))
                   (g . (fromTok ch :))
{-# INLINE reachOffset' #-}

-- | Like 'reachOffset'' but for 'reachOffsetNoLine'.

reachOffsetNoLine'
  :: forall s. Text.Megaparsec.Stream s
  => (Int -> s -> (Tokens s, s))
     -- ^ How to split input stream at given offset
  -> (forall b. (b -> Token s -> b) -> b -> Tokens s -> b)
     -- ^ How to fold over input stream
  -> (Token s, Token s)
     -- ^ Newline token and tab token
  -> Int
     -- ^ Offset to reach
  -> PosState s
     -- ^ Initial 'PosState' to use
  -> (SourcePos, PosState s)
     -- ^ Reached 'SourcePos' and updated 'PosState'
reachOffsetNoLine' splitAt'
                   foldl''
                   (newlineTok, tabTok)
                   o
                   PosState {..} =
  ( spos
  , PosState
      { pstateInput = post
      , pstateOffset = max pstateOffset o
      , pstateSourcePos = spos
      , pstateTabWidth = pstateTabWidth
      , pstateLinePrefix = pstateLinePrefix
      }
  )
  where
    spos = foldl'' go pstateSourcePos pre
    (pre, post) = splitAt' (o - pstateOffset) pstateInput
    go (SourcePos n l c) ch =
      let c' = unPos c
          w  = unPos pstateTabWidth
      in if | ch == newlineTok ->
                SourcePos n (l <> pos1) pos1
            | ch == tabTok ->
                SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w))
            | otherwise ->
                SourcePos n l (c <> pos1)
{-# INLINE reachOffsetNoLine' #-}

-- | Replace tab characters with given number of spaces.

expandTab
  :: Text.Megaparsec.Pos
  -> String
  -> String
expandTab w' = go 0
  where
    go 0 []        = []
    go 0 ('\t':xs) = go w xs
    go 0 (x:xs)    = x : go 0 xs
    go n xs        = ' ' : go (n - 1) xs
    w              = unPos w'
