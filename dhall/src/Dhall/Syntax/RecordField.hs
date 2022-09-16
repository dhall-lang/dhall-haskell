{-# LANGUAGE DeriveGeneric #-}

module Dhall.Syntax.RecordField
    ( RecordField(..)
    , makeRecordField

      -- * Optics
    , recordFieldExprs
    ) where

import {-# SOURCE #-} Dhall.Syntax.Expr (Expr)
import                GHC.Generics      (Generic)

-- | Record the field of a record-type and record-literal expression.
-- The reason why we use the same ADT for both of them is because they store
-- the same information.
--
-- For example,
--
-- > { {- A -} x {- B -} : {- C -} T }
--
-- ... or
--
-- > { {- A -} x {- B -} = {- C -} T }
--
-- will be instantiated as follows:
--
-- * @recordFieldSrc0@ corresponds to the @A@ comment.
-- * @recordFieldValue@ is @"T"@
-- * @recordFieldSrc1@ corresponds to the @B@ comment.
-- * @recordFieldSrc2@ corresponds to the @C@ comment.
--
-- Although the @A@ comment isn't annotating the @"T"@ Record Field,
-- this is the best place to keep these comments.
--
-- Note that @recordFieldSrc2@ is always 'Nothing' when the 'RecordField' is for
-- a punned entry, because there is no @=@ sign. For example,
--
-- > { {- A -} x {- B -} }
--
-- will be instantiated as follows:
--
-- * @recordFieldSrc0@ corresponds to the @A@ comment.
-- * @recordFieldValue@ corresponds to @(Var "x")@
-- * @recordFieldSrc1@ corresponds to the @B@ comment.
-- * @recordFieldSrc2@ will be 'Nothing'
--
-- The labels involved in a record using dot-syntax like in this example:
--
-- > { {- A -} a {- B -} . {- C -} b {- D -} . {- E -} c {- F -} = {- G -} e }
--
-- will be instantiated as follows:
--
-- * For both the @a@ and @b@ field, @recordfieldSrc2@ is 'Nothing'
-- * For the @a@ field:
--   * @recordFieldSrc0@ corresponds to the @A@ comment
--   * @recordFieldSrc1@ corresponds to the @B@ comment
-- * For the @b@ field:
--   * @recordFieldSrc0@ corresponds to the @C@ comment
--   * @recordFieldSrc1@ corresponds to the @D@ comment
-- * For the @c@ field:
--   * @recordFieldSrc0@ corresponds to the @E@ comment
--   * @recordFieldSrc1@ corresponds to the @F@ comment
--   * @recordFieldSrc2@ corresponds to the @G@ comment
--
-- That is, for every label except the last one the semantics of
-- @recordFieldSrc0@ and @recordFieldSrc1@ are the same from a regular record
-- label but @recordFieldSrc2@ is always 'Nothing'. For the last keyword, all
-- srcs are 'Just'
data RecordField s a = RecordField
    { recordFieldSrc0  :: Maybe s
    , recordFieldValue :: Expr s a
    , recordFieldSrc1  :: Maybe s
    , recordFieldSrc2  :: Maybe s
    } deriving Generic

-- | Construct a 'RecordField' with no src information
makeRecordField :: Expr s a -> RecordField s a
makeRecordField e = RecordField Nothing e Nothing Nothing

{-| Traverse over the immediate 'Expr' children in a 'RecordField'.
-}
recordFieldExprs
    :: Applicative f
    => (Expr s a -> f (Expr s b))
    -> RecordField s a -> f (RecordField s b)
recordFieldExprs f (RecordField s0 e s1 s2) =
    RecordField
        <$> pure s0
        <*> f e
        <*> pure s1
        <*> pure s2
{-# INLINABLE recordFieldExprs #-}
