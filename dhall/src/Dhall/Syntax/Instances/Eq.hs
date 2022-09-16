{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.Eq () where

import Data.Bits            (xor)
import Dhall.Syntax.Binding
import Dhall.Syntax.Chunks
import Dhall.Syntax.Const
import Dhall.Syntax.Expr
import Dhall.Syntax.Import
import Dhall.Syntax.Types
import Dhall.Syntax.Var

deriving instance Eq Const
deriving instance Eq Var
deriving instance (Eq s, Eq a) => Eq (Binding s a)
deriving instance (Eq s, Eq a) => Eq (Chunks s a)
deriving instance Eq PreferAnnotation
deriving instance (Eq s, Eq a) => Eq (RecordField s a)
deriving instance (Eq s, Eq a) => Eq (FunctionBinding s a)
deriving instance Eq s => Eq (FieldSelection s)
deriving instance Eq WithComponent
-- | This instance encodes what the Dhall standard calls an \"exact match\"
-- between two expressions.
--
-- Note that
--
-- >>> nan = DhallDouble (0/0)
-- >>> DoubleLit nan == DoubleLit nan
-- True
deriving instance (Eq s, Eq a) => Eq (Expr s a)
deriving instance Eq Directory
deriving instance Eq File
deriving instance Eq FilePrefix
deriving instance Eq Scheme
deriving instance Eq URL
deriving instance Eq ImportType
deriving instance Eq ImportMode
deriving instance Eq ImportHashed
deriving instance Eq Import

-- | This instance satisfies all the customary 'Eq' laws except substitutivity.
--
-- In particular:
--
-- >>> nan = DhallDouble (0/0)
-- >>> nan == nan
-- True
--
-- This instance is also consistent with with the binary encoding of Dhall @Double@s:
--
-- >>> toBytes n = Dhall.Binary.encodeExpression (DoubleLit n :: Expr Void Import)
--
-- prop> \a b -> (a == b) == (toBytes a == toBytes b)
instance Eq DhallDouble where
    DhallDouble a == DhallDouble b
        | isNaN a && isNaN b                      = True
        | isNegativeZero a `xor` isNegativeZero b = False
        | otherwise                               = a == b
