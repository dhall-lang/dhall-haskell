{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.Ord () where

import Dhall.Syntax.Binding
import Dhall.Syntax.Chunks
import Dhall.Syntax.Const
import Dhall.Syntax.Expr
import Dhall.Syntax.FunctionBinding
import Dhall.Syntax.Import
import Dhall.Syntax.Instances.Eq    ()
import Dhall.Syntax.RecordField
import Dhall.Syntax.Types
import Dhall.Syntax.Var

deriving instance Ord Const
deriving instance Ord Var
deriving instance (Ord s, Ord a) => Ord (Binding s a)
deriving instance (Ord s, Ord a) => Ord (Chunks s a)
deriving instance Ord PreferAnnotation
deriving instance (Ord s, Ord a) => Ord (RecordField s a)
deriving instance (Ord s, Ord a) => Ord (FunctionBinding s a)
deriving instance Ord s => Ord (FieldSelection s)
deriving instance Ord WithComponent
-- | Note that this 'Ord' instance inherits `DhallDouble`'s defects.
deriving instance (Ord s, Ord a) => Ord (Expr s a)
deriving instance Ord Directory
deriving instance Ord File
deriving instance Ord FilePrefix
deriving instance Ord Scheme
deriving instance Ord URL
deriving instance Ord ImportType
deriving instance Ord ImportMode
deriving instance Ord ImportHashed
deriving instance Ord Import

-- | This instance relies on the 'Eq' instance for 'DhallDouble' but cannot
-- satisfy the customary 'Ord' laws when @NaN@ is involved.
instance Ord DhallDouble where
    compare a@(DhallDouble a') b@(DhallDouble b') =
        if a == b
            then EQ
            else compare a' b'
