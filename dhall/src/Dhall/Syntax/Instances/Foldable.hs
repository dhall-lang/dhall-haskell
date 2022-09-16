{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.Foldable () where

import Dhall.Syntax.Binding
import Dhall.Syntax.Chunks
import Dhall.Syntax.Expr
import Dhall.Syntax.RecordField
import Dhall.Syntax.Types

deriving instance Foldable (Binding s)
deriving instance Foldable (Chunks s)
deriving instance Foldable (RecordField s)
deriving instance Foldable (FunctionBinding s)
deriving instance Foldable FieldSelection
deriving instance Foldable (Expr s)
