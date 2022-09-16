{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.Traversable () where

import Dhall.Syntax.Binding
import Dhall.Syntax.Chunks
import Dhall.Syntax.Expr
import Dhall.Syntax.Instances.Foldable ()
import Dhall.Syntax.Instances.Functor  ()
import Dhall.Syntax.Types

deriving instance Traversable (Binding s)
deriving instance Traversable (Chunks s)
deriving instance Traversable (PreferAnnotation s)
deriving instance Traversable (RecordField s)
deriving instance Traversable (FunctionBinding s)
deriving instance Traversable FieldSelection
deriving instance Traversable (Expr s)
