{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.Show () where

import Dhall.Syntax.Binding
import Dhall.Syntax.Chunks
import Dhall.Syntax.Const
import Dhall.Syntax.Expr
import Dhall.Syntax.Import
import Dhall.Syntax.Types
import Dhall.Syntax.Var

deriving instance Show Const
deriving instance Show Var
deriving instance (Show s, Show a) => Show (Binding s a)
deriving instance Show DhallDouble
deriving instance (Show s, Show a) => Show (Chunks s a)
deriving instance Show PreferAnnotation
deriving instance (Show s, Show a) => Show (RecordField s a)
deriving instance (Show s, Show a) => Show (FunctionBinding s a)
deriving instance Show s => Show (FieldSelection s)
deriving instance Show WithComponent
deriving instance (Show s, Show a) => Show (Expr s a)
deriving instance Show Directory
deriving instance Show File
deriving instance Show FilePrefix
deriving instance Show Scheme
deriving instance Show URL
deriving instance Show ImportType
deriving instance Show ImportMode
deriving instance Show ImportHashed
deriving instance Show Import
