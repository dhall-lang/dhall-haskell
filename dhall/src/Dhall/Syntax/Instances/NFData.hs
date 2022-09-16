{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.NFData () where

import Control.DeepSeq      (NFData)
import Dhall.Syntax.Binding
import Dhall.Syntax.Chunks
import Dhall.Syntax.Const
import Dhall.Syntax.Expr
import Dhall.Syntax.Import
import Dhall.Syntax.Types
import Dhall.Syntax.Var

instance NFData Const
instance NFData Var
instance (NFData s, NFData a) => NFData (Binding s a)
instance NFData DhallDouble
instance (NFData s, NFData a) => NFData (Chunks s a)
instance NFData PreferAnnotation
instance (NFData s, NFData a) => NFData (RecordField s a)
instance (NFData s, NFData a) => NFData (FunctionBinding s a)
instance NFData s => NFData (FieldSelection s)
instance NFData WithComponent
instance (NFData s, NFData a) => NFData (Expr s a)
instance NFData Directory
instance NFData File
instance NFData FilePrefix
instance NFData Scheme
instance NFData URL
instance NFData ImportType
instance NFData ImportMode
instance NFData ImportHashed
instance NFData Import
