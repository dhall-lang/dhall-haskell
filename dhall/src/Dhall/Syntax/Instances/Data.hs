{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.Data () where

import Data.Data          (Data)
import Dhall.Syntax.Binding
import Dhall.Syntax.Const
import Dhall.Syntax.Expr
import Dhall.Syntax.Types
import Dhall.Syntax.Var

deriving instance Data Const
deriving instance Data Var
deriving instance (Data a, Data s) => Data (Binding s a)
deriving instance Data DhallDouble
deriving instance (Data a, Data s) => Data (Chunks s a)
deriving instance (Data a, Data s) => Data (PreferAnnotation s a)
deriving instance (Data a, Data s) => Data (RecordField s a)
deriving instance (Data a, Data s) => Data (FunctionBinding s a)
deriving instance Data s => Data (FieldSelection s)
deriving instance Data WithComponent
deriving instance (Data a, Data s) => Data (Expr s a)
