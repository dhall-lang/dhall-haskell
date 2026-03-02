{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Dhall.Syntax.Instances.Lift () where

import Dhall.Syntax.Binding
import Dhall.Syntax.Chunks
import Dhall.Syntax.Const
import Dhall.Syntax.Expr
import Dhall.Syntax.FunctionBinding
import Dhall.Syntax.RecordField
import Dhall.Syntax.Types
import Dhall.Syntax.Var
import Language.Haskell.TH.Syntax   (Lift)

#if !MIN_VERSION_template_haskell(2,21,0)
import qualified Data.Fixed as Fixed
#endif
#if !MIN_VERSION_time(1,15,0)
import qualified Data.Time as Time
#endif

#if !MIN_VERSION_time(1,14,0)
deriving instance Lift Time.Day
#endif
#if !MIN_VERSION_time(1,15,0)
deriving instance Lift Time.TimeOfDay
deriving instance Lift Time.TimeZone
#endif
#if !MIN_VERSION_template_haskell(2,21,0)
deriving instance Lift (Fixed.Fixed a)
#endif
deriving instance Lift Const
deriving instance Lift Var
deriving instance (Lift s, Lift a) => Lift (Binding s a)
deriving instance Lift DhallDouble
deriving instance (Lift s, Lift a) => Lift (Chunks s a)
deriving instance Lift PreferAnnotation
deriving instance (Lift s, Lift a) => Lift (RecordField s a)
deriving instance (Lift s, Lift a) => Lift (FunctionBinding s a)
deriving instance Lift s => Lift (FieldSelection s)
deriving instance Lift WithComponent
deriving instance (Lift s, Lift a) => Lift (Expr s a)
