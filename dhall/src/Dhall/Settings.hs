{-| This module proviedes the different settings used to evaluate a Dhall
    expression.
-}

module Dhall.Settings
    ( -- * Input settings
      InputSettings
    , defaultInputSettings
    , rootDirectory
    , sourceName
    , HasInputSettings (..)

      -- * Evaluation settings
    , EvaluateSettings
    , defaultEvaluateSettings
    , newManager
    , normalizer
    , startingContext
    , substitutions
    , HasEvaluateSettings (..)
    ) where

import Data.Void              (Void)
import Dhall.Src              (Src)
import Dhall.Syntax           (Expr)
import Lens.Micro             (Lens', lens)
import qualified Dhall.Context
import qualified Dhall.Core
import qualified Dhall.Import.Manager
import qualified Dhall.Substitution

--------------------------------------------------------------------------------
-- Input settings
--------------------------------------------------------------------------------

-- | @since 1.16
data InputSettings = InputSettings
  { _rootDirectory :: FilePath
  , _sourceName :: FilePath
  , _evaluateSettings :: EvaluateSettings
  }

-- | Default input settings: Resolves imports relative to @.@ (the
-- current working directory), report errors as coming from @(input)@,
-- and default evaluation settings from 'defaultEvaluateSettings'.
--
-- @since 1.16
defaultInputSettings :: InputSettings
defaultInputSettings = InputSettings
  { _rootDirectory = "."
  , _sourceName = "(input)"
  , _evaluateSettings = defaultEvaluateSettings
  }


-- | Access the directory to resolve imports relative to.
--
-- @since 1.16
--
-- @since 1.43: Work on all types that have an instance of 'HasInputSettings'
--              instead of 'InputSettings'.
rootDirectory
    :: (HasInputSettings s)
    => Lens' s FilePath
rootDirectory =
    inputSettings
        . lens _rootDirectory (\s x -> s { _rootDirectory = x })

-- | Access the name of the source to report locations from; this is
-- only used in error messages, so it's okay if this is a best guess
-- or something symbolic.
--
-- @since 1.16
--
-- @since 1.43: Work on all types that have an instance of 'HasInputSettings'
--              instead of 'InputSettings'.
sourceName
    :: (HasInputSettings s)
    => Lens' s FilePath
sourceName =
    inputSettings
        . lens _sourceName (\s x -> s { _sourceName = x})

-- | @since 1.43
class HasInputSettings s where
    inputSettings :: Lens' s InputSettings

instance HasInputSettings InputSettings where
    inputSettings = id



--------------------------------------------------------------------------------
-- Evaluation settings
--------------------------------------------------------------------------------

-- | @since 1.16
data EvaluateSettings = EvaluateSettings
  { _newManager      :: IO Dhall.Import.Manager.Manager
  , _normalizer      :: Maybe (Dhall.Core.ReifiedNormalizer Void)
  , _startingContext :: Dhall.Context.Context (Expr Src Void)
  , _substitutions   :: Dhall.Substitution.Substitutions Src Void
  }

-- | Default evaluation settings: No extra entries in the initial
-- context, and no special normalizer behaviour.
--
-- @since 1.16
defaultEvaluateSettings :: EvaluateSettings
defaultEvaluateSettings = EvaluateSettings
  { _newManager      = Dhall.Import.Manager.defaultNewManager
  , _normalizer      = Nothing
  , _startingContext = Dhall.Context.empty
  , _substitutions   = Dhall.Substitution.empty
  }

-- | Access the starting context used for evaluation and type-checking.
--
-- @since 1.16
startingContext
  :: (HasEvaluateSettings s)
  => Lens' s (Dhall.Context.Context (Expr Src Void))
startingContext =
    evaluateSettings
        . lens _startingContext (\s x -> s { _startingContext = x})

-- | Access the custom substitutions.
--
-- @since 1.30
substitutions
  :: (HasEvaluateSettings s)
  => Lens' s (Dhall.Substitution.Substitutions Src Void)
substitutions =
    evaluateSettings
        . lens _substitutions (\s x -> s { _substitutions = x })

-- | Access the custom normalizer.
--
-- @since 1.16
normalizer
  :: (HasEvaluateSettings s)
  => Lens' s (Maybe (Dhall.Core.ReifiedNormalizer Void))
normalizer =
    evaluateSettings
        . lens _normalizer (\s x -> s { _normalizer = x })

-- | Access the HTTP manager initializer.
--
-- @since 1.36
newManager
  :: (HasEvaluateSettings s)
  => Lens' s (IO Dhall.Import.Manager.Manager)
newManager =
    evaluateSettings
        . lens _newManager (\s x -> s { _newManager = x })

-- | @since 1.16
class HasEvaluateSettings s where
  evaluateSettings :: Lens' s EvaluateSettings

instance HasEvaluateSettings InputSettings where
  evaluateSettings =
    lens _evaluateSettings (\s x -> s { _evaluateSettings = x })

instance HasEvaluateSettings EvaluateSettings where
  evaluateSettings = id
