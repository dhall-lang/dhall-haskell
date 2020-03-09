{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Dhall.Deriving
  ( -- * DerivingVia newtype
    Codec (..)

    -- * type-level functions on 'InterpretOptions'
  , ModifyOptions (..)

    -- * Identity for 'ModifyOptions' and for 'TextFunction'
  , AsIs

    -- * Composition for 'ModifyOptions' and for 'TextFunction'
  , type (<<<)

    -- * Primitives for 'ModifyOptions'
  , Field
  , Constructor
  , SetSingletonConstructors

    -- * type-level functions on Text
  , TextFunction (..)

    -- * Primitives for 'TextFunction'
  , DropPrefix
  , TitleCase
  , CamelCase
  , PascalCase
  , SnakeCase
  , SpinalCase
  , TrainCase

    -- * Type-level versions of 'SingletonConstructors'
  , Bare
  , Wrapped
  , Smart

    -- * InterpretOptions setters
  , addFieldModifier
  , addConstructorModifier
  , setSingletonConstructors
  ) where

import Data.Proxy (Proxy (..))
import Dhall
import GHC.Generics (Generic(Rep))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

import qualified Data.Text as Text
import qualified Data.Text.Manipulate as Case

-- | Intended for use on @deriving via@ clauses for types with a
--   'Generic' instance. The @tag@ argument is used to construct an
--   'InterpretOptions' value which is used as the first argument
--   to 'genericAutoWith'.
newtype Codec tag a = Codec { unCodec :: a }

instance (Generic a, GenericFromDhall (Rep a), ModifyOptions tag) => FromDhall (Codec tag a) where
  autoWith _ = Codec <$> genericAutoWith (modifyOptions @tag defaultInterpretOptions)

instance (Generic a, GenericToDhall (Rep a), ModifyOptions tag) => ToDhall (Codec tag a) where
  injectWith _ = unCodec >$< genericToDhallWith (modifyOptions @tag defaultInterpretOptions)

-- | Convert a type into a @InterpretOptions -> InterpretOptions@ function
class ModifyOptions a where
  modifyOptions :: InterpretOptions -> InterpretOptions


-- | The identity for functions on @InterpretOptions@. Useful for deriving
--   @FromDhall@ and @ToDhall@ with the default options.
type AsIs = ()

instance ModifyOptions AsIs where
  modifyOptions = id

instance TextFunction AsIs where
  textFunction = id


-- | Composition for functions on @InterpretOptions@ and on @Text@.
--   We use @<<<@ since @.@ isn't a valid type operator yet
--   (it will be valid starting from ghc-8.8.1)
data a <<< b
infixr 1 <<<

instance (ModifyOptions a, ModifyOptions b) => ModifyOptions (a <<< b) where
  modifyOptions = modifyOptions @a . modifyOptions @b

instance (TextFunction a, TextFunction b) => TextFunction (a <<< b) where
  textFunction = textFunction @a . textFunction @b


-- | @Field t@ Post-composes the @fieldModifier@ from @options@ with the
--   value-level version of @t@, obtained with @TextFunction@
data Field a
instance TextFunction a => ModifyOptions (Field a) where
  modifyOptions = addFieldModifier (textFunction @a)

-- | @Constructor t@ Post-composes the @constructorModifier@ from @options@
--   with the value-level version of @t@, obtained with @TextFunction@
data Constructor a
instance TextFunction a => ModifyOptions (Constructor a) where
  modifyOptions = addConstructorModifier (textFunction @a)


-- * Text Functions

-- | Convert a type into a @Text -> Text@ function
class TextFunction a where
  textFunction :: Text -> Text

-- | @DropPrefix prefix@ corresponds to the value level
--   function @'dropPrefix' prefix@
data DropPrefix (s :: Symbol)
instance KnownSymbol s => TextFunction (DropPrefix s) where
  textFunction = dropPrefix (Text.pack (symbolVal @s Proxy))

-- | Convert casing to @Title Cased Phrase@
data TitleCase
instance TextFunction TitleCase where
  textFunction = Case.toTitle

-- | Convert casing to @camelCasedPhrase@
data CamelCase
instance TextFunction CamelCase where
  textFunction = Case.toCamel

-- | Convert casing to @PascalCasedPhrase@
data PascalCase
instance TextFunction PascalCase where
  textFunction = Case.toPascal

-- | Convert casing to @snake_cased_phrase@
data SnakeCase
instance TextFunction SnakeCase where
  textFunction = Case.toSnake

-- | Convert casing to @spinal-cased-phrase@
data SpinalCase
instance TextFunction SpinalCase where
  textFunction = Case.toSpinal

-- | Convert casing to @Train-Cased-Phrase@
data TrainCase
instance TextFunction TrainCase where
  textFunction = Case.toTrain


-- | @SetSingletonConstructors t@ Replaces the @singletonConstructors@
--  from @options@ with the value-level version of @t@.
data SetSingletonConstructors a
instance ToSingletonConstructors a => ModifyOptions (SetSingletonConstructors a) where
  modifyOptions = setSingletonConstructors (asSingletonConstructors @a)

-- | Convert a type of kind @SingletonConstructors@
--   into a value of type @SingletonConstructors@
class ToSingletonConstructors (a :: SingletonConstructors) where
  asSingletonConstructors :: SingletonConstructors

-- | Type-level version of 'Dhall.Bare'
type Bare = 'Bare
instance ToSingletonConstructors Bare where
  asSingletonConstructors = Bare

-- | Type-level version of 'Dhall.Wrapped'
type Wrapped = 'Wrapped
instance ToSingletonConstructors Wrapped where
  asSingletonConstructors = Wrapped

-- | Type-level version of 'Dhall.Smart'
type Smart = 'Smart
instance ToSingletonConstructors Smart where
  asSingletonConstructors = Smart


-- * Text helper

-- | @dropPrefix prefix text@ returns the suffix of @text@ if its prefix
--   matches @prefix@, or the entire @text@ otherwise
dropPrefix :: Text -> (Text -> Text)
dropPrefix prefix text = case Text.stripPrefix prefix text of
  Just stripped -> stripped
  Nothing       -> text

-- * InterpretOptions setters

-- | @addFieldModifier f options@ Post-composes the @fieldModifier@
--  from @options@ with @f@.
addFieldModifier :: (Text -> Text) -> InterpretOptions -> InterpretOptions
addFieldModifier f options = options
  { fieldModifier = f . fieldModifier options }

-- | @addConstructorModifier f options@ Post-composes the @constructorModifier@
--   from @options@ with @f@.
addConstructorModifier :: (Text -> Text) -> InterpretOptions -> InterpretOptions
addConstructorModifier f options = options
  { constructorModifier = f . constructorModifier options }

-- | @setSingletonConstructors v options@ Replaces the @singletonConstructors@
--  from @options@ with @v@.
setSingletonConstructors :: SingletonConstructors -> InterpretOptions -> InterpretOptions
setSingletonConstructors v options = options
  { singletonConstructors = v }
