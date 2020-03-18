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

{-| Newtypes for writing customizable 'FromDhall' and 'ToDhall' instances
    through the DerivingVia strategy.

    Inspired by Matt Parson's blog post
    [Mirror Mirror: Reflection and Encoding Via](https://www.parsonsmatt.org/2020/02/04/mirror_mirror.html),
    but applied to Dhall instead of JSON.

    This module is intended to be used with
    [DerivingVia](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DerivingVia)
    so it's only available for GHC >= v8.6.1.

    Check "Dhall.Deriving#derivingVia" if you want to see this module in action.

-}

module Dhall.Deriving
  (
    -- * Introduction
    -- $introduction

    -- * Writing FromDhall instances by hand
    -- $instancesByHand

    -- * Letting DerivingVia do the work
    -- $derivingVia

    -- * Behind the scenes of Codec
    -- $behindTheScenes

    -- * DerivingVia newtype
    Codec (..)

    -- * Type-level functions on InterpretOptions
  , ModifyOptions (..)
  , Field
  , Constructor
  , SetSingletonConstructors

    -- * Type-level functions on Text
  , TextFunction (..)
  , DropPrefix
  , TitleCase
  , CamelCase
  , PascalCase
  , SnakeCase
  , SpinalCase
  , TrainCase

    -- * Type-level versions of SingletonConstructors
  , ToSingletonConstructors
  , Bare
  , Wrapped
  , Smart

    -- * Identity and Composition for ModifyOptions and TextFunction
  , AsIs
  , type (<<<)

    -- * Helper function on Text
  , dropPrefix

    -- * InterpretOptions setters
  , addFieldModifier
  , addConstructorModifier
  , setSingletonConstructors

  ) where

import Data.Proxy (Proxy (..))
import Dhall
import GHC.Generics (Generic (Rep))
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


-- | The identity for functions on 'InterpretOptions' and on @Text@.
--   Useful for deriving @FromDhall@ and @ToDhall@ with the default options.
type AsIs = ()

instance ModifyOptions AsIs where
  modifyOptions = id

instance TextFunction AsIs where
  textFunction = id


-- | Composition for functions on 'InterpretOptions' and on @Text@.
--   We use @<<<@ since @.@ isn't a valid type operator yet
--   (it will be valid starting from ghc-8.8.1)
data a <<< b
infixr 1 <<<

instance (ModifyOptions a, ModifyOptions b) => ModifyOptions (a <<< b) where
  modifyOptions = modifyOptions @a . modifyOptions @b

instance (TextFunction a, TextFunction b) => TextFunction (a <<< b) where
  textFunction = textFunction @a . textFunction @b


-- | @Field t@ post-composes the @fieldModifier@ from @options@ with the
--   value-level version of @t@, obtained with @TextFunction@
data Field a
instance TextFunction a => ModifyOptions (Field a) where
  modifyOptions = addFieldModifier (textFunction @a)

-- | @Constructor t@ post-composes the @constructorModifier@ from @options@
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


-- | @SetSingletonConstructors t@ replaces the @singletonConstructors@
--  from @options@ with the value-level version of @t@.
data SetSingletonConstructors a
instance ToSingletonConstructors a => ModifyOptions (SetSingletonConstructors a) where
  modifyOptions = setSingletonConstructors (asSingletonConstructors @a)

-- | Convert a type of kind @SingletonConstructors@
--   into a value of type @SingletonConstructors@
class ToSingletonConstructors (a :: SingletonConstructors) where
  asSingletonConstructors :: SingletonConstructors

-- | Type-level version of 'Dhall.Bare'.
-- Never wrap the field of a singleton constructor in a record
type Bare = 'Bare
instance ToSingletonConstructors Bare where
  asSingletonConstructors = Bare

-- | Type-level version of 'Dhall.Wrapped'
-- Always wrap the field of a singleton constructor in a record
type Wrapped = 'Wrapped
instance ToSingletonConstructors Wrapped where
  asSingletonConstructors = Wrapped

-- | Type-level version of 'Dhall.Smart'
-- Wrap the field of a singleton constructor in a record
-- only if the field is named
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

-- | @addFieldModifier f options@ post-composes the @fieldModifier@
--  from @options@ with @f@.
addFieldModifier :: (Text -> Text) -> InterpretOptions -> InterpretOptions
addFieldModifier f options = options
  { fieldModifier = f . fieldModifier options }

-- | @addConstructorModifier f options@ post-composes the @constructorModifier@
--   from @options@ with @f@.
addConstructorModifier :: (Text -> Text) -> InterpretOptions -> InterpretOptions
addConstructorModifier f options = options
  { constructorModifier = f . constructorModifier options }

-- | @setSingletonConstructors v options@ replaces the @singletonConstructors@
--  from @options@ with @v@.
setSingletonConstructors :: SingletonConstructors -> InterpretOptions -> InterpretOptions
setSingletonConstructors v options = options
  { singletonConstructors = v }

{- $introduction

Let's take the following Haskell data types:

>>> :set -XDerivingStrategies

>>> :{
newtype Name = Name { getName :: Text }
  deriving stock (Show)
:}

>>> :{
data Font = Arial | ComicSans | Helvetica | TimesNewRoman
  deriving stock (Show)
:}

>>> :{
data Person = Person
  { personName :: Name
  , personFavoriteFont :: Font
  }
  deriving stock (Show)
:}

And assume we want to read the following Dhall file as a @Person@:

@
-- ./simon.dhall
let Name = Text
let Font = \< Arial | `Comic Sans` | Helvetica | `Times New Roman` \>
let Person = { name : Name, favoriteFont : Font }
in  { name = \"Simon\", favoriteFont = Font.`Comic Sans` } : Person
@

Usually, you would build a 'Decoder' by hand, like this

>>> :{
font :: Decoder Font
font =
  union
    (  (Arial         <$ constructor "Arial"           unit)
    <> (ComicSans     <$ constructor "Comic Sans"      unit)
    <> (Helvetica     <$ constructor "Helvetica"       unit)
    <> (TimesNewRoman <$ constructor "Times New Roman" unit)
    )
:}

>>> :{
name :: Decoder Name
name = Name <$> strictText
:}

>>> :{
person :: Decoder Person
person =
  record
    ( Person <$> field "name"         name
             <*> field "favoriteFont" font
    )
:}

and then you use it like this

>>> input person "./simon.dhall"
Person {personName = Name {getName = "Simon"}, personFavoriteFont = ComicSans}

So, it works! However, this is quite mechanic, and the compiler has pretty
much all the information it needs to do it for you. Besides, you'd like to
provide an instance of 'FromDhall' so you can use the polymorphic 'Decoder'
'auto' instead of explicitly calling @person@.
-}

{- $instancesByHand
"Aha!," you think, "I'll write an empty @instance 'FromDhall' Person@".
That in turn requires you to add two other instances for @Font@ and for @Name@,
plus 'Generic' instances for each of those, but that's okay.

>>> :set -XStandaloneDeriving
>>> :set -XDeriveGeneric

>>> :{
deriving stock instance Generic Name
deriving stock instance Generic Font
deriving stock instance Generic Person
:}

>>> :{
instance FromDhall Name
instance FromDhall Font
instance FromDhall Person
:}

However, when you try to read the same file with 'auto', you get this:

>>> input auto "./simon.dhall" :: IO Person
*** Exception:
...Error...: Expression doesn't match annotation
...
{ - personFavoriteFont : …
, - personName : …
, + favoriteFont : …
, + name : …
}
...
1│ ./simon.dhall : { personName : { getName : Text }
2│ , personFavoriteFont : < Arial | ComicSans | Helvetica | TimesNewRoman >
3│ }
...

What happened? The field names don't quite match, since we're using prefixed
field names in Haskell but no prefixes in Dhall. "Okay," you think,
"I can write a custom instance which builds on 'Generic' thanks to
'genericAutoWith', I only need to supply a function to drop the prefixes
and @camelCase@ the rest". So, using 'Data.Text.Manipulate.toCamel':

>>> import Data.Text.Manipulate (toCamel)
>>> import qualified Data.Text as Text
>>> :{
instance FromDhall Person where
  autoWith _ =
    genericAutoWith defaultInterpretOptions
      { fieldModifier = toCamel . Text.drop (Text.length "person") }
:}

Let's try to read that again:

>>> input auto "./simon.dhall":: IO Person
*** Exception:
...Error...: Expression doesn't match annotation
...
{ favoriteFont : < - ComicSans : …
                 | - TimesNewRoman : …
                 | + `Comic Sans` : …
                 | + `Times New Roman` : …
                 | …
                 >
, name : - { … : … }
         + Text
}
...
1│ ./simon.dhall : { name : { getName : Text }
2│ , favoriteFont : < Arial | ComicSans | Helvetica | TimesNewRoman >
3│ }
...

Okay, we're almost there. We have two things to solve now.
First, the @Font@ constructors are @PascalCased@ in Haskell,
but @Title Cased@ in Dhall. We can communicate this to our
'FromDhall' instance using 'Data.Text.Manipulate.toTitle':

>>> import Data.Text.Manipulate (toTitle)
>>> :{
instance FromDhall Font where
  autoWith _ =
    genericAutoWith defaultInterpretOptions
      { constructorModifier = toTitle }
:}

Second, we defined the @Name@ type in Haskell as a newtype over @Text@, with a
@getName@ field for unwrapping. In Dhall, however, @Name@ is a synonym of
'Text', which is why 'input' above was expecting a record.
The 'Dhall.Bare' option for 'singletonConstructors' is a perfect fit here:
it translates Haskell singleton constructors into the Dhall version of the
nested type, without wrapping it into a record.
We can then tweak our 'FromDhall' instance like this:

>>> :{
instance FromDhall Name where
  autoWith _ =
    genericAutoWith defaultInterpretOptions
      { singletonConstructors = Bare }
:}

Since we're running this interactively, we also need to update the
instance for @Person@, but it's the same as before.

>>> :{
instance FromDhall Person where
  autoWith _ =
    genericAutoWith defaultInterpretOptions
      { fieldModifier = toCamel . Text.drop (Text.length "person") }
:}

Now, for the moment of truth:

>>> input auto "./simon.dhall":: IO Person
Person {personName = Name {getName = "Simon"}, personFavoriteFont = ComicSans}

That took a bit more work than we wanted, though, and a lot of it was just
boilerplate for defining the instances through `genericAutoWith`, tweaking
a single parameter at a time. Even worse, if we also wanted to provide
'ToDhall' instances we would need to keep the options in sync between both
instances, since otherwise the values wouldn't be able to round-trip from
Dhall to Dhall through Haskell.

-}

{- $derivingVia
   #derivingVia#

Starting with this dhall file:

@
-- ./simon.dhall
let Name = Text
let Font = \< Arial | `Comic Sans` | Helvetica | `Times New Roman` \>
let Person = { name : Name, favoriteFont : Font }
in  { name = \"Simon\", favoriteFont = Font.`Comic Sans` } : Person
@

We can define the equivalent Haskell types as follows. Note that we
derive the 'FromDhall' and 'ToDhall' instances @via 'Codec' tag TheType@,
using a different @tag@ depending on the transformations we need to apply to
the Haskell type to get the Dhall equivalent:

>>> :set -XDataKinds
>>> :set -XDeriveGeneric
>>> :set -XDerivingVia
>>> :set -XTypeOperators

>>> :{
newtype Name = Name { getName :: Text }
  deriving stock (Generic, Show)
  deriving (FromDhall, ToDhall)
    via Codec (SetSingletonConstructors Bare) Name
:}

>>> :{
data Font = Arial | ComicSans | Helvetica | TimesNewRoman
  deriving stock (Generic, Show)
  deriving (FromDhall, ToDhall)
    via Codec (Constructor TitleCase) Font
:}

>>> :{
data Person = Person
  { personName :: Name
  , personFavoriteFont :: Font
  }
  deriving stock (Generic, Show)
  deriving (FromDhall, ToDhall)
    via Codec (Field (CamelCase <<< DropPrefix "person")) Person
:}

we can then read the file using 'auto':

>>> simon <- input auto "./simon.dhall":: IO Person
>>> print simon
Person {personName = Name {getName = "Simon"}, personFavoriteFont = ComicSans}

And using 'inject' we can get @simon@ back as a Dhall value:

>>> import qualified Data.Text.IO as Text
>>> import Dhall.Core (pretty)
>>> Text.putStrLn . pretty . embed inject $ simon
{ name = "Simon"
, favoriteFont =
    < Arial | `Comic Sans` | Helvetica | `Times New Roman` >.`Comic Sans`
}

-}

{- $behindTheScenes

@'Codec' tag a@ is really just a newtype over @a@, equipped with a
phantom @tag@. The 'FromDhall' instance for 'Codec' uses the generic
representation of @a@, together with the 'InterpretOptions' defined by @tag@ as
a series of modifications to be applied on 'defaultInterpretOptions'.

For the default behavior, using 'AsIs' (a synonym for @()@) as the @tag@
leaves the interpret options alone, so it's equivalent to the empty instance
we first tried to use.

@'Field' a@ and @'Constructor' a@ can be used to modify, respectively, the
'fieldModifier' and 'constructorModifier' options of 'InterpretOptions', by
post-composing the modifier with @'textFunction' \@a@, that is, the value-level
equivalent of @a@, obtained through the 'TextFunction' class.

In the case of @Person@, we used

@
  Codec (Field (CamelCase <<< DropPrefix "person")) Person
@

which means that the @Text -> Text@ version of

@
  CamelCase <<< DropPrefix "person"
@

was used to modify the @fieldModifier@ option.

In the value level, this translates to composing ('<<<')
'Data.Text.Manipulate.toCamel' ('CamelCase') with @'dropPrefix' "person"@
(@'DropPrefix' "person"@).

Finally, @'SetSingletonConstructors' a@ can be used to set the
'singletonConstructors' option of 'InterpretOptions', by replacing the option
with the value-level equivalent of @a@.
-}

{- $setup
>>> :set -XOverloadedStrings
-}
