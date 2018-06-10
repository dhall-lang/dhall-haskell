{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Dhall.Parser.Combinators where


import           Control.Applicative        (Alternative (..), liftA2)
import           Control.Monad              (MonadPlus)
import           Data.Data                  (Data)
import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import           Data.Semigroup             (Semigroup (..))
import           Data.Sequence              (ViewL (..))
import           Data.Set                   (Set)
import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           Formatting.Buildable       (Buildable (..))
import           Prelude                    hiding (const, pi)
import           Text.Parser.Combinators    (try, (<?>))
import           Text.Parser.Token          (TokenParsing (..))

import qualified Data.Char
import qualified Data.HashMap.Strict.InsOrd
import qualified Data.List
import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Text
import qualified Text.Megaparsec
import qualified Text.Megaparsec.Char
import qualified Text.Parser.Char
import qualified Text.Parser.Combinators
import qualified Text.Parser.Token.Style

-- | Source code extract
data Src = Src Text.Megaparsec.SourcePos Text.Megaparsec.SourcePos Text
  deriving (Data, Eq, Show)

instance Buildable Src where
    build (Src begin _ text) =
            build text <> "\n"
        <>  "\n"
        <>  build (Text.Megaparsec.sourcePosPretty begin)
        <>  "\n"

{-| A `Parser` that is almost identical to
    @"Text.Megaparsec".`Text.Megaparsec.Parsec`@ except treating Haskell-style
    comments as whitespace
-}
newtype Parser a = Parser { unParser :: Text.Megaparsec.Parsec Void Text a }
    deriving
    (   Functor
    ,   Applicative
    ,   Monad
    ,   Alternative
    ,   MonadPlus
    ,   Text.Megaparsec.MonadParsec Void Text
    )

instance Data.Semigroup.Semigroup a => Data.Semigroup.Semigroup (Parser a) where
    (<>) = liftA2 (<>)

instance (Data.Semigroup.Semigroup a, Monoid a) => Monoid (Parser a) where
    mempty = pure mempty

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

instance IsString a => IsString (Parser a) where
    fromString x = fromString x <$ Text.Megaparsec.Char.string (fromString x)

instance Text.Parser.Combinators.Parsing Parser where
  try = Text.Megaparsec.try

  (<?>) = (Text.Megaparsec.<?>)

  skipMany = Text.Megaparsec.skipMany

  skipSome = Text.Megaparsec.skipSome

  unexpected = fail

  eof = Parser Text.Megaparsec.eof

  notFollowedBy = Text.Megaparsec.notFollowedBy

instance Text.Parser.Char.CharParsing Parser where
  satisfy = Parser . Text.Megaparsec.Char.satisfy

  char = Text.Megaparsec.Char.char

  notChar = Text.Megaparsec.Char.char

  anyChar = Text.Megaparsec.Char.anyChar

  string = fmap Data.Text.unpack . Text.Megaparsec.Char.string . fromString

  text = Text.Megaparsec.Char.string

instance TokenParsing Parser where
    someSpace =
        Text.Parser.Token.Style.buildSomeSpaceParser
            (Parser (Text.Megaparsec.skipSome (Text.Megaparsec.Char.satisfy Data.Char.isSpace)))
            Text.Parser.Token.Style.haskellCommentStyle

    highlight _ = id

    semi = token (Text.Megaparsec.Char.char ';' <?> ";")

count :: (Semigroup a, Monoid a) => Int -> Parser a -> Parser a
count n parser = mconcat (replicate n parser)

range :: (Semigroup a, Monoid a) => Int -> Int -> Parser a -> Parser a
range minimumBound maximumMatches parser =
    count minimumBound parser <> loop maximumMatches
  where
    loop 0 = mempty
    loop n = (parser <> loop (n - 1)) <|> mempty

option :: (Alternative f, Monoid a) => f a -> f a
option p = p <|> pure mempty

star :: (Alternative f, Monoid a) => f a -> f a
star p = plus p <|> pure mempty

plus :: (Alternative f, Monoid a) => f a -> f a
plus p = mappend <$> p <*> star p

satisfy :: (Char -> Bool) -> Parser Text
satisfy = fmap Data.Text.singleton . Text.Parser.Char.satisfy

noDuplicates :: Ord a => [a] -> Parser (Set a)
noDuplicates = go Data.Set.empty
  where
    go found    []  = return found
    go found (x:xs) =
        if Data.Set.member x found
        then fail "Duplicate key"
        else go (Data.Set.insert x found) xs

toMap :: [(Text, a)] -> Parser (InsOrdHashMap Text a)
toMap kvs = do
    let adapt (k, v) = (k, pure v)
    let m = fromListWith (<|>) (fmap adapt kvs)
    let action k vs = case Data.Sequence.viewl vs of
            EmptyL  -> empty
            v :< vs' ->
                if null vs'
                then pure v
                else
                    Text.Parser.Combinators.unexpected
                        ("duplicate field: " ++ Data.Text.unpack k)
    Data.HashMap.Strict.InsOrd.traverseWithKey action m
  where
    fromListWith combine = Data.List.foldl' snoc nil
      where
        nil = Data.HashMap.Strict.InsOrd.empty

        snoc m (k, v) = Data.HashMap.Strict.InsOrd.insertWith combine k v m
