{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Dhall.Parser.Combinators where


import           Control.Applicative        (Alternative (..), liftA2)
import           Control.Monad              (MonadPlus (..))
import           Data.Data                  (Data)
import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import           Data.Semigroup             (Semigroup (..))
import           Data.Sequence              (ViewL (..))
import           Data.Set                   (Set)
import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import           Data.Text.Prettyprint.Doc  (Pretty (..))
import           Data.Void                  (Void)
import           Prelude                    hiding (const, pi)
import           Text.Parser.Combinators    (try, (<?>))
import           Text.Parser.Token          (TokenParsing (..))

import qualified Data.Char
import qualified Data.HashMap.Strict.InsOrd
import qualified Data.List
import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Text
import qualified Control.Monad.Fail
import qualified Text.Megaparsec
import qualified Text.Megaparsec.Char
import qualified Text.Parser.Char
import qualified Text.Parser.Combinators
import qualified Text.Parser.Token.Style

-- | Source code extract
data Src = Src !Text.Megaparsec.SourcePos !Text.Megaparsec.SourcePos Text
  -- Text field is intentionally lazy
  deriving (Data, Eq, Show)

-- | Doesn't force the 'Text' part
laxSrcEq :: Src -> Src -> Bool
laxSrcEq (Src p q _) (Src p' q' _) = eq p  p' && eq q q'
  where
    -- Don't compare filename (which is FilePath = String)
    eq  :: Text.Megaparsec.SourcePos -> Text.Megaparsec.SourcePos -> Bool
    eq (Text.Megaparsec.SourcePos _ a b) (Text.Megaparsec.SourcePos _ a' b') =
        a == a' && b == b'
{-# INLINE laxSrcEq #-}

instance Pretty Src where
    pretty (Src begin _ text) =
            preview
        <>  "\n"
        <>  pretty (Text.Megaparsec.sourcePosPretty begin)
      where
        prefix = Data.Text.replicate (n - 1) " "
          where
            n = Text.Megaparsec.unPos (Text.Megaparsec.sourceColumn begin)

        ls = Data.Text.lines (prefix <> text)

        header = take 3 ls

        footer = takeEnd 3 ls

        excerpt = filter (Data.Text.any (/= ' ')) (header <> footer)

        leadingSpaces =
            Data.Text.length . Data.Text.takeWhile (== ' ')

        minSpaces = minimum (map leadingSpaces excerpt)

        maxLength = maximum (map Data.Text.length excerpt)

        snip = Data.Text.replicate minSpaces " "
            <> Data.Text.replicate (maxLength - minSpaces) "="

        preview
            | length ls <= 6 =
                    pretty text <> "\n"
            | otherwise =
                    pretty (Data.Text.unlines header)
                <>  pretty snip <> "\n"
                <>  pretty (Data.Text.unlines footer)

takeEnd :: Int -> [a] -> [a]
takeEnd n l = go (drop n l) l
  where
    go (_:xs) (_:ys) = go xs ys
    go _ r = r

{-| A `Parser` that is almost identical to
    @"Text.Megaparsec".`Text.Megaparsec.Parsec`@ except treating Haskell-style
    comments as whitespace
-}
newtype Parser a = Parser { unParser :: Text.Megaparsec.Parsec Void Text a }

instance Functor Parser where
    fmap f (Parser x) = Parser (fmap f x)
    {-# INLINE fmap #-}

    f <$ Parser x = Parser (f <$ x)
    {-# INLINE (<$) #-}

instance Applicative Parser where
    pure = Parser . pure
    {-# INLINE pure #-}

    Parser f <*> Parser x = Parser (f <*> x)
    {-# INLINE (<*>) #-}

    Parser a <* Parser b = Parser (a <* b)
    {-# INLINE (<*) #-}

    Parser a *> Parser b = Parser (a *> b)
    {-# INLINE (*>) #-}

instance Monad Parser where
    return = pure
    {-# INLINE return #-}

    (>>) = (*>)
    {-# INLINE (>>) #-}

    Parser n >>= k = Parser (n >>= unParser . k)
    {-# INLINE (>>=) #-}

    fail = Control.Monad.Fail.fail
    {-# INLINE fail #-}

instance Control.Monad.Fail.MonadFail Parser where
    fail = Parser . Control.Monad.Fail.fail
    {-# INLINE fail #-}

instance Alternative Parser where
    empty = Parser empty
    -- {-# INLINE empty #-}

    Parser a <|> Parser b = Parser (a <|> b)
    -- {-# INLINE (<|>) #-}

    some (Parser a) = Parser (some a)
    -- {-# INLINE some #-}

    many (Parser a) = Parser (many a)
    -- {-# INLINE many #-}

instance MonadPlus Parser where
    mzero = empty
    -- {-# INLINE mzero #-}

    mplus = (<|>)
    -- {-# INLINE mplus #-}

instance Text.Megaparsec.MonadParsec Void Text Parser where
    failure u e    = Parser (Text.Megaparsec.failure u e)

    fancyFailure e = Parser (Text.Megaparsec.fancyFailure e)

    label l (Parser p) = Parser (Text.Megaparsec.label l p)

    hidden (Parser p) = Parser (Text.Megaparsec.hidden p)

    try (Parser p) = Parser (Text.Megaparsec.try p)

    lookAhead (Parser p) = Parser (Text.Megaparsec.lookAhead p)

    notFollowedBy (Parser p) = Parser (Text.Megaparsec.notFollowedBy p)

    withRecovery e (Parser p) = Parser (Text.Megaparsec.withRecovery (unParser . e) p)

    observing (Parser p) = Parser (Text.Megaparsec.observing p)

    eof = Parser Text.Megaparsec.eof

    token f e = Parser (Text.Megaparsec.token f e)

    tokens f ts = Parser (Text.Megaparsec.tokens f ts)

    takeWhileP s f = Parser (Text.Megaparsec.takeWhileP s f)

    takeWhile1P s f = Parser (Text.Megaparsec.takeWhile1P s f)

    takeP s n = Parser (Text.Megaparsec.takeP s n)

    getParserState = Parser Text.Megaparsec.getParserState
    {-# INLINE getParserState #-}

    updateParserState f = Parser (Text.Megaparsec.updateParserState f)

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
  satisfy = Parser . Text.Megaparsec.satisfy

  char = Text.Megaparsec.Char.char

  notChar = Text.Megaparsec.Char.char

  anyChar = Text.Megaparsec.anySingle

  string = fmap Data.Text.unpack . Text.Megaparsec.Char.string . fromString

  text = Text.Megaparsec.Char.string

instance TokenParsing Parser where
    someSpace =
        Text.Parser.Token.Style.buildSomeSpaceParser
            (Parser (Text.Megaparsec.skipSome (Text.Megaparsec.satisfy Data.Char.isSpace)))
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

takeWhile :: (Char -> Bool) -> Parser Text
takeWhile predicate = Parser (Text.Megaparsec.takeWhileP Nothing predicate)

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 predicate = Parser (Text.Megaparsec.takeWhile1P Nothing predicate)

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
