{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Lexing logic for the Dhall language
module Dhall.Lexer (
    -- * Lexer
      lexer

    -- * Types
    , Token(..)

    -- * Re-exports
    , Alex
    , AlexPosn(..)
    , alexError
    , alexGetInput
    , runAlex
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))
import Data.Text.Buildable (Buildable(..))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder)
import Filesystem.Path (FilePath)
import Numeric.Natural (Natural)
import Prelude hiding (FilePath)

import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lex.Fractional
import qualified Data.ByteString.Lex.Integral
import qualified Data.Text
import qualified Data.Text.Buildable
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.Builder
import qualified Filesystem.Path.CurrentOS
import qualified NeatInterpolation
}

%wrapper "monad-bytestring"

$digit = 0-9

-- Same as Haskell
$opchar = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]

$fst       = [A-Za-z\_]
$labelchar = [A-Za-z0-9\_\/]

$nonwhite = ~$white

tokens :-

    $white+                             ;
    "--".*                              ;
    "("                                 { emit OpenParen        }
    ")"                                 { emit CloseParen       }
    "{"                                 { emit OpenBrace        }
    "}"                                 { emit CloseBrace       }
    "<"                                 { emit OpenAngle        }
    ">"                                 { emit CloseAngle       }
    "{=}"                               { emit EmptyRecordLit   }
    "["                                 { emit OpenBracket      }
    "]"                                 { emit CloseBracket     }
    ":"                                 { emit Colon            }
    ","                                 { emit Comma            }
    "|"                                 { emit Bar              }
    "."                                 { emit Dot              }
    "="                                 { emit Equals           }
    "&&"                                { emit And              }
    "||"                                { emit Or               }
    "=="                                { emit DoubleEquals     }
    "/="                                { emit SlashEquals      }
    "+"                                 { emit Plus             }
    "++"                                { emit DoublePlus       }
    "*"                                 { emit Star             }
    "@"                                 { emit At               }
    "let"                               { emit Let              }
    "in"                                { emit In               }
    "Type"                              { emit Type             }
    "Kind"                              { emit Kind             }
    "->" | "→"                          { emit Arrow            }
    "forall" | "∀"                      { emit Forall           }
    "\" | "λ"                           { emit Lambda           }
    "Bool"                              { emit Bool             }
    "True"                              { emit True_            }
    "False"                             { emit False_           }
    "apply"                             { emit Apply            }
    "if"                                { emit If               }
    "then"                              { emit Then             }
    "else"                              { emit Else             }
    "Natural"                           { emit Natural          }
    "Natural/fold"                      { emit NaturalFold      }
    "Natural/isZero"                    { emit NaturalIsZero    }
    "Natural/even"                      { emit NaturalEven      }
    "Natural/odd"                       { emit NaturalOdd       }
    "Integer"                           { emit Integer          }
    "Double"                            { emit Double           }
    "Text"                              { emit Text             }
    "List"                              { emit List             }
    "List/build"                        { emit ListBuild        }
    "List/fold"                         { emit ListFold         }
    "List/length"                       { emit ListLength       }
    "List/head"                         { emit ListHead         }
    "List/last"                         { emit ListLast         }
    "List/indexed"                      { emit ListIndexed      }
    "List/reverse"                      { emit ListReverse      }
    "Maybe"                             { emit Maybe            }
    "Maybe/fold"                        { emit MaybeFold        }
    \" ([^\"] | \\.)* \"                { capture (TextLit . str)        }
    $fst $labelchar* | "(" $opchar+ ")" { capture (Label . toText)       }
    \-? $digit+                         { capture (Number . toInt)       }
    $digit+ (\. $digit+)? ([eE][\+\-]? $digit+)?
                                        { capture (DoubleLit . toDouble) }
    "+" $digit+                         { capture (NaturalLit . toNat)   }
    "https://" $nonwhite+               { capture (URL . toText)         }
    "http://" $nonwhite+                { capture (URL . toText)         }
    "/" $nonwhite+                      { capture (File . toFile 0   )   }
    "./" $nonwhite+                     { capture (File . toFile 2   )   }
    "../" $nonwhite+                    { capture (File . toFile 0   )   }
{
emit :: Token -> AlexAction Token
emit x = \_ _ -> return x

alexEOF :: Alex Token
alexEOF = return EOF

capture :: (ByteString -> Token) -> AlexAction Token
capture k (_, _, rest, _) len = return (k bytes)
  where
    bytes = Data.ByteString.Lazy.take len rest

toInt :: ByteString -> Integer
toInt bytes =
    case m of
        Just (n, _) -> n
        Nothing     -> error "toInt: internal error"
  where
    m = Data.ByteString.Lex.Integral.readSigned
            Data.ByteString.Lex.Integral.readDecimal
            (Data.ByteString.Lazy.toStrict bytes)

toDouble :: ByteString -> Double
toDouble bytes =
    case Data.ByteString.Lex.Fractional.readExponential bytes' of
        Just (n, _) -> n
        _           -> error "toDouble: internal error"
  where
    bytes' = Data.ByteString.Lazy.toStrict bytes

toNat :: ByteString -> Natural
toNat = fromIntegral . toInt . Data.ByteString.Lazy.drop 1

toFile :: Int64 -> ByteString -> FilePath
toFile n =
      Filesystem.Path.CurrentOS.fromText
    . Data.Text.Lazy.toStrict
    . Data.Text.Lazy.Encoding.decodeUtf8
    . Data.ByteString.Lazy.drop n

toText :: ByteString -> Text
toText = Data.Text.Lazy.Encoding.decodeUtf8

-- TODO: Properly handle errors here
str :: ByteString -> Builder
str = Data.Text.Lazy.Builder.fromLazyText
    . read
    . Data.Text.Lazy.unpack
    . Data.Text.Lazy.Encoding.decodeUtf8

-- | `Alex` action for reading the next token
lexer :: (Token -> Alex a) -> Alex a
lexer k = alexMonadScan' >>= k

{-| This was forked from the auto-generated `alexMonadScan` function to improve
    the error message
-}
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp@(_,_,str,n) <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),w8,bytes,_) ->
        alexError (Data.Text.unpack [NeatInterpolation.text|
Error: Lexing failed

Explanation: The source code is normally decomposed into a sequence of tokens.
For example, this input:

    λ(a : Type) → a

... is split into these tokens:

    [ "λ", "(", "a", ":", "Type", ")", "→", "a" ]

However, some characters in your program could not be assigned to a recognized
token

Unrecognized input:
↳ $txt0
... beginning at:
↳ Line $txt1, Column $txt2
|])
      where
        txt0 = Data.Text.pack (show input)
        input =
            Data.Text.Lazy.take 76
                (Data.Text.Lazy.cons w8
                    (Data.Text.Lazy.Encoding.decodeUtf8 bytes) )
        txt1 = Data.Text.pack (show line)
        txt2 = Data.Text.pack (show column)
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp'@(_,_,_,n') _ action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len
      where
        len = n'-n

-- | Token type, used to communicate between the lexer and parser
data Token
    = OpenParen
    | CloseParen
    | OpenBrace
    | CloseBrace
    | OpenAngle
    | CloseAngle
    | EmptyRecordLit
    | OpenBracket
    | CloseBracket
    | Colon
    | Comma
    | Bar
    | Dot
    | Equals
    | And
    | Or
    | DoubleEquals
    | SlashEquals
    | Plus
    | DoublePlus
    | At
    | Star
    | Let
    | In
    | Type
    | Kind
    | Arrow
    | Lambda
    | Forall
    | Bool
    | True_
    | False_
    | Apply
    | If
    | Then
    | Else
    | Natural
    | NaturalLit Natural
    | NaturalFold
    | NaturalIsZero
    | NaturalEven
    | NaturalOdd
    | Integer
    | Text
    | Double
    | DoubleLit Double
    | List
    | ListBuild
    | ListFold
    | ListLength
    | ListHead
    | ListLast
    | ListIndexed
    | ListReverse
    | Maybe
    | MaybeFold
    | TextLit Builder
    | Label Text
    | Number Integer
    | File FilePath
    | URL Text
    | EOF
    deriving (Eq, Show)

instance Buildable Token where
    build  OpenParen
        = "("
    build  CloseParen
        = ")"
    build  OpenBrace
        = "{"
    build  CloseBrace
        = "}"
    build  OpenAngle
        = "<"
    build  CloseAngle
        = ">"
    build  EmptyRecordLit
        = "{=}"
    build  OpenBracket
        = "["
    build  CloseBracket
        = "]"
    build  Colon
        = ":"
    build  Comma
        = ","
    build  Bar
        = "|"
    build  Dot
        = "."
    build  Equals
        = "="
    build  And
        = "&&"
    build  Or
        = "||"
    build  DoubleEquals
        = "=="
    build  SlashEquals
        = "/="
    build  Plus
        = "+"
    build  DoublePlus
        = "++"
    build  At
        = "@"
    build  Star
        = "*"
    build  Let
        = "let"
    build  In
        = "in"
    build  Type
        = "Type"
    build  Kind
        = "Kind"
    build  Arrow
        = "->"
    build  Lambda
        = "\\"
    build  Forall
        = "forall"
    build  Bool
        = "Bool"
    build  True_
        = "True"
    build  False_
        = "False"
    build  Apply
        = "apply"
    build  If
        = "if"
    build  Then
        = "then"
    build  Else
        = "else"
    build  Natural
        = "Natural"
    build (NaturalLit n)
        = "+" <> Data.Text.Buildable.build (fromIntegral n :: Integer)
    build  NaturalFold
        = "Natural/fold"
    build  NaturalIsZero
        = "Natural/isZero"
    build  NaturalEven
        = "Natural/even"
    build  NaturalOdd
        = "Natural/odd"
    build  Integer
        = "Integer"
    build  Text
        = "Text"
    build  Double
        = "Double"
    build (DoubleLit n)
        = Data.Text.Buildable.build n
    build  List
        = "List"
    build  ListBuild
        = "List/build"
    build  ListLength
        = "List/length"
    build  ListHead
        = "List/head"
    build  ListLast
        = "List/last"
    build  ListIndexed
        = "List/indexed"
    build  ListReverse
        = "List/reverse"
    build  ListFold
        = "List/fold"
    build  Maybe
        = "Maybe"
    build  MaybeFold
        = "Maybe/fold"
    build (TextLit t)
        = Data.Text.Buildable.build (show t)
    build (Label t)
        = Data.Text.Buildable.build t
    build (Number n)
        = Data.Text.Buildable.build (fromIntegral n :: Integer)
    build (File f)
        = Data.Text.Buildable.build (Filesystem.Path.CurrentOS.encodeString f)
    build (URL t)
        = Data.Text.Buildable.build t
    build  EOF
        = "EOF"
}
