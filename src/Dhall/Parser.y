{
-- | Parsing logic for the Dhall language

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}

module Dhall.Parser (
    -- * Parser
      exprFromBytes

    -- * Types
    , ParseError(..)
    ) where

import Control.Exception (Exception)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import Data.Typeable (Typeable)
import Dhall.Core
import Dhall.Lexer (Alex, AlexPosn(..), Token)

import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Vector
import qualified Data.Text
import qualified Data.Text.Buildable
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Encoding
import qualified Dhall.Lexer
import qualified NeatInterpolation
}

%name expr
%tokentype { Token }
%error { parseError }
%lexer { Dhall.Lexer.lexer } { Dhall.Lexer.EOF }
%monad { Alex }

%token
    '('            { Dhall.Lexer.OpenParen        }
    ')'            { Dhall.Lexer.CloseParen       }
    '{'            { Dhall.Lexer.OpenBrace        }
    '}'            { Dhall.Lexer.CloseBrace       }
    '{{'           { Dhall.Lexer.DoubleOpenBrace  }
    '}}'           { Dhall.Lexer.DoubleCloseBrace }
    '['            { Dhall.Lexer.OpenBracket      }
    ']'            { Dhall.Lexer.CloseBracket     }
    ':'            { Dhall.Lexer.Colon            }
    ','            { Dhall.Lexer.Comma            }
    '.'            { Dhall.Lexer.Dot              }
    '='            { Dhall.Lexer.Equals           }
    '&&'           { Dhall.Lexer.And              }
    '||'           { Dhall.Lexer.Or               }
    '+'            { Dhall.Lexer.Plus             }
    '<>'           { Dhall.Lexer.Diamond          }
    '++'           { Dhall.Lexer.DoublePlus       }
    '*'            { Dhall.Lexer.Star             }
    'let'          { Dhall.Lexer.Let              }
    'in'           { Dhall.Lexer.In               }
    'Type'         { Dhall.Lexer.Type             }
    'Kind'         { Dhall.Lexer.Kind             }
    '->'           { Dhall.Lexer.Arrow            }
    'forall'       { Dhall.Lexer.Forall           }
    '\\'           { Dhall.Lexer.Lambda           }
    'Bool'         { Dhall.Lexer.Bool             }
    'True'         { Dhall.Lexer.True_            }
    'False'        { Dhall.Lexer.False_           }
    'if'           { Dhall.Lexer.If               }
    'then'         { Dhall.Lexer.Then             }
    'else'         { Dhall.Lexer.Else             }
    'Natural'      { Dhall.Lexer.Natural          }
    'Natural/fold' { Dhall.Lexer.NaturalFold      }
    'Integer'      { Dhall.Lexer.Integer          }
    'Double'       { Dhall.Lexer.Double           }
    'Text'         { Dhall.Lexer.Text             }
    'List'         { Dhall.Lexer.List             }
    'List/build'   { Dhall.Lexer.ListBuild        }
    'List/fold'    { Dhall.Lexer.ListFold         }
    'List/indexed' { Dhall.Lexer.ListIndexed      }
    'Maybe'        { Dhall.Lexer.Maybe            }
    'Maybe/fold'   { Dhall.Lexer.MaybeFold        }
    text           { Dhall.Lexer.TextLit    $$    }
    label          { Dhall.Lexer.Label      $$    }
    number         { Dhall.Lexer.Number     $$    }
    double         { Dhall.Lexer.DoubleLit  $$    }
    natural        { Dhall.Lexer.NaturalLit $$    }
    url            { Dhall.Lexer.URL        $$    }
    file           { Dhall.Lexer.File       $$    }

%right '||'
%right '&&'
%right '+'
%right '*'
%right '++'
%right '<>'

%%

Expr0
    : Expr1 ':' Expr0
        { Annot $1 $3 }
    | Expr1
        { $1 }

Expr1
    : '\\' '(' label ':' Expr0 ')' '->' Expr1
        { Lam $3 $5 $8 }
    | 'if' Expr0 'then' Expr1 'else' Expr1
        { BoolIf $2 $4 $6 }
    | 'forall' '(' label ':' Expr0 ')' '->' Expr1
        { Pi $3 $5 $8 }
    | Expr2 '->' Expr1
        { Pi "_" $1 $3 }
    | Lets 'in' Expr1
        { Lets $1 $3 }
    | '[' Elems ']' ':' ListLike Expr5
        { $5 $6 (Data.Vector.fromList $2) }
    | Expr2
        { $1 }

ListLike
    : 'List'
        { ListLit }
    | 'Maybe'
        { MaybeLit }

Expr2
    : Expr2 '||' Expr2
        { BoolOr $1 $3 }
    | Expr2 '+' Expr2
        { NaturalPlus $1 $3 }
    | Expr2 '<>' Expr2
        { TextAppend $1 $3 }
    | Expr2 '++' Expr2
        { ListConcat $1 $3 }
    | Expr3
        { $1 }

Expr3
    : Expr3 '&&' Expr3
        { BoolAnd $1 $3 }
    | Expr3 '*' Expr3
        { NaturalTimes $1 $3 }
    | Expr4
        { $1 }

Expr4
    : Expr4 Expr5
        { App $1 $2 }
    | Expr5
        { $1 }

Expr5
    : label
        { Var $1 }
    | 'Type'
        { Const Type }
    | 'Kind'
        { Const Kind }
    | 'Bool'
        { Bool }
    | 'Natural'
        { Natural }
    | 'Natural/fold'
        { NaturalFold }
    | 'Integer'
        { Integer }
    | 'Double'
        { Double }
    | 'Text'
        { Text }
    | 'List'
        { List }
    | 'List/build'
        { ListBuild }
    | 'List/fold'
        { ListFold }
    | 'List/indexed'
        { ListIndexed }
    | 'Maybe'
        { Maybe }
    | 'Maybe/fold'
        { MaybeFold }
    | 'True'
        { BoolLit True }
    | 'False'
        { BoolLit False }
    | number
        { IntegerLit (fromIntegral $1) }
    | natural
        { NaturalLit $1 }
    | double
        { DoubleLit $1 }
    | text
        { TextLit $1 }
    | RecordLit
        { $1 }
    | Record
        { $1 }
    | Import
        { Embed $1 }
    | Expr5 '.' label
        { Field $1 $3 }
    | '(' Expr0 ')'
        { $2 }
    
Lets
    : LetsRev
        { reverse $1 }

LetsRev
    : Let
        { [$1] }
    | LetsRev Let
        { $2 : $1 }

Let
    : 'let' label Args '=' Expr0
        { Let $2 $3 $5 }

Args
    : ArgsRev
        { reverse $1 }

ArgsRev
    : {- empty -}
        { [] }
    | ArgsRev Arg
        { $2 : $1 }

Arg
    : '(' label ':' Expr0 ')'
        { ($2, $4) }

Elems
    : ElemsRev
        { reverse $1 }

ElemsRev
    : {- empty -}
        { [] }
    | Expr0
        { [$1] }
    | ElemsRev ',' Expr0
        { $3 : $1 }

RecordLit
    : '{' FieldValues '}'
        { RecordLit (Data.Map.fromList $2) }

FieldValues
    : FieldValuesRev
        { reverse $1 }

FieldValuesRev
    : {- empty -}
        { [] }
    | FieldValue
        { [$1] }
    | FieldValuesRev ',' FieldValue
        { $3 : $1 }

FieldValue
    : label '=' Expr0 
        { ($1, $3) }

Record
    : '{{' FieldTypes '}}' 
        { Record (Data.Map.fromList $2) }

FieldTypes
    : FieldTypesRev
        { reverse $1 }

FieldTypesRev
    : {- empty -}
        { [] }
    | FieldType
        { [$1] }
    | FieldTypesRev ',' FieldType
        { $3 : $1 }

FieldType
    : label ':' Expr0
        { ($1, $3) } 

Import
    : file
        { File $1 }
    | url
        { URL $1 }

{
parseError :: Token -> Alex a
parseError token = do
    (AlexPn _ line column, _, bytes, _) <- Dhall.Lexer.alexGetInput
    Dhall.Lexer.alexError (Data.Text.unpack (msg line column bytes))
  where
    msg line column bytes = [NeatInterpolation.text|
Error: Parsing failed

Explanation: The source code is decomposed into a sequence of tokens and these
tokens are then parsed to generate a syntax tree

The parsing step failed to generate a syntax tree due to this unexpected token:
↳ $txt0
... ending at:
↳ Line $txt1, Column $txt2
... with the following unconsumed input:
↳ $txt3
|]
      where
        txt0 =
             Data.Text.Lazy.toStrict
                 (Data.Text.Lazy.Builder.toLazyText
                     (Data.Text.Buildable.build token) )
        txt1 = Data.Text.pack (show line)
        txt2 = Data.Text.pack (show column)
        txt3 = Data.Text.take 76 (Data.Text.pack (show bytes))

-- | A parsing error
newtype ParseError = ParseError Text
    deriving (Typeable)

instance Show ParseError where
    show (ParseError txt) = Data.Text.Lazy.unpack txt

instance Exception ParseError

-- | Parse an expression from a `ByteString`
exprFromBytes :: ByteString -> Either ParseError (Expr Path)
exprFromBytes bytes = case Dhall.Lexer.runAlex bytes expr of
    Left  str -> Left (ParseError (Data.Text.Lazy.pack str))
    Right e   -> Right e
}
