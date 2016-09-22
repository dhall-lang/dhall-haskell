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
    '('               { Dhall.Lexer.OpenParen      }
    ')'               { Dhall.Lexer.CloseParen     }
    '{'               { Dhall.Lexer.OpenBrace      }
    '}'               { Dhall.Lexer.CloseBrace     }
    '{:}'             { Dhall.Lexer.EmptyRecord    }
    '<'               { Dhall.Lexer.OpenAngle      }
    '>'               { Dhall.Lexer.CloseAngle     }
    '<:>'             { Dhall.Lexer.EmptyPattern   }
    '['               { Dhall.Lexer.OpenBracket    }
    ']'               { Dhall.Lexer.CloseBracket   }
    ':'               { Dhall.Lexer.Colon          }
    ','               { Dhall.Lexer.Comma          }
    '.'               { Dhall.Lexer.Dot            }
    '='               { Dhall.Lexer.Equals         }
    '&&'              { Dhall.Lexer.And            }
    '||'              { Dhall.Lexer.Or             }
    '=='              { Dhall.Lexer.DoubleEquals   }
    '/='              { Dhall.Lexer.SlashEquals    }
    '+'               { Dhall.Lexer.Plus           }
    '<>'              { Dhall.Lexer.Diamond        }
    '++'              { Dhall.Lexer.DoublePlus     }
    '*'               { Dhall.Lexer.Star           }
    '@'               { Dhall.Lexer.At             }
    'let'             { Dhall.Lexer.Let            }
    'in'              { Dhall.Lexer.In             }
    'Type'            { Dhall.Lexer.Type           }
    'Kind'            { Dhall.Lexer.Kind           }
    '->'              { Dhall.Lexer.Arrow          }
    'forall'          { Dhall.Lexer.Forall         }
    '\\'              { Dhall.Lexer.Lambda         }
    'Bool'            { Dhall.Lexer.Bool           }
    'True'            { Dhall.Lexer.True_          }
    'False'           { Dhall.Lexer.False_         }
    'if'              { Dhall.Lexer.If             }
    'then'            { Dhall.Lexer.Then           }
    'else'            { Dhall.Lexer.Else           }
    'Natural'         { Dhall.Lexer.Natural        }
    'Natural/fold'    { Dhall.Lexer.NaturalFold    }
    'Natural/isZero'  { Dhall.Lexer.NaturalIsZero  }
    'Natural/even'    { Dhall.Lexer.NaturalEven    }
    'Natural/odd'     { Dhall.Lexer.NaturalOdd     }
    'Integer'         { Dhall.Lexer.Integer        }
    'Double'          { Dhall.Lexer.Double         }
    'Text'            { Dhall.Lexer.Text           }
    'Text/concat'     { Dhall.Lexer.TextConcat     }
    'List'            { Dhall.Lexer.List           }
    'List/build'      { Dhall.Lexer.ListBuild      }
    'List/fold'       { Dhall.Lexer.ListFold       }
    'List/length'     { Dhall.Lexer.ListLength     }
    'List/head'       { Dhall.Lexer.ListHead       }
    'List/last'       { Dhall.Lexer.ListLast       }
    'List/indexed'    { Dhall.Lexer.ListIndexed    }
    'List/reverse'    { Dhall.Lexer.ListReverse    }
    'List/concat'     { Dhall.Lexer.ListConcat     }
    'Maybe'           { Dhall.Lexer.Maybe          }
    'Maybe/fold'      { Dhall.Lexer.MaybeFold      }
    text              { Dhall.Lexer.TextLit    $$  }
    label             { Dhall.Lexer.Label      $$  }
    number            { Dhall.Lexer.Number     $$  }
    double            { Dhall.Lexer.DoubleLit  $$  }
    natural           { Dhall.Lexer.NaturalLit $$  }
    url               { Dhall.Lexer.URL        $$  }
    file              { Dhall.Lexer.File       $$  }

%right '=='
%right '/='
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
    | 'let' label Args '=' Expr0 'in' Expr1
        { Let $2 $3 Nothing $5 $7 }
    | 'let' label Args ':' Expr0 '=' Expr0 'in' Expr1
        { Let $2 $3 (Just $5) $7 $9 }
    | '[' Elems ']' ':' ListLike Expr6
        { $5 $6 (Data.Vector.fromList $2) }
    | Expr2
        { $1 }

ListLike
    : 'List'
        { ListLit }
    | 'Maybe'
        { MaybeLit }

Expr2
    : Expr2 '==' Expr2
        { BoolEQ $1 $3 }
    | Expr2 '/=' Expr2
        { BoolNE $1 $3 }
    | Expr3
        { $1 }

Expr3
    : Expr3 '||' Expr3
        { BoolOr $1 $3 }
    | Expr3 '+' Expr3
        { NaturalPlus $1 $3 }
    | Expr3 '<>' Expr3
        { TextAppend $1 $3 }
    | Expr3 '++' Expr3
        { ListAppend $1 $3 }
    | Expr4
        { $1 }

Expr4
    : Expr4 '&&' Expr4
        { BoolAnd $1 $3 }
    | Expr4 '*' Expr4
        { NaturalTimes $1 $3 }
    | Expr5
        { $1 }

Expr5
    : Expr5 Expr6
        { App $1 $2 }
    | Expr6
        { $1 }

Expr6
    : label
        { Var (V $1 0) }
    | label '@' number
        { Var (V $1 $3) }
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
    | 'Natural/isZero'
        { NaturalIsZero }
    | 'Natural/even'
        { NaturalEven }
    | 'Natural/odd'
        { NaturalOdd }
    | 'Integer'
        { Integer }
    | 'Double'
        { Double }
    | 'Text'
        { Text }
    | 'Text/concat'
        { TextConcat }
    | 'List'
        { List }
    | 'List/build'
        { ListBuild }
    | 'List/fold'
        { ListFold }
    | 'List/length'
        { ListLength }
    | 'List/head'
        { ListHead }
    | 'List/last'
        { ListLast }
    | 'List/indexed'
        { ListIndexed }
    | 'List/reverse'
        { ListReverse }
    | 'List/concat'
        { ListConcat }
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
    | Pattern
        { $1 }
    | PatternLit
        { $1 }
    | Import
        { Embed $1 }
    | Expr6 '.' label
        { Field $1 $3 }
    | '(' Expr0 ')'
        { $2 }
    
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
    : '{' FieldTypes '}'
        { Record (Data.Map.fromList $2) }
    | '{:}'
        { Record (Data.Map.fromList []) }

Pattern
    : '<' FieldTypes '>'
        { Pattern (Data.Map.fromList $2) }
    | '<:>'
        { Pattern (Data.Map.fromList []) }

PatternLit
    : '<' label '=' Expr0 ',' FieldTypes '>'
        { PatternLit $2 $4 (Data.Map.fromList $6) }

FieldTypes
    : FieldTypesRev
        { reverse $1 }

FieldTypesRev
    : FieldType
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
