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
    '<'               { Dhall.Lexer.OpenAngle      }
    '>'               { Dhall.Lexer.CloseAngle     }
    '{=}'             { Dhall.Lexer.EmptyRecordLit }
    '['               { Dhall.Lexer.OpenBracket    }
    ']'               { Dhall.Lexer.CloseBracket   }
    ':'               { Dhall.Lexer.Colon          }
    ','               { Dhall.Lexer.Comma          }
    '|'               { Dhall.Lexer.Bar            }
    '.'               { Dhall.Lexer.Dot            }
    '='               { Dhall.Lexer.Equals         }
    '&&'              { Dhall.Lexer.And            }
    '∧'               { Dhall.Lexer.Merge          }
    '||'              { Dhall.Lexer.Or             }
    '=='              { Dhall.Lexer.DoubleEquals   }
    '/='              { Dhall.Lexer.SlashEquals    }
    '+'               { Dhall.Lexer.Plus           }
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
    'apply'           { Dhall.Lexer.Apply          }
    'if'              { Dhall.Lexer.If             }
    'then'            { Dhall.Lexer.Then           }
    'else'            { Dhall.Lexer.Else           }
    'Natural'         { Dhall.Lexer.Natural        }
    'Natural/fold'    { Dhall.Lexer.NaturalFold    }
    'Natural/build'   { Dhall.Lexer.NaturalBuild   }
    'Natural/isZero'  { Dhall.Lexer.NaturalIsZero  }
    'Natural/even'    { Dhall.Lexer.NaturalEven    }
    'Natural/odd'     { Dhall.Lexer.NaturalOdd     }
    'Integer'         { Dhall.Lexer.Integer        }
    'Double'          { Dhall.Lexer.Double         }
    'Text'            { Dhall.Lexer.Text           }
    'List'            { Dhall.Lexer.List           }
    'List/build'      { Dhall.Lexer.ListBuild      }
    'List/fold'       { Dhall.Lexer.ListFold       }
    'List/length'     { Dhall.Lexer.ListLength     }
    'List/head'       { Dhall.Lexer.ListHead       }
    'List/last'       { Dhall.Lexer.ListLast       }
    'List/indexed'    { Dhall.Lexer.ListIndexed    }
    'List/reverse'    { Dhall.Lexer.ListReverse    }
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
    | Expr2 '->' Expr1
        { Pi "_" $1 $3 }
    | 'forall' '(' label ':' Expr0 ')' '->' Expr1
        { Pi $3 $5 $8 }
    | 'let' label '=' Expr0 'in' Expr1
        { Let $2 Nothing $4 $6 }
    | 'let' label ':' Expr0 '=' Expr0 'in' Expr1
        { Let $2 (Just $4) $6 $8 }
    | '[' Elems ']' ':' ListLike Expr6
        { $5 $6 (Data.Vector.fromList $2) }
    | 'apply' Expr6 Expr6 ':' Expr5
        { Apply $2 $3 $5 }
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
    | Expr3 '++' Expr3
        { TextAppend $1 $3 }
    | Expr4
        { $1 }

Expr4
    : Expr4 '&&' Expr4
        { BoolAnd $1 $3 }
    | Expr4 '*' Expr4
        { NaturalTimes $1 $3 }
    | Expr4 '∧' Expr4
        { Merge $1 $3 }
    | Expr5
        { $1 }

Expr5
    : Expr5 Expr6
        { App $1 $2 }
    | Expr6
        { $1 }

Expr6
    : Var
        { Var $1 }
    | Const
        { Const $1 }
    | 'Bool'
        { Bool }
    | 'Natural'
        { Natural }
    | 'Natural/fold'
        { NaturalFold }
    | 'Natural/build'
        { NaturalBuild }
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
    | Record
        { $1 }
    | RecordLit
        { $1 }
    | Union
        { $1 }
    | UnionLit
        { $1 }
    | Import
        { Embed $1 }
    | Expr6 '.' label
        { Field $1 $3 }
    | '(' Expr0 ')'
        { $2 }

Const
    : 'Type'
        { Type }
    | 'Kind'
        { Kind }

Var
    : label
        { V $1 0 }
    | label '@' number
        { V $1 $3 }
    
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
    : '{=}'
        { RecordLit (Data.Map.fromList []) }
    | '{' FieldValues '}'
        { RecordLit (Data.Map.fromList $2) }

FieldValues
    : FieldValuesRev
        { reverse $1 }

FieldValuesRev
    : FieldValue
        { [$1] }
    | FieldValuesRev ',' FieldValue
        { $3 : $1 }

FieldValue
    : label '=' Expr0 
        { ($1, $3) }

Record
    : '{' FieldTypes '}'
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

Union
    : '<' AlternativeTypes '>'
        { Union (Data.Map.fromList $2) }

AlternativeTypes
    : AlternativeTypesRev
        { reverse $1 }

AlternativeTypesRev
    : {- empty -}
        { [] }
    | AlternativeType
        { [$1] }
    | AlternativeTypesRev '|' AlternativeType
        { $3 : $1 }

AlternativeType
    : label ':' Expr0
        { ($1, $3) } 

UnionLit
    : '<' label '=' Expr0 '>'
        { UnionLit $2 $4 Data.Map.empty }
    | '<' label '=' Expr0 '|' AlternativeTypes '>'
        { UnionLit $2 $4 (Data.Map.fromList $6) }

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

{-| Parse an expression from a `ByteString` containing a UTF8-encoded Dhall
    program
-}
exprFromBytes :: ByteString -> Either ParseError (Expr Path)
exprFromBytes bytes = case Dhall.Lexer.runAlex bytes expr of
    Left  str -> Left (ParseError (Data.Text.Lazy.pack str))
    Right e   -> Right e
}
