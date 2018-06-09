{-# LANGUAGE OverloadedStrings #-}
-- | Scanners for reserved terms, operators, etc.
module Dhall.Parser.Reserved where

import qualified Data.Text
import qualified Text.Parser.Char
import Dhall.Parser.Combinators
import Text.Parser.Combinators ((<?>))
import Control.Monad (void)
import Control.Applicative ((<|>))

import Dhall.Parser.Token

reserved :: Data.Text.Text -> Parser ()
reserved x = do _ <- Text.Parser.Char.text x; whitespace

_if :: Parser ()
_if = reserved "if"

_then :: Parser ()
_then = reserved "then"

_else :: Parser ()
_else = reserved "else"

_let :: Parser ()
_let = reserved "let"

_in :: Parser ()
_in = reserved "in"

_as :: Parser ()
_as = reserved "as"

_using :: Parser ()
_using = reserved "using"

_merge :: Parser ()
_merge = reserved "merge"

_constructors :: Parser ()
_constructors = reserved "constructors"

_NaturalFold :: Parser ()
_NaturalFold = reserved "Natural/fold"

_NaturalBuild :: Parser ()
_NaturalBuild = reserved "Natural/build"

_NaturalIsZero :: Parser ()
_NaturalIsZero = reserved "Natural/isZero"

_NaturalEven :: Parser ()
_NaturalEven = reserved "Natural/even"

_NaturalOdd :: Parser ()
_NaturalOdd = reserved "Natural/odd"

_NaturalToInteger :: Parser ()
_NaturalToInteger = reserved "Natural/toInteger"

_NaturalShow :: Parser ()
_NaturalShow = reserved "Natural/show"

_IntegerShow :: Parser ()
_IntegerShow = reserved "Integer/show"

_DoubleShow :: Parser ()
_DoubleShow = reserved "Double/show"

_ListBuild :: Parser ()
_ListBuild = reserved "List/build"

_ListFold :: Parser ()
_ListFold = reserved "List/fold"

_ListLength :: Parser ()
_ListLength = reserved "List/length"

_ListHead :: Parser ()
_ListHead = reserved "List/head"

_ListLast :: Parser ()
_ListLast = reserved "List/last"

_ListIndexed :: Parser ()
_ListIndexed = reserved "List/indexed"

_ListReverse :: Parser ()
_ListReverse = reserved "List/reverse"

_OptionalFold :: Parser ()
_OptionalFold = reserved "Optional/fold"

_OptionalBuild :: Parser ()
_OptionalBuild = reserved "Optional/build"

_Bool :: Parser ()
_Bool = reserved "Bool"

_Optional :: Parser ()
_Optional = reserved "Optional"

_Natural :: Parser ()
_Natural = reserved "Natural"

_Integer :: Parser ()
_Integer = reserved "Integer"

_Double :: Parser ()
_Double = reserved "Double"

_Text :: Parser ()
_Text = reserved "Text"

_List :: Parser ()
_List = reserved "List"

_True :: Parser ()
_True = reserved "True"

_False :: Parser ()
_False = reserved "False"

_Type :: Parser ()
_Type = reserved "Type"

_Kind :: Parser ()
_Kind = reserved "Kind"

_equal :: Parser ()
_equal = reserved "="

_or :: Parser ()
_or = reserved "||"

_plus :: Parser ()
_plus = reserved "+"

_textAppend :: Parser ()
_textAppend = reserved "++"

_listAppend :: Parser ()
_listAppend = reserved "#"

_and :: Parser ()
_and = reserved "&&"

_times :: Parser ()
_times = reserved "*"

_doubleEqual :: Parser ()
_doubleEqual = reserved "=="

_notEqual :: Parser ()
_notEqual = reserved "!="

_dot :: Parser ()
_dot = reserved "."

_openBrace :: Parser ()
_openBrace = reserved "{"

_closeBrace :: Parser ()
_closeBrace = reserved "}"

_openBracket :: Parser ()
_openBracket = reserved "["

_closeBracket :: Parser ()
_closeBracket = reserved "]"

_openAngle :: Parser ()
_openAngle = reserved "<"

_closeAngle :: Parser ()
_closeAngle = reserved ">"

_bar :: Parser ()
_bar = reserved "|"

_comma :: Parser ()
_comma = reserved ","

_openParens :: Parser ()
_openParens = reserved "("

_closeParens :: Parser ()
_closeParens = reserved ")"

_colon :: Parser ()
_colon = reserved ":"

_at :: Parser ()
_at = reserved "@"

_combine :: Parser ()
_combine = do
    void (Text.Parser.Char.char '∧' <?> "\"∧\"") <|> void (Text.Parser.Char.text "/\\")
    whitespace

_combineTypes :: Parser ()
_combineTypes = do
    void (Text.Parser.Char.char '⩓' <?> "\"⩓\"") <|> void (Text.Parser.Char.text "//\\\\")
    whitespace

_prefer :: Parser ()
_prefer = do
    void (Text.Parser.Char.char '⫽' <?> "\"⫽\"") <|> void (Text.Parser.Char.text "//")
    whitespace

_lambda :: Parser ()
_lambda = do
    _ <- Text.Parser.Char.satisfy predicate
    whitespace
  where
    predicate 'λ'  = True
    predicate '\\' = True
    predicate _    = False

_forall :: Parser ()
_forall = do
    void (Text.Parser.Char.char '∀' <?> "\"∀\"") <|> void (Text.Parser.Char.text "forall")
    whitespace

_arrow :: Parser ()
_arrow = do
    void (Text.Parser.Char.char '→' <?> "\"→\"") <|> void (Text.Parser.Char.text "->")
    whitespace

