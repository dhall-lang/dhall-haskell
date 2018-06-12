{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

{-| This module provides internal pretty-printing utilities which are used by
    other modules but are not part of the public facing API
-}

module Dhall.Pretty.Internal (
      Ann(..)
    , annToAnsiStyle
    , prettyExpr
    , prettyVar
    , pretty
    , escapeText

    , prettyConst
    , prettyLabel
    , prettyLabels
    , prettyNatural
    , prettyNumber
    , prettyScientific
    , prettyToStrictText
    , prettyToString

    , docToStrictText

    , builtin
    , keyword
    , literal
    , operator

    , colon
    , comma
    , dot
    , equals
    , forall
    , label
    , lambda
    , langle
    , lbrace
    , lbracket
    , lparen
    , pipe
    , rangle
    , rarrow
    , rbrace
    , rbracket
    , rparen
    ) where

import {-# SOURCE #-} Dhall.Core

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative (Applicative(..), (<$>))
#endif
import Data.Foldable
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Monoid ((<>))
import Data.Scientific (Scientific)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty, space)
import Numeric.Natural (Natural)
import Prelude hiding (succ)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Terminal

import qualified Data.Char
import qualified Data.HashMap.Strict.InsOrd
import qualified Data.HashSet
import qualified Data.List
import qualified Data.Set
import qualified Data.Text                               as Text
import qualified Data.Text.Prettyprint.Doc               as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text   as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.String as Pretty

{-| Annotation type used to tag elements in a pretty-printed document for
    syntax highlighting purposes
-}
data Ann
  = Keyword     -- ^ Used for syntactic keywords
  | Syntax      -- ^ Syntax punctuation such as commas, parenthesis, and braces
  | Label       -- ^ Record labels
  | Literal     -- ^ Literals such as integers and strings
  | Builtin     -- ^ Builtin types and values
  | Operator    -- ^ Operators

{-| Convert annotations to their corresponding color for syntax highlighting
    purposes
-}
annToAnsiStyle :: Ann -> Terminal.AnsiStyle
annToAnsiStyle Keyword  = Terminal.bold <> Terminal.colorDull Terminal.Green
annToAnsiStyle Syntax   = Terminal.bold <> Terminal.colorDull Terminal.Green
annToAnsiStyle Label    = mempty
annToAnsiStyle Literal  = Terminal.colorDull Terminal.Magenta
annToAnsiStyle Builtin  = Terminal.underlined
annToAnsiStyle Operator = Terminal.bold <> Terminal.colorDull Terminal.Green

-- | Pretty print an expression
prettyExpr :: Pretty a => Expr s a -> Doc Ann
prettyExpr = prettyExprA

{-| Internal utility for pretty-printing, used when generating element lists
    to supply to `enclose` or `enclose'`.  This utility indicates that the
    compact represent is the same as the multi-line representation for each
    element
-}
duplicate :: a -> (a, a)
duplicate x = (x, x)

-- Annotation helpers
keyword, syntax, label, literal, builtin, operator :: Doc Ann -> Doc Ann
keyword  = Pretty.annotate Keyword
syntax   = Pretty.annotate Syntax
label    = Pretty.annotate Label
literal  = Pretty.annotate Literal
builtin  = Pretty.annotate Builtin
operator = Pretty.annotate Operator

comma, lbracket, rbracket, langle, rangle, lbrace, rbrace, lparen, rparen, pipe, rarrow, backtick, dollar, colon, lambda, forall, equals, dot :: Doc Ann
comma    = syntax Pretty.comma
lbracket = syntax Pretty.lbracket
rbracket = syntax Pretty.rbracket
langle   = syntax Pretty.langle
rangle   = syntax Pretty.rangle
lbrace   = syntax Pretty.lbrace
rbrace   = syntax Pretty.rbrace
lparen   = syntax Pretty.lparen
rparen   = syntax Pretty.rparen
pipe     = syntax Pretty.pipe
rarrow   = syntax "→"
backtick = syntax "`"
dollar   = syntax "$"
colon    = syntax ":"
lambda   = syntax "λ"
forall   = syntax "∀"
equals   = syntax "="
dot      = syntax "."

-- | Pretty-print a list
list :: [Doc Ann] -> Doc Ann
list   [] = lbracket <> rbracket
list docs =
    enclose
        (lbracket <> space)
        (lbracket <> space)
        (comma <> space)
        (comma <> space)
        (space <> rbracket)
        rbracket
        (fmap duplicate docs)

-- | Pretty-print union types and literals
angles :: [(Doc Ann, Doc Ann)] -> Doc Ann
angles   [] = langle <> rangle
angles docs =
    enclose
        (langle <> space)
        (langle <> space)
        (space <> pipe <> space)
        (pipe <> space)
        (space <> rangle)
        rangle
        docs

-- | Pretty-print record types and literals
braces :: [(Doc Ann, Doc Ann)] -> Doc Ann
braces   [] = lbrace <> rbrace
braces docs =
    enclose
        (lbrace <> space)
        (lbrace <> space)
        (comma <> space)
        (comma <> space)
        (space <> rbrace)
        rbrace
        docs

-- | Pretty-print anonymous functions and function types
arrows :: [(Doc Ann, Doc Ann)] -> Doc Ann
arrows =
    enclose'
        ""
        "  "
        (" " <> rarrow <> " ")
        (rarrow <> space)

{-| Format an expression that holds a variable number of elements, such as a
    list, record, or union
-}
enclose
    :: Doc ann
    -- ^ Beginning document for compact representation
    -> Doc ann
    -- ^ Beginning document for multi-line representation
    -> Doc ann
    -- ^ Separator for compact representation
    -> Doc ann
    -- ^ Separator for multi-line representation
    -> Doc ann
    -- ^ Ending document for compact representation
    -> Doc ann
    -- ^ Ending document for multi-line representation
    -> [(Doc ann, Doc ann)]
    -- ^ Elements to format, each of which is a pair: @(compact, multi-line)@
    -> Doc ann
enclose beginShort _         _        _       endShort _       []   =
    beginShort <> endShort
  where
enclose beginShort beginLong sepShort sepLong endShort endLong docs =
    Pretty.group
        (Pretty.flatAlt
            (Pretty.align
                (mconcat (zipWith combineLong (beginLong : repeat sepLong) docsLong) <> endLong)
            )
            (mconcat (zipWith combineShort (beginShort : repeat sepShort) docsShort) <> endShort)
        )
  where
    docsShort = fmap fst docs

    docsLong = fmap snd docs

    combineLong x y = x <> y <> Pretty.hardline

    combineShort x y = x <> y

{-| Format an expression that holds a variable number of elements without a
    trailing document such as nested `let`, nested lambdas, or nested `forall`s
-}
enclose'
    :: Doc ann
    -- ^ Beginning document for compact representation
    -> Doc ann
    -- ^ Beginning document for multi-line representation
    -> Doc ann
    -- ^ Separator for compact representation
    -> Doc ann
    -- ^ Separator for multi-line representation
    -> [(Doc ann, Doc ann)]
    -- ^ Elements to format, each of which is a pair: @(compact, multi-line)@
    -> Doc ann
enclose' beginShort beginLong sepShort sepLong docs =
    Pretty.group (Pretty.flatAlt long short)
  where
    longLines = zipWith (<>) (beginLong : repeat sepLong) docsLong

    long =
        Pretty.align (mconcat (Data.List.intersperse Pretty.hardline longLines))

    short = mconcat (zipWith (<>) (beginShort : repeat sepShort) docsShort)

    docsShort = fmap fst docs

    docsLong = fmap snd docs

alpha :: Char -> Bool
alpha c = ('\x41' <= c && c <= '\x5A') || ('\x61' <= c && c <= '\x7A')

digit :: Char -> Bool
digit c = '\x30' <= c && c <= '\x39'

headCharacter :: Char -> Bool
headCharacter c = alpha c || c == '_'

tailCharacter :: Char -> Bool
tailCharacter c = alpha c || digit c || c == '_' || c == '-' || c == '/'

prettyLabel :: Text -> Doc Ann
prettyLabel a = label doc
    where
        doc =
            case Text.uncons a of
                Just (h, t)
                    | headCharacter h && Text.all tailCharacter t && not (Data.HashSet.member a reservedIdentifiers)
                        -> Pretty.pretty a
                _       -> backtick <> Pretty.pretty a <> backtick

prettyLabels :: Set Text -> Doc Ann
prettyLabels a
    | Data.Set.null a =
        lbrace <> rbrace
    | otherwise =
        braces (map (duplicate . prettyLabel) (Data.Set.toList a))

prettyNumber :: Integer -> Doc Ann
prettyNumber = literal . Pretty.pretty

prettyNatural :: Natural -> Doc Ann
prettyNatural = literal . Pretty.pretty

prettyScientific :: Scientific -> Doc Ann
prettyScientific = literal . Pretty.pretty . show

prettyChunks :: Pretty a => Chunks s a -> Doc Ann
prettyChunks (Chunks a b) =
    if any (\(builder, _) -> hasNewLine builder) a || hasNewLine b
    then Pretty.flatAlt long short
    else short
  where
    long =
        Pretty.align
        (   literal ("''" <> Pretty.hardline)
        <>  Pretty.align
            (foldMap prettyMultilineChunk a <> prettyMultilineBuilder b)
        <>  literal "''"
        )

    short =
        literal "\"" <> foldMap prettyChunk a <> literal (prettyText b <> "\"")

    hasNewLine = Text.any (== '\n')

    prettyMultilineChunk (c, d) =
      prettyMultilineBuilder c <> dollar <> lbrace <> prettyExprA d <> rbrace

    prettyMultilineBuilder builder = literal (mconcat docs)
      where
        lazyLines = Text.splitOn "\n" (escapeSingleQuotedText builder)

        docs =
            Data.List.intersperse Pretty.hardline (fmap Pretty.pretty lazyLines)

    prettyChunk (c, d) = prettyText c <> syntax "${" <> prettyExprA d <> syntax rbrace

    prettyText t = literal (Pretty.pretty (escapeText t))

prettyConst :: Const -> Doc Ann
prettyConst Type = builtin "Type"
prettyConst Kind = builtin "Kind"

prettyVar :: Var -> Doc Ann
prettyVar (V x 0) = label (Pretty.unAnnotate (prettyLabel x))
prettyVar (V x n) = label (Pretty.unAnnotate (prettyLabel x <> "@" <> prettyNumber n))

prettyExprA :: Pretty a => Expr s a -> Doc Ann
prettyExprA a0@(Annot _ _) =
    enclose'
        ""
        "  "
        (" " <> colon <> " ")
        (colon <> space)
        (fmap duplicate (docs a0))
  where
    docs (Annot a b) = prettyExprB a : docs b
    docs (Note  _ b) = docs b
    docs          b  = [ prettyExprB b ]
prettyExprA (Note _ a) =
    prettyExprA a
prettyExprA a0 =
    prettyExprB a0

prettyExprB :: Pretty a => Expr s a -> Doc Ann
prettyExprB a0@(Lam _ _ _) = arrows (fmap duplicate (docs a0))
  where
    docs (Lam a b c) = Pretty.group (Pretty.flatAlt long short) : docs c
      where
        long =  (lambda <> space)
            <>  Pretty.align
                (   (lparen <> space)
                <>  prettyLabel a
                <>  Pretty.hardline
                <>  (colon <> space)
                <>  prettyExprA b
                <>  Pretty.hardline
                <>  rparen
                )

        short = (lambda <> lparen)
            <>  prettyLabel a
            <>  (space <> colon <> space)
            <>  prettyExprA b
            <>  rparen
    docs (Note  _ c) = docs c
    docs          c  = [ prettyExprB c ]
prettyExprB a0@(BoolIf _ _ _) =
    Pretty.group (Pretty.flatAlt long short)
  where
    prefixesLong =
            "      "
        :   cycle
                [ Pretty.hardline <> keyword "then" <> "  "
                , Pretty.hardline <> keyword "else" <> "  "
                ]

    prefixesShort =
            ""
        :   cycle
                [ space <> keyword "then" <> space
                , space <> keyword "else" <> space
                ]

    longLines = zipWith (<>) prefixesLong (docsLong a0)

    long =
        Pretty.align (mconcat (Data.List.intersperse Pretty.hardline longLines))

    short = mconcat (zipWith (<>) prefixesShort (docsShort a0))

    docsLong (BoolIf a b c) =
        docLong ++ docsLong c
      where
        docLong =
            [   keyword "if" <> " " <> prettyExprA a
            ,   prettyExprA b
            ]
    docsLong (Note  _    c) = docsLong c
    docsLong             c  = [ prettyExprB c ]

    docsShort (BoolIf a b c) =
        docShort ++ docsShort c
      where
        docShort =
            [   keyword "if" <> " " <> prettyExprA a
            ,   prettyExprA b
            ]
    docsShort (Note  _    c) = docsShort c
    docsShort             c  = [ prettyExprB c ]

prettyExprB a0@(Pi _ _ _) =
    arrows (fmap duplicate (docs a0))
  where
    docs (Pi "_" b c) = prettyExprC b : docs c
    docs (Pi a   b c) = Pretty.group (Pretty.flatAlt long short) : docs c
      where
        long =  forall <> space
            <>  Pretty.align
                (   lparen <> space
                <>  prettyLabel a
                <>  Pretty.hardline
                <>  colon <> space
                <>  prettyExprA b
                <>  Pretty.hardline
                <>  rparen
                )

        short = forall <> lparen
            <>  prettyLabel a
            <>  space <> colon <> space
            <>  prettyExprA b
            <>  rparen
    docs (Note _   c) = docs c
    docs           c  = [ prettyExprB c ]
prettyExprB a0@(Let _ _ _ _) =
    enclose' "" "    " (space <> keyword "in" <> space) (Pretty.hardline <> keyword "in" <> "  ")
        (fmap duplicate (docs a0))
  where
    docs (Let a Nothing c d) =
        Pretty.group (Pretty.flatAlt long short) : docs d
      where
        long =  keyword "let" <> space
            <>  Pretty.align
                (   prettyLabel a
                <>  space <> equals
                <>  Pretty.hardline
                <>  "  "
                <>  prettyExprA c
                )

        short = keyword "let" <> space
            <>  prettyLabel a
            <>  (space <> equals <> space)
            <>  prettyExprA c
    docs (Let a (Just b) c d) =
        Pretty.group (Pretty.flatAlt long short) : docs d
      where
        long = keyword "let" <> space
            <>  Pretty.align
                (   prettyLabel a
                <>  Pretty.hardline
                <>  colon <> space
                <>  prettyExprA b
                <>  Pretty.hardline
                <>  equals <> space
                <>  prettyExprA c
                )

        short = keyword "let" <> space
            <>  prettyLabel a
            <>  space <> colon <> space
            <>  prettyExprA b
            <>  space <> equals <> space
            <>  prettyExprA c
    docs (Note _ d)  =
        docs d
    docs d =
        [ prettyExprB d ]
prettyExprB (ListLit Nothing b) =
    list (map prettyExprA (Data.Foldable.toList b))
prettyExprB (ListLit (Just a) b) =
        list (map prettyExprA (Data.Foldable.toList b))
    <>  " : "
    <>  prettyExprD (App List a)
prettyExprB (OptionalLit a b) =
        list (map prettyExprA (Data.Foldable.toList b))
    <>  " : "
    <>  prettyExprD (App Optional a)
prettyExprB (Merge a b (Just c)) =
    Pretty.group (Pretty.flatAlt long short)
  where
    long =
        Pretty.align
            (   keyword "merge"
            <>  Pretty.hardline
            <>  prettyExprE a
            <>  Pretty.hardline
            <>  prettyExprE b
            <>  Pretty.hardline
            <>  colon <> space
            <>  prettyExprD c
            )

    short = keyword "merge" <> space
        <>  prettyExprE a
        <>  " "
        <>  prettyExprE b
        <>  space <> colon <> space
        <>  prettyExprD c
prettyExprB (Merge a b Nothing) =
    Pretty.group (Pretty.flatAlt long short)
  where
    long =
        Pretty.align
            (   keyword "merge"
            <>  Pretty.hardline
            <>  prettyExprE a
            <>  Pretty.hardline
            <>  prettyExprE b
            )

    short = keyword "merge" <> space
        <>  prettyExprE a
        <>  " "
        <>  prettyExprE b
prettyExprB (Note _ b) =
    prettyExprB b
prettyExprB a =
    prettyExprC a

prettyExprC :: Pretty a => Expr s a -> Doc Ann
prettyExprC = prettyExprC0

prettyExprC0 :: Pretty a => Expr s a -> Doc Ann
prettyExprC0 a0@(BoolOr _ _) =
    enclose' "" "    " (space <> operator "||" <> space) (operator "||" <> "  ") (fmap duplicate (docs a0))
  where
    docs (BoolOr a b) = prettyExprC1 a : docs b
    docs (Note   _ b) = docs b
    docs           b  = [ prettyExprC1 b ]
prettyExprC0 (Note _ a) =
    prettyExprC0 a
prettyExprC0 a0 =
    prettyExprC1 a0

prettyExprC1 :: Pretty a => Expr s a -> Doc Ann
prettyExprC1 a0@(TextAppend _ _) =
    enclose' "" "    " (" " <> operator "++" <> " ") (operator "++" <> "  ") (fmap duplicate (docs a0))
  where
    docs (TextAppend a b) = prettyExprC2 a : docs b
    docs (Note       _ b) = docs b
    docs               b  = [ prettyExprC2 b ]
prettyExprC1 (Note _ a) =
    prettyExprC1 a
prettyExprC1 a0 =
    prettyExprC2 a0

prettyExprC2 :: Pretty a => Expr s a -> Doc Ann
prettyExprC2 a0@(NaturalPlus _ _) =
    enclose' "" "  " (" " <> operator "+" <> " ") (operator "+" <> " ") (fmap duplicate (docs a0))
  where
    docs (NaturalPlus a b) = prettyExprC3 a : docs b
    docs (Note        _ b) = docs b
    docs                b  = [ prettyExprC3 b ]
prettyExprC2 (Note _ a) =
    prettyExprC2 a
prettyExprC2 a0 =
    prettyExprC3 a0

prettyExprC3 :: Pretty a => Expr s a -> Doc Ann
prettyExprC3 a0@(ListAppend _ _) =
    enclose' "" "  " (" " <> operator "#" <> " ") (operator "#" <> " ") (fmap duplicate (docs a0))
  where
    docs (ListAppend a b) = prettyExprC4 a : docs b
    docs (Note       _ b) = docs b
    docs               b  = [ prettyExprC4 b ]
prettyExprC3 (Note _ a) =
    prettyExprC3 a
prettyExprC3 a0 =
    prettyExprC4 a0

prettyExprC4 :: Pretty a => Expr s a -> Doc Ann
prettyExprC4 a0@(BoolAnd _ _) =
    enclose' "" "    " (" " <> operator "&&" <> " ") (operator "&&" <> "  ") (fmap duplicate (docs a0))
  where
    docs (BoolAnd a b) = prettyExprC5 a : docs b
    docs (Note    _ b) = docs b
    docs            b  = [ prettyExprC5 b ]
prettyExprC4 (Note _ a) =
    prettyExprC4 a
prettyExprC4 a0 =
   prettyExprC5 a0

prettyExprC5 :: Pretty a => Expr s a -> Doc Ann
prettyExprC5 a0@(Combine _ _) =
    enclose' "" "  " (" " <> operator "∧" <> " ") (operator "∧" <> " ") (fmap duplicate (docs a0))
  where
    docs (Combine a b) = prettyExprC6 a : docs b
    docs (Note    _ b) = docs b
    docs            b  = [ prettyExprC6 b ]
prettyExprC5 (Note _ a) =
    prettyExprC5 a
prettyExprC5 a0 =
    prettyExprC6 a0

prettyExprC6 :: Pretty a => Expr s a -> Doc Ann
prettyExprC6 a0@(Prefer _ _) =
    enclose' "" "  " (" " <> operator "⫽" <> " ") (operator "⫽" <> " ") (fmap duplicate (docs a0))
  where
    docs (Prefer a b) = prettyExprC7 a : docs b
    docs (Note   _ b) = docs b
    docs           b  = [ prettyExprC7 b ]
prettyExprC6 (Note _ a) =
    prettyExprC6 a
prettyExprC6 a0 =
    prettyExprC7 a0

prettyExprC7 :: Pretty a => Expr s a -> Doc Ann
prettyExprC7 a0@(CombineTypes _ _) =
    enclose' "" "  " (" " <> operator "⩓" <> " ") (operator "⩓" <> " ") (fmap duplicate (docs a0))
  where
    docs (CombineTypes a b) = prettyExprC8 a : docs b
    docs (Note         _ b) = docs b
    docs                 b  = [ prettyExprC8 b ]
prettyExprC7 (Note _ a) =
    prettyExprC7 a
prettyExprC7 a0 =
    prettyExprC8 a0

prettyExprC8 :: Pretty a => Expr s a -> Doc Ann
prettyExprC8 a0@(NaturalTimes _ _) =
    enclose' "" "  " (" " <> operator "*" <> " ") (operator "*" <> " ") (fmap duplicate (docs a0))
  where
    docs (NaturalTimes a b) = prettyExprC9 a : docs b
    docs (Note         _ b) = docs b
    docs                 b  = [ prettyExprC9 b ]
prettyExprC8 (Note _ a) =
    prettyExprC8 a
prettyExprC8 a0 =
    prettyExprC9 a0

prettyExprC9 :: Pretty a => Expr s a -> Doc Ann
prettyExprC9 a0@(BoolEQ _ _) =
    enclose' "" "    " (" " <> operator "==" <> " ") (operator "==" <> "  ") (fmap duplicate (docs a0))
  where
    docs (BoolEQ a b) = prettyExprC10 a : docs b
    docs (Note   _ b) = docs b
    docs           b  = [ prettyExprC10 b ]
prettyExprC9 (Note _ a) =
    prettyExprC9 a
prettyExprC9 a0 =
    prettyExprC10 a0

prettyExprC10 :: Pretty a => Expr s a -> Doc Ann
prettyExprC10 a0@(BoolNE _ _) =
    enclose' "" "    " (" " <> operator "!=" <> " ") (operator "!=" <> "  ") (fmap duplicate (docs a0))
  where
    docs (BoolNE a b) = prettyExprD a : docs b
    docs (Note   _ b) = docs b
    docs           b  = [ prettyExprD b ]
prettyExprC10 (Note _ a) =
    prettyExprC10 a
prettyExprC10 a0 =
    prettyExprD a0

prettyExprD :: Pretty a => Expr s a -> Doc Ann
prettyExprD a0 = case a0 of
    App _ _        -> result
    Constructors _ -> result
    Note _ b       -> prettyExprD b
    _              -> prettyExprE a0
  where
    result = enclose' "" "" " " "" (fmap duplicate (reverse (docs a0)))

    docs (App        a b) = prettyExprE b : docs a
    docs (Constructors b) = [ prettyExprE b , keyword "constructors" ]
    docs (Note       _ b) = docs b
    docs               b  = [ prettyExprE b ]

prettyExprE :: Pretty a => Expr s a -> Doc Ann
prettyExprE (Field   a b) = prettyExprE a <> dot <> prettyLabel b
prettyExprE (Project a b) = prettyExprE a <> dot <> prettyLabels b
prettyExprE (Note    _ b) = prettyExprE b
prettyExprE  a            = prettyExprF a

prettyExprF :: Pretty a => Expr s a -> Doc Ann
prettyExprF (Var a) =
    prettyVar a
prettyExprF (Const k) =
    prettyConst k
prettyExprF Bool =
    builtin "Bool"
prettyExprF Natural =
    builtin "Natural"
prettyExprF NaturalFold =
    builtin "Natural/fold"
prettyExprF NaturalBuild =
    builtin "Natural/build"
prettyExprF NaturalIsZero =
    builtin "Natural/isZero"
prettyExprF NaturalEven =
    builtin "Natural/even"
prettyExprF NaturalOdd =
    builtin "Natural/odd"
prettyExprF NaturalToInteger =
    builtin "Natural/toInteger"
prettyExprF NaturalShow =
    builtin "Natural/show"
prettyExprF Integer =
    builtin "Integer"
prettyExprF IntegerShow =
    builtin "Integer/show"
prettyExprF Double =
    builtin "Double"
prettyExprF DoubleShow =
    builtin "Double/show"
prettyExprF Text =
    builtin "Text"
prettyExprF List =
    builtin "List"
prettyExprF ListBuild =
    builtin "List/build"
prettyExprF ListFold =
    builtin "List/fold"
prettyExprF ListLength =
    builtin "List/length"
prettyExprF ListHead =
    builtin "List/head"
prettyExprF ListLast =
    builtin "List/last"
prettyExprF ListIndexed =
    builtin "List/indexed"
prettyExprF ListReverse =
    builtin "List/reverse"
prettyExprF Optional =
    builtin "Optional"
prettyExprF OptionalFold =
    builtin "Optional/fold"
prettyExprF OptionalBuild =
    builtin "Optional/build"
prettyExprF (BoolLit True) =
    builtin "True"
prettyExprF (BoolLit False) =
    builtin "False"
prettyExprF (IntegerLit a)
    | 0 <= a    = literal "+" <> prettyNumber a
    | otherwise = prettyNumber a
prettyExprF (NaturalLit a) =
    prettyNatural a
prettyExprF (DoubleLit a) =
    prettyScientific a
prettyExprF (TextLit a) =
    prettyChunks a
prettyExprF (Record a) =
    prettyRecord a
prettyExprF (RecordLit a) =
    prettyRecordLit a
prettyExprF (Union a) =
    prettyUnion a
prettyExprF (UnionLit a b c) =
    prettyUnionLit a b c
prettyExprF (ListLit Nothing b) =
    list (map prettyExprA (Data.Foldable.toList b))
prettyExprF (Embed a) =
    Pretty.pretty a
prettyExprF (Note _ b) =
    prettyExprF b
prettyExprF a =
    Pretty.group (Pretty.flatAlt long short)
  where
    long = Pretty.align (lparen <> space <> prettyExprA a <> Pretty.hardline <> rparen)

    short = lparen <> prettyExprA a <> rparen

prettyKeyValue :: Pretty a => Doc Ann -> (Text, Expr s a) -> (Doc Ann, Doc Ann)
prettyKeyValue separator (key, value) =
    (   prettyLabel key <> " " <> separator <> " " <> prettyExprA value
    ,       prettyLabel key
        <>  " "
        <>  separator
        <>  long
    )
  where
    long = Pretty.hardline <> "    " <> prettyExprA value

prettyRecord :: Pretty a => InsOrdHashMap Text (Expr s a) -> Doc Ann
prettyRecord =
    braces . map (prettyKeyValue colon) . Data.HashMap.Strict.InsOrd.toList

prettyRecordLit :: Pretty a => InsOrdHashMap Text (Expr s a) -> Doc Ann
prettyRecordLit a
    | Data.HashMap.Strict.InsOrd.null a =
        lbrace <> equals <> rbrace
    | otherwise
        = braces (map (prettyKeyValue equals) (Data.HashMap.Strict.InsOrd.toList a))

prettyUnion :: Pretty a => InsOrdHashMap Text (Expr s a) -> Doc Ann
prettyUnion =
    angles . map (prettyKeyValue colon) . Data.HashMap.Strict.InsOrd.toList

prettyUnionLit
    :: Pretty a => Text -> Expr s a -> InsOrdHashMap Text (Expr s a) -> Doc Ann
prettyUnionLit a b c =
    angles (front : map adapt (Data.HashMap.Strict.InsOrd.toList c))
  where
    front = prettyKeyValue equals (a, b)

    adapt = prettyKeyValue colon

-- | Pretty-print a value
pretty :: Pretty a => a -> Text
pretty = Pretty.renderStrict . Pretty.layoutPretty options . Pretty.pretty
  where
   options = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.Unbounded }

-- | Escape a `Text` literal using Dhall's escaping rules for single-quoted
--   @Text@
escapeSingleQuotedText :: Text -> Text
escapeSingleQuotedText inputBuilder = outputBuilder
  where
    outputText = substitute "${" "''${" (substitute "''" "'''" inputBuilder)

    outputBuilder = outputText

    substitute before after = Text.intercalate after . Text.splitOn before

{-| Escape a `Text` literal using Dhall's escaping rules

    Note that the result does not include surrounding quotes
-}
escapeText :: Text -> Text
escapeText text = Text.concatMap adapt text
  where
    adapt c
        | '\x20' <= c && c <= '\x21' = Text.singleton c
        -- '\x22' == '"'
        | '\x23' == c                = Text.singleton c
        -- '\x24' == '$'
        | '\x25' <= c && c <= '\x5B' = Text.singleton c
        -- '\x5C' == '\\'
        | '\x5D' <= c && c <= '\x7F' = Text.singleton c
        | c == '"'                   = "\\\""
        | c == '$'                   = "\\$"
        | c == '\\'                  = "\\\\"
        | c == '\b'                  = "\\b"
        | c == '\f'                  = "\\f"
        | c == '\n'                  = "\\n"
        | c == '\r'                  = "\\r"
        | c == '\t'                  = "\\t"
        | otherwise                  = "\\u" <> showDigits (Data.Char.ord c)

    showDigits r0 = Text.pack (map showDigit [q1, q2, q3, r3])
      where
        (q1, r1) = r0 `quotRem` 4096
        (q2, r2) = r1 `quotRem`  256
        (q3, r3) = r2 `quotRem`   16

    showDigit n
        | n < 10    = Data.Char.chr (Data.Char.ord '0' + n)
        | otherwise = Data.Char.chr (Data.Char.ord 'A' + n - 10)

prettyToString :: Pretty a => a -> String
prettyToString =
    Pretty.renderString . Pretty.layoutPretty options . Pretty.pretty
  where
   options = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.Unbounded }

docToStrictText :: Doc ann -> Text.Text
docToStrictText = Pretty.renderStrict . Pretty.layoutPretty options
  where
   options = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.Unbounded }

prettyToStrictText :: Pretty a => a -> Text.Text
prettyToStrictText = docToStrictText . Pretty.pretty
