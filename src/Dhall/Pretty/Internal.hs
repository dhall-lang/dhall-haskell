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
    , buildConst
    , buildVar
    , buildExpr
    , buildNatural
    , buildNumber
    , buildScientific
    , pretty
    , escapeText
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
import Data.Text.Buildable (Buildable(..))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Prettyprint.Doc (Doc, Pretty, space)
import Numeric.Natural (Natural)
import Prelude hiding (succ)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Terminal

import qualified Data.Char
import qualified Data.HashMap.Strict.InsOrd
import qualified Data.HashSet
import qualified Data.List
import qualified Data.Text.Lazy                        as Text
import qualified Data.Text.Lazy.Builder                as Builder
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Data.Vector

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
annToAnsiStyle Keyword  = Terminal.colorDull Terminal.Green
annToAnsiStyle Syntax   = Terminal.colorDull Terminal.Green
annToAnsiStyle Label    = mempty
annToAnsiStyle Literal  = Terminal.colorDull Terminal.Magenta
annToAnsiStyle Builtin  = Terminal.underlined
annToAnsiStyle Operator = Terminal.colorDull Terminal.Green

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

    hasNewLine builder = Text.any (== '\n') lazyText
      where
        lazyText = Builder.toLazyText builder

    prettyMultilineChunk (c, d) =
      prettyMultilineBuilder c <> dollar <> lbrace <> prettyExprA d <> rbrace

    prettyMultilineBuilder builder = literal (mconcat docs)
      where
        lazyText = Builder.toLazyText (escapeSingleQuotedText builder)

        lazyLines = Text.splitOn "\n" lazyText

        docs =
            Data.List.intersperse Pretty.hardline (fmap Pretty.pretty lazyLines)

    prettyChunk (c, d) = prettyText c <> syntax "${" <> prettyExprA d <> syntax rbrace

    prettyText t = literal (Pretty.pretty (Builder.toLazyText (escapeText t)))

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
    enclose' "" "      " (space <> keyword "else" <> space) (Pretty.hardline <> keyword "else" <> "  ") (fmap duplicate (docs a0))
  where
    docs (BoolIf a b c) =
        Pretty.group (Pretty.flatAlt long short) : docs c
      where
        long =
             Pretty.align
                (   (keyword "if" <> "    ")
                <>  prettyExprA a
                <>  Pretty.hardline
                <>  (keyword "then" <> "  ")
                <>  prettyExprA b
                )

        short = (keyword "if" <> " ")
            <>  prettyExprA a
            <>  (space <> keyword "then" <> space)
            <>  prettyExprA b
    docs (Note  _    c) = docs c
    docs             c  = [ prettyExprB c ]
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
    list (map prettyExprA (Data.Vector.toList b))
prettyExprB (ListLit (Just a) b) =
        list (map prettyExprA (Data.Vector.toList b))
    <>  " : "
    <>  prettyExprD (App List a)
prettyExprB (OptionalLit a b) =
        list (map prettyExprA (Data.Vector.toList b))
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
prettyExprC7 a0@(NaturalTimes _ _) =
    enclose' "" "  " (" " <> operator "*" <> " ") (operator "*" <> " ") (fmap duplicate (docs a0))
  where
    docs (NaturalTimes a b) = prettyExprC8 a : docs b
    docs (Note         _ b) = docs b
    docs                 b  = [ prettyExprC8 b ]
prettyExprC7 (Note _ a) =
    prettyExprC7 a
prettyExprC7 a0 =
    prettyExprC8 a0

prettyExprC8 :: Pretty a => Expr s a -> Doc Ann
prettyExprC8 a0@(BoolEQ _ _) =
    enclose' "" "    " (" " <> operator "==" <> " ") (operator "==" <> "  ") (fmap duplicate (docs a0))
  where
    docs (BoolEQ a b) = prettyExprC9 a : docs b
    docs (Note   _ b) = docs b
    docs           b  = [ prettyExprC9 b ]
prettyExprC8 (Note _ a) =
    prettyExprC8 a
prettyExprC8 a0 =
    prettyExprC9 a0

prettyExprC9 :: Pretty a => Expr s a -> Doc Ann
prettyExprC9 a0@(BoolNE _ _) =
    enclose' "" "    " (" " <> operator "!=" <> " ") (operator "!=" <> "  ") (fmap duplicate (docs a0))
  where
    docs (BoolNE a b) = prettyExprD a : docs b
    docs (Note   _ b) = docs b
    docs           b  = [ prettyExprD b ]
prettyExprC9 (Note _ a) =
    prettyExprC9 a
prettyExprC9 a0 =
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
prettyExprE (Field a b) = prettyExprE a <> dot <> prettyLabel b
prettyExprE (Note  _ b) = prettyExprE b
prettyExprE  a          = prettyExprF a

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
prettyExprF (IntegerLit a) =
    prettyNumber a
prettyExprF (NaturalLit a) =
    literal "+" <> prettyNatural a
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
    list (map prettyExprA (Data.Vector.toList b))
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
pretty = Pretty.renderLazy . Pretty.layoutPretty options . Pretty.pretty
  where
   options = Pretty.LayoutOptions { Pretty.layoutPageWidth = Pretty.Unbounded }

-- | Builder corresponding to the @label@ token in "Dhall.Parser"
buildLabel :: Text -> Builder
buildLabel l = case Text.uncons l of
    Just (h, t)
        | headCharacter h && Text.all tailCharacter t && not (Data.HashSet.member l reservedIdentifiers)
            -> build l
    _       -> "`" <> build l <> "`"


-- | Builder corresponding to the @number@ token in "Dhall.Parser"
buildNumber :: Integer -> Builder
buildNumber a = build (show a)

-- | Builder corresponding to the @natural@ token in "Dhall.Parser"
buildNatural :: Natural -> Builder
buildNatural a = build (show a)

-- | Builder corresponding to the @double@ token in "Dhall.Parser"
buildScientific :: Scientific -> Builder
buildScientific = build . show

-- | Builder corresponding to the @text@ token in "Dhall.Parser"
buildChunks :: Buildable a => Chunks s a -> Builder
buildChunks (Chunks a b) = "\"" <> foldMap buildChunk a <> escapeText b <> "\""
  where
    buildChunk (c, d) = escapeText c <> "${" <> buildExprA d <> "}"

-- | Escape a `Builder` literal using Dhall's escaping rules for single-quoted
--   @Text@
escapeSingleQuotedText :: Builder -> Builder
escapeSingleQuotedText inputBuilder = outputBuilder
  where
    inputText = Builder.toLazyText inputBuilder

    outputText = substitute "${" "''${" (substitute "''" "'''" inputText)

    outputBuilder = Builder.fromLazyText outputText

    substitute before after = Text.intercalate after . Text.splitOn before

{-| Escape a `Builder` literal using Dhall's escaping rules
  
    Note that the result does not include surrounding quotes
-}
escapeText :: Builder -> Builder
escapeText a = Builder.fromLazyText (Text.concatMap adapt text)
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

    text = Builder.toLazyText a

-- | Builder corresponding to the @expr@ parser in "Dhall.Parser"
buildExpr :: Buildable a => Expr s a -> Builder
buildExpr = buildExprA

-- | Builder corresponding to the @exprA@ parser in "Dhall.Parser"
buildExprA :: Buildable a => Expr s a -> Builder
buildExprA (Annot a b) = buildExprB a <> " : " <> buildExprA b
buildExprA (Note  _ b) = buildExprA b
buildExprA a           = buildExprB a

-- | Builder corresponding to the @exprB@ parser in "Dhall.Parser"
buildExprB :: Buildable a => Expr s a -> Builder
buildExprB (Lam a b c) =
        "λ("
    <>  buildLabel a
    <>  " : "
    <>  buildExprA b
    <>  ") → "
    <>  buildExprB c
buildExprB (BoolIf a b c) =
        "if "
    <>  buildExprA a
    <>  " then "
    <>  buildExprA b
    <>  " else "
    <>  buildExprA c
buildExprB (Pi "_" b c) =
        buildExprC b
    <>  " → "
    <>  buildExprB c
buildExprB (Pi a b c) =
        "∀("
    <>  buildLabel a
    <>  " : "
    <>  buildExprA b
    <>  ") → "
    <>  buildExprB c
buildExprB (Let a Nothing c d) =
        "let "
    <>  buildLabel a
    <>  " = "
    <>  buildExprA c
    <>  " in "
    <>  buildExprB d
buildExprB (Let a (Just b) c d) =
        "let "
    <>  buildLabel a
    <>  " : "
    <>  buildExprA b
    <>  " = "
    <>  buildExprA c
    <>  " in "
    <>  buildExprB d
buildExprB (ListLit Nothing b) =
    "[" <> buildElems (Data.Vector.toList b) <> "]"
buildExprB (ListLit (Just a) b) =
    "[" <> buildElems (Data.Vector.toList b) <> "] : List "  <> buildExprE a
buildExprB (OptionalLit a b) =
    "[" <> buildElems (Data.Vector.toList b) <> "] : Optional "  <> buildExprE a
buildExprB (Merge a b (Just c)) =
    "merge " <> buildExprE a <> " " <> buildExprE b <> " : " <> buildExprD c
buildExprB (Merge a b Nothing) =
    "merge " <> buildExprE a <> " " <> buildExprE b
buildExprB (Note _ b) =
    buildExprB b
buildExprB a =
    buildExprC a

-- | Builder corresponding to the @exprC@ parser in "Dhall.Parser"
buildExprC :: Buildable a => Expr s a -> Builder
buildExprC = buildExprC0

-- | Builder corresponding to the @exprC0@ parser in "Dhall.Parser"
buildExprC0 :: Buildable a => Expr s a -> Builder
buildExprC0 (BoolOr a b) = buildExprC1 a <> " || " <> buildExprC0 b
buildExprC0 (Note   _ b) = buildExprC0 b
buildExprC0  a           = buildExprC1 a

-- | Builder corresponding to the @exprC1@ parser in "Dhall.Parser"
buildExprC1 :: Buildable a => Expr s a -> Builder
buildExprC1 (TextAppend a b) = buildExprC2 a <> " ++ " <> buildExprC1 b
buildExprC1 (Note       _ b) = buildExprC1 b
buildExprC1  a               = buildExprC2 a

-- | Builder corresponding to the @exprC2@ parser in "Dhall.Parser"
buildExprC2 :: Buildable a => Expr s a -> Builder
buildExprC2 (NaturalPlus a b) = buildExprC3 a <> " + " <> buildExprC2 b
buildExprC2 (Note        _ b) = buildExprC2 b
buildExprC2  a                = buildExprC3 a

-- | Builder corresponding to the @exprC3@ parser in "Dhall.Parser"
buildExprC3 :: Buildable a => Expr s a -> Builder
buildExprC3 (ListAppend a b) = buildExprC4 a <> " # " <> buildExprC3 b
buildExprC3 (Note       _ b) = buildExprC3 b
buildExprC3  a               = buildExprC4 a

-- | Builder corresponding to the @exprC4@ parser in "Dhall.Parser"
buildExprC4 :: Buildable a => Expr s a -> Builder
buildExprC4 (BoolAnd a b) = buildExprC5 a <> " && " <> buildExprC4 b
buildExprC4 (Note    _ b) = buildExprC4 b
buildExprC4  a            = buildExprC5 a

-- | Builder corresponding to the @exprC5@ parser in "Dhall.Parser"
buildExprC5 :: Buildable a => Expr s a -> Builder
buildExprC5 (Combine   a b) = buildExprC6 a <> " ∧ " <> buildExprC5 b
buildExprC5 (Note      _ b) = buildExprC5 b
buildExprC5  a              = buildExprC6 a

-- | Builder corresponding to the @exprC6@ parser in "Dhall.Parser"
buildExprC6 :: Buildable a => Expr s a -> Builder
buildExprC6 (Prefer a b) = buildExprC7 a <> " ⫽ " <> buildExprC6 b
buildExprC6 (Note   _ b) = buildExprC6 b
buildExprC6  a           = buildExprC7 a

-- | Builder corresponding to the @exprC7@ parser in "Dhall.Parser"
buildExprC7 :: Buildable a => Expr s a -> Builder
buildExprC7 (NaturalTimes a b) = buildExprC8 a <> " * " <> buildExprC7 b
buildExprC7 (Note         _ b) = buildExprC7 b
buildExprC7  a                 = buildExprC8 a

-- | Builder corresponding to the @exprC8@ parser in "Dhall.Parser"
buildExprC8 :: Buildable a => Expr s a -> Builder
buildExprC8 (BoolEQ a b) = buildExprC9 a <> " == " <> buildExprC8 b
buildExprC8 (Note   _ b) = buildExprC8 b
buildExprC8  a           = buildExprC9 a

-- | Builder corresponding to the @exprC9@ parser in "Dhall.Parser"
buildExprC9 :: Buildable a => Expr s a -> Builder
buildExprC9 (BoolNE a b) = buildExprD  a <> " != " <> buildExprC9 b
buildExprC9 (Note   _ b) = buildExprC9 b
buildExprC9  a           = buildExprD  a

-- | Builder corresponding to the @exprD@ parser in "Dhall.Parser"
buildExprD :: Buildable a => Expr s a -> Builder
buildExprD (App        a b) = buildExprD a <> " " <> buildExprE b
buildExprD (Constructors b) = "constructors " <> buildExprE b
buildExprD (Note       _ b) = buildExprD b
buildExprD  a               = buildExprE a

-- | Builder corresponding to the @exprE@ parser in "Dhall.Parser"
buildExprE :: Buildable a => Expr s a -> Builder
buildExprE (Field a b) = buildExprE a <> "." <> buildLabel b
buildExprE (Note  _ b) = buildExprE b
buildExprE  a          = buildExprF a

-- | Builder corresponding to the @exprF@ parser in "Dhall.Parser"
buildExprF :: Buildable a => Expr s a -> Builder
buildExprF (Var a) =
    buildVar a
buildExprF (Const k) =
    buildConst k
buildExprF Bool =
    "Bool"
buildExprF Natural =
    "Natural"
buildExprF NaturalFold =
    "Natural/fold"
buildExprF NaturalBuild =
    "Natural/build"
buildExprF NaturalIsZero =
    "Natural/isZero"
buildExprF NaturalEven =
    "Natural/even"
buildExprF NaturalOdd =
    "Natural/odd"
buildExprF NaturalToInteger =
    "Natural/toInteger"
buildExprF NaturalShow =
    "Natural/show"
buildExprF Integer =
    "Integer"
buildExprF IntegerShow =
    "Integer/show"
buildExprF Double =
    "Double"
buildExprF DoubleShow =
    "Double/show"
buildExprF Text =
    "Text"
buildExprF List =
    "List"
buildExprF ListBuild =
    "List/build"
buildExprF ListFold =
    "List/fold"
buildExprF ListLength =
    "List/length"
buildExprF ListHead =
    "List/head"
buildExprF ListLast =
    "List/last"
buildExprF ListIndexed =
    "List/indexed"
buildExprF ListReverse =
    "List/reverse"
buildExprF Optional =
    "Optional"
buildExprF OptionalFold =
    "Optional/fold"
buildExprF OptionalBuild =
    "Optional/build"
buildExprF (BoolLit True) =
    "True"
buildExprF (BoolLit False) =
    "False"
buildExprF (IntegerLit a) =
    buildNumber a
buildExprF (NaturalLit a) =
    "+" <> buildNatural a
buildExprF (DoubleLit a) =
    buildScientific a
buildExprF (TextLit a) =
    buildChunks a
buildExprF (Record a) =
    buildRecord a
buildExprF (RecordLit a) =
    buildRecordLit a
buildExprF (Union a) =
    buildUnion a
buildExprF (UnionLit a b c) =
    buildUnionLit a b c
buildExprF (ListLit Nothing b) =
    "[" <> buildElems (Data.Vector.toList b) <> "]"
buildExprF (Embed a) =
    build a
buildExprF (Note _ b) =
    buildExprF b
buildExprF a =
    "(" <> buildExprA a <> ")"

-- | Builder corresponding to the @const@ parser in "Dhall.Parser"
buildConst :: Const -> Builder
buildConst Type = "Type"
buildConst Kind = "Kind"

-- | Builder corresponding to the @var@ parser in "Dhall.Parser"
buildVar :: Var -> Builder
buildVar (V x 0) = buildLabel x
buildVar (V x n) = buildLabel x <> "@" <> buildNumber n

-- | Builder corresponding to the @elems@ parser in "Dhall.Parser"
buildElems :: Buildable a => [Expr s a] -> Builder
buildElems   []   = ""
buildElems   [a]  = buildExprA a
buildElems (a:bs) = buildExprA a <> ", " <> buildElems bs

-- | Builder corresponding to the @recordLit@ parser in "Dhall.Parser"
buildRecordLit :: Buildable a => InsOrdHashMap Text (Expr s a) -> Builder
buildRecordLit a | Data.HashMap.Strict.InsOrd.null a =
    "{=}"
buildRecordLit a =
    "{ " <> buildFieldValues (Data.HashMap.Strict.InsOrd.toList a) <> " }"

-- | Builder corresponding to the @fieldValues@ parser in "Dhall.Parser"
buildFieldValues :: Buildable a => [(Text, Expr s a)] -> Builder
buildFieldValues    []  = ""
buildFieldValues   [a]  = buildFieldValue a
buildFieldValues (a:bs) = buildFieldValue a <> ", " <> buildFieldValues bs

-- | Builder corresponding to the @fieldValue@ parser in "Dhall.Parser"
buildFieldValue :: Buildable a => (Text, Expr s a) -> Builder
buildFieldValue (a, b) = buildLabel a <> " = " <> buildExprA b

-- | Builder corresponding to the @record@ parser in "Dhall.Parser"
buildRecord :: Buildable a => InsOrdHashMap Text (Expr s a) -> Builder
buildRecord a | Data.HashMap.Strict.InsOrd.null a =
    "{}"
buildRecord a =
    "{ " <> buildFieldTypes (Data.HashMap.Strict.InsOrd.toList a) <> " }"

-- | Builder corresponding to the @fieldTypes@ parser in "Dhall.Parser"
buildFieldTypes :: Buildable a => [(Text, Expr s a)] -> Builder
buildFieldTypes    []  = ""
buildFieldTypes   [a]  = buildFieldType a
buildFieldTypes (a:bs) = buildFieldType a <> ", " <> buildFieldTypes bs

-- | Builder corresponding to the @fieldType@ parser in "Dhall.Parser"
buildFieldType :: Buildable a => (Text, Expr s a) -> Builder
buildFieldType (a, b) = buildLabel a <> " : " <> buildExprA b

-- | Builder corresponding to the @union@ parser in "Dhall.Parser"
buildUnion :: Buildable a => InsOrdHashMap Text (Expr s a) -> Builder
buildUnion a | Data.HashMap.Strict.InsOrd.null a =
    "<>"
buildUnion a =
    "< " <> buildAlternativeTypes (Data.HashMap.Strict.InsOrd.toList a) <> " >"

-- | Builder corresponding to the @alternativeTypes@ parser in "Dhall.Parser"
buildAlternativeTypes :: Buildable a => [(Text, Expr s a)] -> Builder
buildAlternativeTypes [] =
    ""
buildAlternativeTypes [a] =
    buildAlternativeType a
buildAlternativeTypes (a:bs) =
    buildAlternativeType a <> " | " <> buildAlternativeTypes bs

-- | Builder corresponding to the @alternativeType@ parser in "Dhall.Parser"
buildAlternativeType :: Buildable a => (Text, Expr s a) -> Builder
buildAlternativeType (a, b) = buildLabel a <> " : " <> buildExprA b

-- | Builder corresponding to the @unionLit@ parser in "Dhall.Parser"
buildUnionLit
    :: Buildable a
    => Text -> Expr s a -> InsOrdHashMap Text (Expr s a) -> Builder
buildUnionLit a b c
    | Data.HashMap.Strict.InsOrd.null c =
            "< "
        <>  buildLabel a
        <>  " = "
        <>  buildExprA b
        <>  " >"
    | otherwise =
            "< "
        <>  buildLabel a
        <>  " = "
        <>  buildExprA b
        <>  " | "
        <>  buildAlternativeTypes (Data.HashMap.Strict.InsOrd.toList c)
        <>  " >"
