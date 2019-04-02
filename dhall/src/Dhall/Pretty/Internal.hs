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

    , CharacterSet(..)
    , prettyCharacterSet

    , prettyVar
    , pretty
    , escapeText

    , prettyConst
    , prettyLabel
    , prettyAnyLabel
    , prettyLabels
    , prettyNatural
    , prettyNumber
    , prettyDouble
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

import Dhall.Core

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative (Applicative(..), (<$>))
#endif
import Data.Foldable
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty, space)
import Dhall.Map (Map)
import Dhall.Set (Set)
import Numeric.Natural (Natural)
import Prelude hiding (succ)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Terminal

import qualified Data.Char
import qualified Data.HashSet
import qualified Data.List
import qualified Data.Set
import qualified Data.Text                               as Text
import qualified Data.Text.Prettyprint.Doc               as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text   as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.String as Pretty
import qualified Dhall.Map
import qualified Dhall.Set

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

data CharacterSet = ASCII | Unicode

-- | Pretty print an expression
prettyExpr :: Pretty a => Expr s a -> Doc Ann
prettyExpr = prettyCharacterSet Unicode

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

comma, lbracket, rbracket, langle, rangle, lbrace, rbrace, lparen, rparen, pipe, backtick, dollar, colon, equals, dot :: Doc Ann
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
backtick = syntax "`"
dollar   = syntax "$"
colon    = syntax ":"
equals   = syntax "="
dot      = syntax "."

lambda :: CharacterSet -> Doc Ann
lambda Unicode = syntax "λ"
lambda ASCII   = syntax "\\"

forall :: CharacterSet -> Doc Ann
forall Unicode = syntax "∀"
forall ASCII   = syntax "forall "

rarrow :: CharacterSet -> Doc Ann
rarrow Unicode = syntax "→"
rarrow ASCII   = syntax "->"

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
arrows :: CharacterSet -> [(Doc Ann, Doc Ann)] -> Doc Ann
arrows ASCII =
    enclose'
        ""
        "    "
        (" " <> rarrow ASCII <> " ")
        (rarrow ASCII <> "  ")
arrows Unicode =
    enclose'
        ""
        "  "
        (" " <> rarrow Unicode <> " ")
        (rarrow Unicode <> " ")

combine :: CharacterSet -> Text
combine ASCII   = "/\\"
combine Unicode = "∧"

combineTypes :: CharacterSet -> Text
combineTypes ASCII   = "//\\\\"
combineTypes Unicode = "⩓"

prefer :: CharacterSet -> Text
prefer ASCII   = "//"
prefer Unicode = "⫽"

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

prettyLabelShared :: Bool -> Text -> Doc Ann
prettyLabelShared allowReserved a = label doc
    where
        doc =
            case Text.uncons a of
                Just (h, t)
                    | headCharacter h && Text.all tailCharacter t && (allowReserved || not (Data.HashSet.member a reservedIdentifiers))
                        -> Pretty.pretty a
                _       -> backtick <> Pretty.pretty a <> backtick

prettyLabel :: Text -> Doc Ann
prettyLabel = prettyLabelShared False

prettyAnyLabel :: Text -> Doc Ann
prettyAnyLabel = prettyLabelShared True

prettyLabels :: Set Text -> Doc Ann
prettyLabels a
    | Data.Set.null (Dhall.Set.toSet a) =
        lbrace <> rbrace
    | otherwise =
        braces (map (duplicate . prettyAnyLabel) (Dhall.Set.toList a))

prettyNumber :: Integer -> Doc Ann
prettyNumber = literal . Pretty.pretty

prettyNatural :: Natural -> Doc Ann
prettyNatural = literal . Pretty.pretty

prettyDouble :: Double -> Doc Ann
prettyDouble = literal . Pretty.pretty

prettyConst :: Const -> Doc Ann
prettyConst Type = builtin "Type"
prettyConst Kind = builtin "Kind"
prettyConst Sort = builtin "Sort"

prettyVar :: Var -> Doc Ann
prettyVar (V x 0) = label (Pretty.unAnnotate (prettyLabel x))
prettyVar (V x n) = label (Pretty.unAnnotate (prettyLabel x <> "@" <> prettyNumber n))

prettyCharacterSet :: Pretty a => CharacterSet -> Expr s a -> Doc Ann
prettyCharacterSet characterSet = prettyExpression
  where
    prettyExpression a0@(Lam _ _ _) =
        arrows characterSet (fmap duplicate (docs a0))
      where
        docs (Lam a b c) = Pretty.group (Pretty.flatAlt long short) : docs c
          where
            long =  (lambda characterSet <> space)
                <>  Pretty.align
                    (   (lparen <> space)
                    <>  prettyLabel a
                    <>  Pretty.hardline
                    <>  (colon <> space)
                    <>  prettyExpression b
                    <>  Pretty.hardline
                    <>  rparen
                    )

            short = (lambda characterSet <> lparen)
                <>  prettyLabel a
                <>  (space <> colon <> space)
                <>  prettyExpression b
                <>  rparen
        docs (Note  _ c) = docs c
        docs          c  = [ prettyExpression c ]
    prettyExpression a0@(BoolIf _ _ _) =
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
                [   keyword "if" <> " " <> prettyExpression a
                ,   prettyExpression b
                ]
        docsLong (Note  _    c) = docsLong c
        docsLong             c  = [ prettyExpression c ]

        docsShort (BoolIf a b c) =
            docShort ++ docsShort c
          where
            docShort =
                [   keyword "if" <> " " <> prettyExpression a
                ,   prettyExpression b
                ]
        docsShort (Note  _    c) = docsShort c
        docsShort             c  = [ prettyExpression c ]
    prettyExpression (Let as b) =
        enclose' "" "" space Pretty.hardline
            (fmap duplicate (fmap docA (toList as)) ++ [ docB ])
      where
        docA (Binding c Nothing e) =
            Pretty.group (Pretty.flatAlt long short)
          where
            long =  keyword "let" <> space
                <>  Pretty.align
                    (   prettyLabel c
                    <>  space <> equals
                    <>  Pretty.hardline
                    <>  "  "
                    <>  prettyExpression e
                    )

            short = keyword "let" <> space
                <>  prettyLabel c
                <>  (space <> equals <> space)
                <>  prettyExpression e
        docA (Binding c (Just d) e) =
            Pretty.group (Pretty.flatAlt long short)
          where
            long = keyword "let" <> space
                <>  Pretty.align
                    (   prettyLabel c
                    <>  Pretty.hardline
                    <>  colon <> space
                    <>  prettyExpression d
                    <>  Pretty.hardline
                    <>  equals <> space
                    <>  prettyExpression e
                    )

            short = keyword "let" <> space
                <>  prettyLabel c
                <>  space <> colon <> space
                <>  prettyExpression d
                <>  space <> equals <> space
                <>  prettyExpression e

        docB =
            ( keyword "in" <> " " <> prettyExpression b
            , keyword "in" <> "  "  <> prettyExpression b
            )
    prettyExpression a0@(Pi _ _ _) =
        arrows characterSet (fmap duplicate (docs a0))
      where
        docs (Pi "_" b c) = prettyOperatorExpression b : docs c
        docs (Pi a   b c) = Pretty.group (Pretty.flatAlt long short) : docs c
          where
            long =  forall characterSet <> space
                <>  Pretty.align
                    (   lparen <> space
                    <>  prettyLabel a
                    <>  Pretty.hardline
                    <>  colon <> space
                    <>  prettyExpression b
                    <>  Pretty.hardline
                    <>  rparen
                    )

            short = forall characterSet <> lparen
                <>  prettyLabel a
                <>  space <> colon <> space
                <>  prettyExpression b
                <>  rparen
        docs (Note _   c) = docs c
        docs           c  = [ prettyExpression c ]
    prettyExpression (Note _ a) =
        prettyExpression a
    prettyExpression a0 =
        prettyAnnotatedExpression a0

    prettyAnnotatedExpression :: Pretty a => Expr s a -> Doc Ann
    prettyAnnotatedExpression (Merge a b (Just c)) =
        Pretty.group (Pretty.flatAlt long short)
      where
        long =
            Pretty.align
                (   keyword "merge"
                <>  Pretty.hardline
                <>  prettyImportExpression a
                <>  Pretty.hardline
                <>  prettyImportExpression b
                <>  Pretty.hardline
                <>  colon <> space
                <>  prettyApplicationExpression c
                )

        short = keyword "merge" <> space
            <>  prettyImportExpression a
            <>  " "
            <>  prettyImportExpression b
            <>  space <> colon <> space
            <>  prettyApplicationExpression c
    prettyAnnotatedExpression (Merge a b Nothing) =
        Pretty.group (Pretty.flatAlt long short)
      where
        long =
            Pretty.align
                (   keyword "merge"
                <>  Pretty.hardline
                <>  prettyImportExpression a
                <>  Pretty.hardline
                <>  prettyImportExpression b
                )

        short = keyword "merge" <> space
            <>  prettyImportExpression a
            <>  " "
            <>  prettyImportExpression b
    prettyAnnotatedExpression a0@(Annot _ _) =
        enclose'
            ""
            "  "
            (" " <> colon <> " ")
            (colon <> space)
            (fmap duplicate (docs a0))
      where
        docs (Annot a b) = prettyOperatorExpression a : docs b
        docs (Note  _ b) = docs b
        docs          b  = [ prettyExpression b ]
    prettyAnnotatedExpression (ListLit (Just a) b) =
            list (map prettyExpression (Data.Foldable.toList b))
        <>  " : "
        <>  prettyApplicationExpression (App List a)
    prettyAnnotatedExpression (OptionalLit a b) =
            list (map prettyExpression (Data.Foldable.toList b))
        <>  " : "
        <>  prettyApplicationExpression (App Optional a)
    prettyAnnotatedExpression (Note _ a) =
        prettyAnnotatedExpression a
    prettyAnnotatedExpression a0 =
        prettyOperatorExpression a0

    prettyOperatorExpression :: Pretty a => Expr s a -> Doc Ann
    prettyOperatorExpression = prettyImportAltExpression

    prettyOperator :: Text -> [Doc Ann] -> Doc Ann
    prettyOperator op docs =
        enclose'
            ""
            prefix
            (" " <> operator (Pretty.pretty op) <> " ")
            (operator (Pretty.pretty op) <> spacer)
            (reverse (fmap duplicate docs))
      where
        prefix = if Text.length op == 1 then "  " else "    "

        spacer = if Text.length op == 1 then " "  else "  "

    prettyImportAltExpression :: Pretty a => Expr s a -> Doc Ann
    prettyImportAltExpression a0@(ImportAlt _ _) =
        prettyOperator "?" (docs a0)
      where
        docs (ImportAlt a b) = prettyOrExpression b : docs a
        docs (Note      _ b) = docs b
        docs              b  = [ prettyOrExpression b ]
    prettyImportAltExpression (Note _ a) =
        prettyImportAltExpression a
    prettyImportAltExpression a0 =
        prettyOrExpression a0

    prettyOrExpression :: Pretty a => Expr s a -> Doc Ann
    prettyOrExpression a0@(BoolOr _ _) =
        prettyOperator "||" (docs a0)
      where
        docs (BoolOr a b) = prettyPlusExpression b : docs a
        docs (Note   _ b) = docs b
        docs           b  = [ prettyPlusExpression b ]
    prettyOrExpression (Note _ a) =
        prettyOrExpression a
    prettyOrExpression a0 =
        prettyPlusExpression a0

    prettyPlusExpression :: Pretty a => Expr s a -> Doc Ann
    prettyPlusExpression a0@(NaturalPlus _ _) =
        prettyOperator "+" (docs a0)
      where
        docs (NaturalPlus a b) = prettyTextAppendExpression b : docs a
        docs (Note        _ b) = docs b
        docs                b  = [ prettyTextAppendExpression b ]
    prettyPlusExpression (Note _ a) =
        prettyPlusExpression a
    prettyPlusExpression a0 =
        prettyTextAppendExpression a0

    prettyTextAppendExpression :: Pretty a => Expr s a -> Doc Ann
    prettyTextAppendExpression a0@(TextAppend _ _) =
        prettyOperator "++" (docs a0)
      where
        docs (TextAppend a b) = prettyListAppendExpression b : docs a
        docs (Note       _ b) = docs b
        docs               b  = [ prettyListAppendExpression b ]
    prettyTextAppendExpression (Note _ a) =
        prettyTextAppendExpression a
    prettyTextAppendExpression a0 =
        prettyListAppendExpression a0

    prettyListAppendExpression :: Pretty a => Expr s a -> Doc Ann
    prettyListAppendExpression a0@(ListAppend _ _) =
        prettyOperator "#" (docs a0)
      where
        docs (ListAppend a b) = prettyAndExpression b : docs a
        docs (Note       _ b) = docs b
        docs               b  = [ prettyAndExpression b ]
    prettyListAppendExpression (Note _ a) =
        prettyListAppendExpression a
    prettyListAppendExpression a0 =
        prettyAndExpression a0

    prettyAndExpression :: Pretty a => Expr s a -> Doc Ann
    prettyAndExpression a0@(BoolAnd _ _) =
        prettyOperator "&&" (docs a0)
      where
        docs (BoolAnd a b) = prettyCombineExpression b : docs a
        docs (Note    _ b) = docs b
        docs            b  = [ prettyCombineExpression b ]
    prettyAndExpression (Note _ a) =
        prettyAndExpression a
    prettyAndExpression a0 =
       prettyCombineExpression a0

    prettyCombineExpression :: Pretty a => Expr s a -> Doc Ann
    prettyCombineExpression a0@(Combine _ _) =
        prettyOperator (combine characterSet) (docs a0)
      where
        docs (Combine a b) = prettyPreferExpression b : docs a
        docs (Note    _ b) = docs b
        docs            b  = [ prettyPreferExpression b ]
    prettyCombineExpression (Note _ a) =
        prettyCombineExpression a
    prettyCombineExpression a0 =
        prettyPreferExpression a0

    prettyPreferExpression :: Pretty a => Expr s a -> Doc Ann
    prettyPreferExpression a0@(Prefer _ _) =
        prettyOperator (prefer characterSet) (docs a0)
      where
        docs (Prefer a b) = prettyCombineTypesExpression b : docs a
        docs (Note   _ b) = docs b
        docs           b  = [ prettyCombineTypesExpression b ]
    prettyPreferExpression (Note _ a) =
        prettyPreferExpression a
    prettyPreferExpression a0 =
        prettyCombineTypesExpression a0

    prettyCombineTypesExpression :: Pretty a => Expr s a -> Doc Ann
    prettyCombineTypesExpression a0@(CombineTypes _ _) =
        prettyOperator (combineTypes characterSet) (docs a0)
      where
        docs (CombineTypes a b) = prettyTimesExpression b : docs a
        docs (Note         _ b) = docs b
        docs                 b  = [ prettyTimesExpression b ]
    prettyCombineTypesExpression (Note _ a) =
        prettyCombineTypesExpression a
    prettyCombineTypesExpression a0 =
        prettyTimesExpression a0

    prettyTimesExpression :: Pretty a => Expr s a -> Doc Ann
    prettyTimesExpression a0@(NaturalTimes _ _) =
        prettyOperator "*" (docs a0)
      where
        docs (NaturalTimes a b) = prettyEqualExpression b : docs a
        docs (Note         _ b) = docs b
        docs                 b  = [ prettyEqualExpression b ]
    prettyTimesExpression (Note _ a) =
        prettyTimesExpression a
    prettyTimesExpression a0 =
        prettyEqualExpression a0

    prettyEqualExpression :: Pretty a => Expr s a -> Doc Ann
    prettyEqualExpression a0@(BoolEQ _ _) =
        prettyOperator "==" (docs a0)
      where
        docs (BoolEQ a b) = prettyNotEqualExpression b : docs a
        docs (Note   _ b) = docs b
        docs           b  = [ prettyNotEqualExpression b ]
    prettyEqualExpression (Note _ a) =
        prettyEqualExpression a
    prettyEqualExpression a0 =
        prettyNotEqualExpression a0

    prettyNotEqualExpression :: Pretty a => Expr s a -> Doc Ann
    prettyNotEqualExpression a0@(BoolNE _ _) =
        prettyOperator "!=" (docs a0)
      where
        docs (BoolNE a b) = prettyApplicationExpression b : docs a
        docs (Note   _ b) = docs b
        docs           b  = [ prettyApplicationExpression b ]
    prettyNotEqualExpression (Note _ a) =
        prettyNotEqualExpression a
    prettyNotEqualExpression a0 =
        prettyApplicationExpression a0

    prettyApplicationExpression :: Pretty a => Expr s a -> Doc Ann
    prettyApplicationExpression a0 = case a0 of
        App _ _  -> result
        Some _   -> result
        Note _ b -> prettyApplicationExpression b
        _        -> prettyImportExpression a0
      where
        result = enclose' "" "" " " "" (fmap duplicate (reverse (docs a0)))

        docs (App  a b) = prettyImportExpression b : docs a
        docs (Some   a) = [ prettyImportExpression a , builtin "Some"         ]
        docs (Note _ b) = docs b
        docs         b  = [ prettyImportExpression b ]

    prettyImportExpression :: Pretty a => Expr s a -> Doc Ann
    prettyImportExpression (Embed a) =
        Pretty.pretty a
    prettyImportExpression (Note _ a) =
        prettyImportExpression a
    prettyImportExpression a0 =
        prettySelectorExpression a0

    prettySelectorExpression :: Pretty a => Expr s a -> Doc Ann
    prettySelectorExpression (Field a b) =
        prettySelectorExpression a <> dot <> prettyAnyLabel b
    prettySelectorExpression (Project a b) =
        prettySelectorExpression a <> dot <> prettyLabels b
    prettySelectorExpression (Note _ b) =
        prettySelectorExpression b
    prettySelectorExpression a0 =
        prettyPrimitiveExpression a0

    prettyPrimitiveExpression :: Pretty a => Expr s a -> Doc Ann
    prettyPrimitiveExpression (Var a) =
        prettyVar a
    prettyPrimitiveExpression (Const k) =
        prettyConst k
    prettyPrimitiveExpression Bool =
        builtin "Bool"
    prettyPrimitiveExpression Natural =
        builtin "Natural"
    prettyPrimitiveExpression NaturalFold =
        builtin "Natural/fold"
    prettyPrimitiveExpression NaturalBuild =
        builtin "Natural/build"
    prettyPrimitiveExpression NaturalIsZero =
        builtin "Natural/isZero"
    prettyPrimitiveExpression NaturalEven =
        builtin "Natural/even"
    prettyPrimitiveExpression NaturalOdd =
        builtin "Natural/odd"
    prettyPrimitiveExpression NaturalToInteger =
        builtin "Natural/toInteger"
    prettyPrimitiveExpression NaturalShow =
        builtin "Natural/show"
    prettyPrimitiveExpression Integer =
        builtin "Integer"
    prettyPrimitiveExpression IntegerShow =
        builtin "Integer/show"
    prettyPrimitiveExpression IntegerToDouble =
        builtin "Integer/toDouble"
    prettyPrimitiveExpression Double =
        builtin "Double"
    prettyPrimitiveExpression DoubleShow =
        builtin "Double/show"
    prettyPrimitiveExpression Text =
        builtin "Text"
    prettyPrimitiveExpression TextShow =
        builtin "Text/show"
    prettyPrimitiveExpression List =
        builtin "List"
    prettyPrimitiveExpression ListBuild =
        builtin "List/build"
    prettyPrimitiveExpression ListFold =
        builtin "List/fold"
    prettyPrimitiveExpression ListLength =
        builtin "List/length"
    prettyPrimitiveExpression ListHead =
        builtin "List/head"
    prettyPrimitiveExpression ListLast =
        builtin "List/last"
    prettyPrimitiveExpression ListIndexed =
        builtin "List/indexed"
    prettyPrimitiveExpression ListReverse =
        builtin "List/reverse"
    prettyPrimitiveExpression Optional =
        builtin "Optional"
    prettyPrimitiveExpression None =
        builtin "None"
    prettyPrimitiveExpression OptionalFold =
        builtin "Optional/fold"
    prettyPrimitiveExpression OptionalBuild =
        builtin "Optional/build"
    prettyPrimitiveExpression (BoolLit True) =
        builtin "True"
    prettyPrimitiveExpression (BoolLit False) =
        builtin "False"
    prettyPrimitiveExpression (IntegerLit a)
        | 0 <= a    = literal "+" <> prettyNumber a
        | otherwise = prettyNumber a
    prettyPrimitiveExpression (NaturalLit a) =
        prettyNatural a
    prettyPrimitiveExpression (DoubleLit a) =
        prettyDouble a
    prettyPrimitiveExpression (TextLit a) =
        prettyChunks a
    prettyPrimitiveExpression (Record a) =
        prettyRecord a
    prettyPrimitiveExpression (RecordLit a) =
        prettyRecordLit a
    prettyPrimitiveExpression (Union a) =
        prettyUnion a
    prettyPrimitiveExpression (UnionLit a b c) =
        prettyUnionLit a b c
    prettyPrimitiveExpression (ListLit Nothing b) =
        list (map prettyExpression (Data.Foldable.toList b))
    prettyPrimitiveExpression (Note _ b) =
        prettyPrimitiveExpression b
    prettyPrimitiveExpression a =
        Pretty.group (Pretty.flatAlt long short)
      where
        long =
            Pretty.align
                (lparen <> space <> prettyExpression a <> Pretty.hardline <> rparen)

        short = lparen <> prettyExpression a <> rparen

    prettyKeyValue :: Pretty a => Doc Ann -> (Text, Expr s a) -> (Doc Ann, Doc Ann)
    prettyKeyValue separator (key, val) =
        (       prettyAnyLabel key
            <>  " "
            <>  separator
            <>  " "
            <>  prettyExpression val
        ,       prettyAnyLabel key
            <>  " "
            <>  separator
            <>  long
        )
      where
        long = Pretty.hardline <> "    " <> prettyExpression val

    prettyRecord :: Pretty a => Map Text (Expr s a) -> Doc Ann
    prettyRecord =
        braces . map (prettyKeyValue colon) . Dhall.Map.toList

    prettyRecordLit :: Pretty a => Map Text (Expr s a) -> Doc Ann
    prettyRecordLit a
        | Data.Foldable.null a =
            lbrace <> equals <> rbrace
        | otherwise
            = braces (map (prettyKeyValue equals) (Dhall.Map.toList a))

    prettyAlternative (key, Just val) = prettyKeyValue colon (key, val)
    prettyAlternative (key, Nothing ) = duplicate (prettyAnyLabel key)

    prettyUnion :: Pretty a => Map Text (Maybe (Expr s a)) -> Doc Ann
    prettyUnion =
        angles . map prettyAlternative . Dhall.Map.toList

    prettyUnionLit
        :: Pretty a
        => Text -> Expr s a -> Map Text (Maybe (Expr s a)) -> Doc Ann
    prettyUnionLit a b c =
        angles (front : map prettyAlternative (Dhall.Map.toList c))
      where
        front = prettyKeyValue equals (a, b)

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
                prettyMultilineBuilder c
            <>  dollar
            <>  lbrace
            <>  prettyExpression d
            <>  rbrace

        prettyMultilineBuilder builder = literal (mconcat docs)
          where
            lazyLines = Text.splitOn "\n" (escapeSingleQuotedText builder)

            docs =
                Data.List.intersperse Pretty.hardline (fmap Pretty.pretty lazyLines)

        prettyChunk (c, d) =
                prettyText c
            <>  syntax "${"
            <>  prettyExpression d
            <>  syntax rbrace

        prettyText t = literal (Pretty.pretty (escapeText t))

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
        | '\x20' <= c && c <= '\x21'     = Text.singleton c
        -- '\x22' == '"'
        | '\x23' == c                    = Text.singleton c
        -- '\x24' == '$'
        | '\x25' <= c && c <= '\x5B'     = Text.singleton c
        -- '\x5C' == '\\'
        | '\x5D' <= c && c <= '\x10FFFF' = Text.singleton c
        | c == '"'                       = "\\\""
        | c == '$'                       = "\\$"
        | c == '\\'                      = "\\\\"
        | c == '\b'                      = "\\b"
        | c == '\f'                      = "\\f"
        | c == '\n'                      = "\\n"
        | c == '\r'                      = "\\r"
        | c == '\t'                      = "\\t"
        | otherwise                      = "\\u" <> showDigits (Data.Char.ord c)

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
