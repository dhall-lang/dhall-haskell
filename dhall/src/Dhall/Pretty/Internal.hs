{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE BangPatterns        #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

{-| This module provides internal pretty-printing utilities which are used by
    other modules but are not part of the public facing API
-}

module Dhall.Pretty.Internal (
      Ann(..)
    , AnnotPrinting(..)
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
    , prettyInt
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
import {-# SOURCE #-} Dhall.Parser.Token (reservedIdentifiers)

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

data CharacterSet = ASCII | Unicode  deriving Show

-- | Toggle showing or hiding type annotations which come from elaboration.
--   For non-normalized but elaborated code, the original source annotations
--   are still availabel, so it makes sense to turn off extra annotation printing
--   with 'PrintSrcAnnot', which only prints user annotations.
--
--   However, normalization eliminates user annotations, so it makes sense
--   to print normal expressions with 'PrintAllAnnot', in order to make them
--   unambiguous and re-checkable.
data AnnotPrinting
  -- | Print all annotations.
  = PrintAllAnnot
  -- | Only print annotations given in the source.
  | PrintSrcAnnot
  deriving Show

annotPrinting :: AnnotSource -> AnnotPrinting -> Bool
annotPrinting _ PrintAllAnnot = True
annotPrinting SourceAnnot _   = True
annotPrinting _           _   = False

-- | Pretty print an expression
prettyExpr :: Pretty a => AnnotPrinting -> Named (Expr a) -> Doc Ann
prettyExpr = prettyCharacterSet Unicode

{-| Internal utility for pretty-printing, used when generating element lists
    to supply to `enclose` or `enclose'`. This utility indicates that the
    compact representation is the same as the multi-line representation for each
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

prettyInt :: Int -> Doc Ann
prettyInt = literal . Pretty.pretty

prettyNatural :: Natural -> Doc Ann
prettyNatural = literal . Pretty.pretty

prettyDouble :: Double -> Doc Ann
prettyDouble = literal . Pretty.pretty

prettyConst :: Const -> Doc Ann
prettyConst Type = builtin "Type"
prettyConst Kind = builtin "Kind"
prettyConst Sort = builtin "Sort"

prettyVar :: Names -> Int -> Doc Ann
prettyVar ns i =
  let !x = indexNames ns i
      go !acc _             0  = acc
      go  acc (NBind _ x')  i' = go (if x == x' then acc + 1 else acc) ns (i' - 1)
      go  _   NEmpty        _  = error $ internalError "prettyVar.go: impossible out of bounds index"
  in case go 0 ns i of
    0 -> label (Pretty.unAnnotate (prettyLabel x))
    i' -> label (Pretty.unAnnotate (prettyLabel x <> "@" <> prettyInt i'))

prettyCharacterSet :: forall a. Pretty a => CharacterSet -> AnnotPrinting -> Named (Expr a) -> Doc Ann
prettyCharacterSet characterSet printingOpt (Named ns' expression) =
    Pretty.group (prettyExpression ns' expression)
  where
    prettyExpression :: Names -> Expr a -> Doc Ann
    prettyExpression ns a0@(Lam _ _ _) =
        arrows characterSet (fmap duplicate (docs a0))
      where
        docs (Lam (a, _) b c) = Pretty.group (Pretty.flatAlt long short) : docs c
          where
            long =  (lambda characterSet <> space)
                <>  Pretty.align
                    (   (lparen <> space)
                    <>  prettyLabel a
                    <>  Pretty.hardline
                    <>  (colon <> space)
                    <>  prettyExpression ns b
                    <>  Pretty.hardline
                    <>  rparen
                    )

            short = (lambda characterSet <> lparen)
                <>  prettyLabel a
                <>  (space <> colon <> space)
                <>  prettyExpression ns b
                <>  rparen
        docs          c  = [ prettyExpression ns c ]
    prettyExpression ns a0@(BoolIf _ _ _) =
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
                [   keyword "if" <> " " <> prettyExpression ns a
                ,   prettyExpression ns b
                ]
        docsLong             c  = [ prettyExpression ns c ]

        docsShort (BoolIf a b c) =
            docShort ++ docsShort c
          where
            docShort =
                [   keyword "if" <> " " <> prettyExpression ns a
                ,   prettyExpression ns b
                ]
        docsShort             c  = [ prettyExpression ns c ]
    prettyExpression ns (Let as b) =
        enclose' "" "" space Pretty.hardline
            (fmap duplicate as' ++ [ docB ])
      where
        (ns', as') = docA ns (toList as)

        docB =
            ( keyword "in" <> " " <> prettyExpression ns' b
            , keyword "in" <> "  "  <> prettyExpression ns' b
            )

        docA ns (Binding c Nothing e: bs) =
            (ns', Pretty.group (Pretty.flatAlt long short) : bs')
          where
            (ns', bs') = docA (NBind ns c) bs
            long =  keyword "let" <> space
                <>  Pretty.align
                    (   prettyLabel c
                    <>  space <> equals
                    <>  Pretty.hardline
                    <>  "  "
                    <>  prettyExpression ns e
                    )
            short = keyword "let" <> space
                <>  prettyLabel c
                <>  (space <> equals <> space)
                <>  prettyExpression ns e
        docA ns (Binding c (Just d) e:bs) =
            (ns', Pretty.group (Pretty.flatAlt long short):bs')
          where
            (ns', bs') = docA (NBind ns c) bs
            long = keyword "let" <> space
                <>  Pretty.align
                    (   prettyLabel c
                    <>  Pretty.hardline
                    <>  colon <> space
                    <>  prettyExpression ns d
                    <>  Pretty.hardline
                    <>  equals <> space
                    <>  prettyExpression ns e
                    )

            short = keyword "let" <> space
                <>  prettyLabel c
                <>  space <> colon <> space
                <>  prettyExpression ns d
                <>  space <> equals <> space
                <>  prettyExpression ns e
        docA ns [] = (ns, [])

    prettyExpression ns a0@(Pi _ _ _ _ _) =
        arrows characterSet (fmap duplicate (docs ns a0))
      where
        docs ns (Pi "_" _ _ b c) = prettyOperatorExpression ns b : docs ns c
        docs ns (Pi a _ _ b c) = Pretty.group (Pretty.flatAlt long short) : docs (NBind ns a) c
          where
            long =  forall characterSet <> space
                <>  Pretty.align
                    (   lparen <> space
                    <>  prettyLabel a
                    <>  Pretty.hardline
                    <>  colon <> space
                    <>  prettyExpression ns b
                    <>  Pretty.hardline
                    <>  rparen
                    )

            short = forall characterSet <> lparen
                <>  prettyLabel a
                <>  space <> colon <> space
                <>  prettyExpression ns b
                <>  rparen
        docs ns c  = [ prettyExpression ns c ]
    prettyExpression ns a0 =
        prettyAnnotatedExpression ns a0

    prettyAnnotatedExpression :: Names -> Expr a -> Doc Ann
    prettyAnnotatedExpression ns (Merge a b (Just (c, s))) | annotPrinting s printingOpt  =
        Pretty.group (Pretty.flatAlt long short)
      where
        long =
            Pretty.align
                (   keyword "merge"
                <>  Pretty.hardline
                <>  prettyImportExpression ns a
                <>  Pretty.hardline
                <>  prettyImportExpression ns b
                <>  Pretty.hardline
                <>  colon <> space
                <>  prettyApplicationExpression ns c
                )

        short = keyword "merge" <> space
            <>  prettyImportExpression ns a
            <>  " "
            <>  prettyImportExpression ns b
            <>  space <> colon <> space
            <>  prettyApplicationExpression ns c
    prettyAnnotatedExpression ns (Merge a b _) =
        Pretty.group (Pretty.flatAlt long short)
      where
        long =
            Pretty.align
                (   keyword "merge"
                <>  Pretty.hardline
                <>  prettyImportExpression ns a
                <>  Pretty.hardline
                <>  prettyImportExpression ns b
                )

        short = keyword "merge" <> space
            <>  prettyImportExpression ns a
            <>  " "
            <>  prettyImportExpression ns b
    prettyAnnotatedExpression ns (ToMap a (Just (b, s))) | annotPrinting s printingOpt =
        Pretty.group (Pretty.flatAlt long short)
      where
        long =
            Pretty.align
                (   keyword "toMap"
                <>  Pretty.hardline
                <>  prettyImportExpression ns a
                <>  Pretty.hardline
                <>  colon <> space
                <>  prettyApplicationExpression ns b
                )

        short = keyword "toMap" <> space
            <>  prettyImportExpression ns a
            <>  space <> colon <> space
            <>  prettyApplicationExpression ns b
    prettyAnnotatedExpression ns (ToMap a _) =
        Pretty.group (Pretty.flatAlt long short)
      where
        long =
            Pretty.align
                (   keyword "toMap"
                <>  Pretty.hardline
                <>  prettyImportExpression ns a
                )

        short = keyword "toMap" <> space
            <>  prettyImportExpression ns a
    prettyAnnotatedExpression ns a0@(Annot _ _) =
        enclose'
            ""
            "  "
            (" " <> colon <> " ")
            (colon <> space)
            (fmap duplicate (docs a0))
      where
        docs (Annot a b) = prettyOperatorExpression ns a : docs b
        docs          b  = [ prettyExpression ns b ]
    prettyAnnotatedExpression ns (ListLit (Just (a, s)) b) | annotPrinting s printingOpt =
            list (map (prettyExpression ns) (Data.Foldable.toList b))
        <>  " : "
        <>  prettyApplicationExpression ns (App List a)
    prettyAnnotatedExpression ns a0 =
        prettyOperatorExpression ns a0

    prettyOperatorExpression :: Names -> Expr a -> Doc Ann
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

    prettyImportAltExpression :: Names -> Expr a -> Doc Ann
    prettyImportAltExpression ns a0@(ImportAlt _ _ _) =
        prettyOperator "?" (docs a0)
      where
        docs (ImportAlt a b _) = prettyOrExpression ns b : docs a
        docs              b    = [ prettyOrExpression ns b ]
    prettyImportAltExpression ns a0 =
        prettyOrExpression ns a0

    prettyOrExpression :: Names -> Expr a -> Doc Ann
    prettyOrExpression ns a0@(BoolOr _ _) =
        prettyOperator "||" (docs a0)
      where
        docs (BoolOr a b) = prettyPlusExpression ns b : docs a
        docs           b  = [ prettyPlusExpression ns b ]
    prettyOrExpression ns a0 =
        prettyPlusExpression ns a0

    prettyPlusExpression :: Names -> Expr a -> Doc Ann
    prettyPlusExpression ns a0@(NaturalPlus _ _) =
        prettyOperator "+" (docs a0)
      where
        docs (NaturalPlus a b) = prettyTextAppendExpression ns b : docs a
        docs                b  = [ prettyTextAppendExpression ns b ]
    prettyPlusExpression ns a0 =
        prettyTextAppendExpression ns a0

    prettyTextAppendExpression :: Names -> Expr a -> Doc Ann
    prettyTextAppendExpression ns a0@(TextAppend _ _) =
        prettyOperator "++" (docs a0)
      where
        docs (TextAppend a b) = prettyListAppendExpression ns b : docs a
        docs               b  = [ prettyListAppendExpression ns b ]
    prettyTextAppendExpression ns a0 =
        prettyListAppendExpression ns a0

    prettyListAppendExpression :: Names -> Expr a -> Doc Ann
    prettyListAppendExpression ns a0@(ListAppend _ _) =
        prettyOperator "#" (docs a0)
      where
        docs (ListAppend a b) = prettyAndExpression ns b : docs a
        docs               b  = [ prettyAndExpression ns b ]
    prettyListAppendExpression ns a0 =
        prettyAndExpression ns a0

    prettyAndExpression :: Names -> Expr a -> Doc Ann
    prettyAndExpression ns a0@(BoolAnd _ _) =
        prettyOperator "&&" (docs a0)
      where
        docs (BoolAnd a b) = prettyCombineExpression ns b : docs a
        docs            b  = [ prettyCombineExpression ns b ]
    prettyAndExpression ns a0 =
       prettyCombineExpression ns a0

    prettyCombineExpression :: Names -> Expr a -> Doc Ann
    prettyCombineExpression ns a0@(Combine _ _) =
        prettyOperator (combine characterSet) (docs a0)
      where
        docs (Combine a b) = prettyPreferExpression ns b : docs a
        docs            b  = [ prettyPreferExpression ns b ]
    prettyCombineExpression ns a0 =
        prettyPreferExpression ns a0

    prettyPreferExpression :: Names -> Expr a -> Doc Ann
    prettyPreferExpression ns a0@(Prefer _ _) =
        prettyOperator (prefer characterSet) (docs a0)
      where
        docs (Prefer a b) = prettyCombineTypesExpression ns b : docs a
        docs           b  = [ prettyCombineTypesExpression ns b ]
    prettyPreferExpression ns a0 =
        prettyCombineTypesExpression ns a0

    prettyCombineTypesExpression :: Names -> Expr a -> Doc Ann
    prettyCombineTypesExpression ns a0@(CombineTypes _ _) =
        prettyOperator (combineTypes characterSet) (docs a0)
      where
        docs (CombineTypes a b) = prettyTimesExpression ns b : docs a
        docs                 b  = [ prettyTimesExpression ns b ]
    prettyCombineTypesExpression ns a0 =
        prettyTimesExpression ns a0

    prettyTimesExpression :: Names -> Expr a -> Doc Ann
    prettyTimesExpression ns a0@(NaturalTimes _ _) =
        prettyOperator "*" (docs a0)
      where
        docs (NaturalTimes a b) = prettyEqualExpression ns b : docs a
        docs                 b  = [ prettyEqualExpression ns b ]
    prettyTimesExpression ns a0 =
        prettyEqualExpression ns a0

    prettyEqualExpression :: Names -> Expr a -> Doc Ann
    prettyEqualExpression ns a0@(BoolEQ _ _) =
        prettyOperator "==" (docs a0)
      where
        docs (BoolEQ a b) = prettyNotEqualExpression ns b : docs a
        docs           b  = [ prettyNotEqualExpression ns b ]
    prettyEqualExpression ns a0 =
        prettyNotEqualExpression ns a0

    prettyNotEqualExpression :: Names -> Expr a -> Doc Ann
    prettyNotEqualExpression ns a0@(BoolNE _ _) =
        prettyOperator "!=" (docs a0)
      where
        docs (BoolNE a b) = prettyApplicationExpression ns b : docs a
        docs           b  = [ prettyApplicationExpression ns b ]
    prettyNotEqualExpression ns a0 =
        prettyApplicationExpression ns a0

    prettyApplicationExpression :: Names -> Expr a -> Doc Ann
    prettyApplicationExpression ns a0 = case a0 of
        App _ _  -> result
        Some _   -> result
        _        -> prettyImportExpression ns a0
      where
        result = enclose' "" "" " " "" (fmap duplicate (reverse (docs a0)))

        docs (App  a b) = prettyImportExpression ns b : docs a
        docs (Some   a) = [ prettyImportExpression ns a , builtin "Some"         ]
        docs         b  = [ prettyImportExpression ns b ]

    prettyImportExpression :: Names -> Expr a -> Doc Ann
    prettyImportExpression _ (EmbedImport a) =
        Pretty.pretty a
    prettyImportExpression ns a0 =
        prettySelectorExpression ns a0

    prettySelectorExpression :: Names -> Expr a -> Doc Ann
    prettySelectorExpression ns (Inject a b _) =
        prettySelectorExpression ns a <> dot <> prettyAnyLabel b
    prettySelectorExpression ns (Project a (ProjSingle b)) =
        prettySelectorExpression ns a <> dot <> prettyAnyLabel b
    prettySelectorExpression ns (Project a (ProjSet b Nothing)) =
        prettySelectorExpression ns a <> dot <> prettyLabels b
    prettySelectorExpression ns (Project a (ProjSet _ (Just b))) =
            prettySelectorExpression ns a
        <>  dot
        <>  lparen
        <>  prettyExpression ns b
        <>  rparen
    prettySelectorExpression ns a0 =
        prettyPrimitiveExpression ns a0

    prettyPrimitiveExpression :: Names -> Expr a -> Doc Ann
    prettyPrimitiveExpression ns (Var i) = prettyVar ns i

    prettyPrimitiveExpression _ (Const k) =
        prettyConst k
    prettyPrimitiveExpression _ Bool =
        builtin "Bool"
    prettyPrimitiveExpression _ Natural =
        builtin "Natural"
    prettyPrimitiveExpression _ NaturalFold =
        builtin "Natural/fold"
    prettyPrimitiveExpression _ NaturalBuild =
        builtin "Natural/build"
    prettyPrimitiveExpression _ NaturalIsZero =
        builtin "Natural/isZero"
    prettyPrimitiveExpression _ NaturalEven =
        builtin "Natural/even"
    prettyPrimitiveExpression _ NaturalOdd =
        builtin "Natural/odd"
    prettyPrimitiveExpression _ NaturalToInteger =
        builtin "Natural/toInteger"
    prettyPrimitiveExpression _ NaturalShow =
        builtin "Natural/show"
    prettyPrimitiveExpression _ Integer =
        builtin "Integer"
    prettyPrimitiveExpression _ IntegerShow =
        builtin "Integer/show"
    prettyPrimitiveExpression _ IntegerToDouble =
        builtin "Integer/toDouble"
    prettyPrimitiveExpression _ Double =
        builtin "Double"
    prettyPrimitiveExpression _ DoubleShow =
        builtin "Double/show"
    prettyPrimitiveExpression _ Text =
        builtin "Text"
    prettyPrimitiveExpression _ TextShow =
        builtin "Text/show"
    prettyPrimitiveExpression _ List =
        builtin "List"
    prettyPrimitiveExpression _ ListBuild =
        builtin "List/build"
    prettyPrimitiveExpression _ ListFold =
        builtin "List/fold"
    prettyPrimitiveExpression _ ListLength =
        builtin "List/length"
    prettyPrimitiveExpression _ ListHead =
        builtin "List/head"
    prettyPrimitiveExpression _ ListLast =
        builtin "List/last"
    prettyPrimitiveExpression _ ListIndexed =
        builtin "List/indexed"
    prettyPrimitiveExpression _ ListReverse =
        builtin "List/reverse"
    prettyPrimitiveExpression _ Optional =
        builtin "Optional"
    prettyPrimitiveExpression _ None =
        builtin "None"
    prettyPrimitiveExpression _ OptionalFold =
        builtin "Optional/fold"
    prettyPrimitiveExpression _ OptionalBuild =
        builtin "Optional/build"
    prettyPrimitiveExpression _ (BoolLit True) =
        builtin "True"
    prettyPrimitiveExpression _ (BoolLit False) =
        builtin "False"
    prettyPrimitiveExpression _ (IntegerLit a)
        | 0 <= a    = literal "+" <> prettyNumber a
        | otherwise = prettyNumber a
    prettyPrimitiveExpression _ (NaturalLit a) =
        prettyNatural a
    prettyPrimitiveExpression _ (DoubleLit a) =
        prettyDouble a
    prettyPrimitiveExpression ns (TextLit a) =
        prettyChunks ns a
    prettyPrimitiveExpression ns (Record a) =
        prettyRecord ns a
    prettyPrimitiveExpression ns (RecordLit a) =
        prettyRecordLit ns a
    prettyPrimitiveExpression ns (Union a) =
        prettyUnion ns a
    prettyPrimitiveExpression ns (ListLit _ b) =
        list (map (prettyExpression ns) (Data.Foldable.toList b))
    prettyPrimitiveExpression ns a =
        Pretty.group (Pretty.flatAlt long short)
      where
        long =
            Pretty.align
                (lparen <> space <> prettyExpression ns a <> Pretty.hardline <> rparen)
        short = lparen <> prettyExpression ns a <> rparen

    prettyKeyValue :: Names -> Doc Ann -> (Text, Expr a) -> (Doc Ann, Doc Ann)
    prettyKeyValue ns separator (key, val) =
        (       prettyAnyLabel key
            <>  " "
            <>  separator
            <>  " "
            <>  prettyExpression ns val
        ,       prettyAnyLabel key
            <>  " "
            <>  separator
            <>  long
        )
      where
        long = Pretty.hardline <> "    " <> prettyExpression ns val

    prettyRecord :: Names -> Map Text (Expr a) -> Doc Ann
    prettyRecord ns =
        braces . map (prettyKeyValue ns colon) . Dhall.Map.toList

    prettyRecordLit :: Names -> Map Text (Expr a) -> Doc Ann
    prettyRecordLit ns a
        | Data.Foldable.null a =
            lbrace <> equals <> rbrace
        | otherwise
            = braces (map (prettyKeyValue ns equals) (Dhall.Map.toList a))

    prettyAlternative ns (key, Just val) = prettyKeyValue ns colon (key, val)
    prettyAlternative _  (key, Nothing ) = duplicate (prettyAnyLabel key)

    prettyUnion :: Names -> Map Text (Maybe (Expr a)) -> Doc Ann
    prettyUnion ns =
        angles . map (prettyAlternative ns) . Dhall.Map.toList

    prettyChunks :: Names -> Chunks (Expr a) -> Doc Ann
    prettyChunks ns (Chunks a b) =
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
            <>  prettyExpression ns d
            <>  rbrace

        prettyMultilineBuilder builder = literal (mconcat docs)
          where
            lazyLines = Text.splitOn "\n" (escapeSingleQuotedText builder)

            docs =
                Data.List.intersperse Pretty.hardline (fmap Pretty.pretty lazyLines)

        prettyChunk (c, d) =
                prettyText c
            <>  syntax "${"
            <>  prettyExpression ns d
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
