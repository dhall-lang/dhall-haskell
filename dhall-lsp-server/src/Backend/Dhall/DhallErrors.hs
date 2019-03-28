module Backend.Dhall.DhallErrors(simpleTypeMessage) where

{-| This file contains mostly copy-paste error formatting code from <root>/dhall/src/Dhall/TypeCheck.hs
    This had to be necessary as to strip down extra information that standard error formatting provides (location, ascii-formatting).
-}

import Dhall.Binary (ToTerm)
import Dhall.TypeCheck
import Dhall.Core(Expr)
import Dhall.Pretty(Ann(..), layoutOpts)

import qualified Dhall.Diff
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.String as R
import qualified Data.Text as T

prettyDiff :: (Eq a, Pretty.Pretty a, ToTerm a) => Expr s a -> Expr s a -> Text
prettyDiff exprL exprR = T.pack . R.renderString . Pretty.layoutPretty layoutOpts  . Pretty.unAnnotate $ diff
  where
   diff = Dhall.Diff.diffNormalized exprL exprR

simpleTypeMessage
    :: (Eq a, Pretty.Pretty a, ToTerm a) => TypeMessage s a -> Text
simpleTypeMessage (UnboundVariable x) = 
    "Unbound variable: " <>  x

simpleTypeMessage (InvalidInputType expr) = 
    "Invalid function input"

simpleTypeMessage (InvalidOutputType expr) = 
    "Invalid function output"

simpleTypeMessage (NotAFunction expr0 expr1) = 
    "Not a function"

simpleTypeMessage (TypeMismatch expr0 expr1 expr2 expr3) = 
    "Wrong type of function argument\n"
        <>  "\n"
        <>  prettyDiff expr1 expr3

simpleTypeMessage (AnnotMismatch expr0 expr1 expr2) = 
    "Expression doesn't match annotation\n"
        <>  "\n"
        <>  prettyDiff expr1 expr2
    

simpleTypeMessage Untyped = 
    "❰Sort❱ has no type, kind, or sort"

    

simpleTypeMessage (InvalidPredicate expr0 expr1) = 
    "Invalid predicate for ❰if❱"

simpleTypeMessage (IfBranchMustBeTerm b expr0 expr1 expr2) =
    "❰if❱ branch is not a term"

simpleTypeMessage (IfBranchMismatch expr0 expr1 expr2 expr3) =
    "❰if❱ branches must have matching types\n"
        <>  "\n"
        <>  prettyDiff expr1 expr3

simpleTypeMessage (InvalidListType expr0) = 
    "Invalid type for ❰List❱ elements"

simpleTypeMessage MissingListType = 
    "An empty list requires a type annotation"

simpleTypeMessage (MismatchedListElements i expr0 _expr1 expr2) =
    "List elements should all have the same type\n"
        <>  "\n"
        <>  prettyDiff expr0 expr2

simpleTypeMessage (InvalidListElement i expr0 _expr1 expr2) =
    "List element has the wrong type\n"
        <>  "\n"
        <>  prettyDiff expr0 expr2

simpleTypeMessage (InvalidOptionalType expr0) = 
    "Invalid type for ❰Optional❱ element"

simpleTypeMessage (InvalidOptionalElement expr0 expr1 expr2) =  
    "❰Optional❱ element has the wrong type\n"
        <>  "\n"
        <>  prettyDiff expr0 expr2

    
simpleTypeMessage (InvalidSome expr0 expr1 expr2) = 
    "❰Some❱ argument has the wrong type"

simpleTypeMessage (InvalidFieldType k expr0) = 
    "Invalid field type"

simpleTypeMessage (FieldAnnotationMismatch k0 expr0 c0 k1 expr1 c1) = 
    "Field annotation mismatch"

simpleTypeMessage (FieldMismatch k0 expr0 c0 k1 expr1 c1) = 
    "Field mismatch"

simpleTypeMessage (InvalidField k expr0) = 
    "Invalid field"

simpleTypeMessage (InvalidAlternativeType k expr0) = 
    "Invalid alternative type"

simpleTypeMessage (InvalidAlternative k expr0) = 
    "Invalid alternative"

simpleTypeMessage (AlternativeAnnotationMismatch k0 expr0 c0 k1 expr1 c1) = 
    "Alternative annotation mismatch"

simpleTypeMessage (ListAppendMismatch expr0 expr1) = 
    "You can only append ❰List❱s with matching element types\n"
        <>  "\n"
        <>  prettyDiff expr0 expr1

simpleTypeMessage (DuplicateAlternative k) = 
    "Duplicate union alternative"

simpleTypeMessage (MustCombineARecord c expr0 expr1) = 
    "You can only combine records"

simpleTypeMessage (RecordMismatch c expr0 expr1 const0 const1) = 
    "Record mismatch"

simpleTypeMessage (CombineTypesRequiresRecordType expr0 expr1) =
    "❰⩓❱ requires arguments that are record types"

simpleTypeMessage (RecordTypeMismatch const0 const1 expr0 expr1) = 
    "Record type mismatch"

simpleTypeMessage (FieldCollision k) = 
    "Field collision"

simpleTypeMessage (MustMergeARecord expr0 expr1) = 
    "❰merge❱ expects a record of handlers"

simpleTypeMessage (MustMergeUnion expr0 expr1) = 
    "❰merge❱ expects a union"

simpleTypeMessage (UnusedHandler ks) = 
    "Unused handler"

simpleTypeMessage (MissingHandler ks) = 
    "Missing handler"

simpleTypeMessage MissingMergeType =
    "An empty ❰merge❱ requires a type annotation"

simpleTypeMessage (HandlerInputTypeMismatch expr0 expr1 expr2) =
    "Wrong handler input type\n"
        <>  "\n"
        <>  prettyDiff expr1 expr2

simpleTypeMessage (InvalidHandlerOutputType expr0 expr1 expr2) =
    "Wrong handler output type\n"
        <>  "\n"
        <>  prettyDiff expr1 expr2

simpleTypeMessage (HandlerOutputTypeMismatch key0 expr0 key1 expr1) =
    "Handlers should have the same output type\n"
        <>  "\n"
        <>  prettyDiff expr0 expr1

simpleTypeMessage (HandlerNotAFunction k expr0) = 
    "Handler is not a function"

simpleTypeMessage (ConstructorsRequiresAUnionType expr0 expr1) = 
    "❰constructors❱ requires a union type"

simpleTypeMessage (CantAccess lazyText0 expr0 expr1) = 
    "Not a record or a union"

simpleTypeMessage (CantProject lazyText0 expr0 expr1) = 
    "Not a record"

simpleTypeMessage (MissingField k expr0) = 
    "Missing record field"

simpleTypeMessage (CantAnd expr0 expr1) =
        buildBooleanOperator "&&" expr0 expr1

simpleTypeMessage (CantOr expr0 expr1) =
        buildBooleanOperator "||" expr0 expr1

simpleTypeMessage (CantEQ expr0 expr1) =
        buildBooleanOperator "==" expr0 expr1

simpleTypeMessage (CantNE expr0 expr1) =
        buildBooleanOperator "!=" expr0 expr1

simpleTypeMessage (CantInterpolate expr0 expr1) = 
    "You can only interpolate ❰Text❱"

simpleTypeMessage (CantTextAppend expr0 expr1) = 
    "❰++❱ only works on ❰Text❱"

simpleTypeMessage (CantListAppend expr0 expr1) = 
    "❰#❱ only works on ❰List❱s"

simpleTypeMessage (CantAdd expr0 expr1) =
        buildNaturalOperator "+" expr0 expr1

simpleTypeMessage (CantMultiply expr0 expr1) =
        buildNaturalOperator "*" expr0 expr1

simpleTypeMessage (NoDependentTypes expr0 expr1) = 
    "No dependent types"

buildBooleanOperator ::  Text -> Expr s a -> Expr s a -> Text
buildBooleanOperator operator expr0 expr1 = 
    "❰" <> txt2 <> "❱ only works on ❰Bool❱s"
  where
    txt2 = operator 

buildNaturalOperator ::  Text -> Expr s a -> Expr s a -> Text
buildNaturalOperator operator expr0 expr1 = 
    "❰" <> txt2 <> "❱ only works on ❰Natural❱s"

    
  where
    txt2 = operator






