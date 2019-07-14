{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Dhall.Errors where

import Control.Exception (Exception(..))
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (asks)
import Crypto.Hash (SHA256)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (Semigroup(..))
import Data.Set (Set)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, Pretty(..))
import Data.Typeable (Typeable)
import Dhall.Context (Cxt(..), ElabM, ImportState(..), typesToList)
import Dhall.Core (Const(..), Import(..))
import Dhall.Eval (Core)
import Dhall.Parser.Combinators (Src)
import Dhall.Pretty (Ann, layoutOpts)

import qualified Data.Set
import qualified Data.Text                               as Text
import qualified Data.Text.Prettyprint.Doc               as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.String as Pretty
import qualified Dhall.Diff
import qualified Dhall.Pretty.Internal
import qualified Dhall.Util
import qualified Crypto.Hash

-- Elaboration errors
--------------------------------------------------------------------------------

-- | An elaboration error that includes import stack, source position and local context
data ContextualError =
  ContextualError !(NonEmpty Import) !Cxt !(Maybe Src) !ElabError

-- We distinguish disabled import resolution from the other import errors, because we don't want
-- to recover from the former when elaborating import alternatives.
data ElabError = TypeError !TypeError | ImportError !ImportError | ImportResolutionDisabled

elabError :: Cxt -> ElabError -> ElabM a
elabError cxt err = do
  stack <- asks _stack
  throwM (ContextualError stack cxt Nothing err)

typeError :: Cxt -> TypeError -> ElabM a
typeError cxt = elabError cxt . TypeError

importError :: Cxt -> ImportError -> ElabM a
importError cxt = elabError cxt . ImportError

instance Show ContextualError where
    show = Pretty.renderString . Pretty.layoutPretty layoutOpts . Pretty.pretty

instance Exception ContextualError

prettyContextualError :: (ElabError -> Doc ann) -> ContextualError -> Doc xxx
prettyContextualError prettyElab (ContextualError imports ctx pos msg) =
  fromString (concat (zipWith indent [0..] toDisplay) ++ "\n") <>
  Pretty.unAnnotate
      (   "\n"
      <>  (if null (typesToList $ _types ctx)
          then ""
          else prettyContext ctx <> "\n\n")
      <>  prettyElab msg
      <>  source)
  where
    indent n import_ =
      "\n" ++ replicate (2 * n) ' ' ++ "↳ " ++ Dhall.Pretty.Internal.prettyToString import_

    -- Tthe final (outermost) import is fake to establish the base
    -- directory. Also, we need outermost-first.
    toDisplay = drop 1 (reverse (toList imports))
    prettyKV (key, val) = pretty key <> " : " <> Dhall.Util.snipDoc (pretty val)

    prettyContext =
            Pretty.vsep
        .   map prettyKV
        .   reverse
        .   typesToList
        .   _types

    source = maybe mempty pretty pos

instance Pretty ContextualError where
    pretty = prettyContextualError $ \case
      TypeError err            -> shortTypeMessage err <> "\n"
      ImportError err          -> fromString (show err) <> "\n"
      ImportResolutionDisabled -> "Import resolution is disabled\n"

{-| Newtype used to wrap error messages so that they render with a more
    detailed explanation of what went wrong
-}
newtype DetailedContextualError = DetailedContextualError ContextualError
    deriving (Typeable)

instance Show DetailedContextualError where
    show = Pretty.renderString . Pretty.layoutPretty layoutOpts . Pretty.pretty

instance Exception DetailedContextualError

instance Pretty DetailedContextualError where
    pretty (DetailedContextualError cxtErr) = prettyContextualError (
      \m -> (case m of
              TypeError err            -> longTypeMessage err <> "\n"
              ImportError err          -> fromString (show err) <> "\n"
              ImportResolutionDisabled -> "Import resolution is disabled\n")
          <>  "────────────────────────────────────────────────────────────────────────────────\n"
          <>  "\n")
      cxtErr

-- Import errors
--------------------------------------------------------------------------------


data ImportError
  -- | An import failed because of a cycle in the import graph
  = Cycle !Import

  {-| Dhall tries to ensure that all expressions hosted on network endpoints are
      weakly referentially transparent, meaning roughly that any two clients will
      compile the exact same result given the same URL.

      To be precise, a strong interpretaton of referential transparency means that
      if you compiled a URL you could replace the expression hosted at that URL
      with the compiled result.  Let's call this \"static linking\".  Dhall (very
      intentionally) does not satisfy this stronger interpretation of referential
      transparency since \"statically linking\" an expression (i.e. permanently
      resolving all imports) means that the expression will no longer update if
      its dependencies change.

      In general, either interpretation of referential transparency is not
      enforceable in a networked context since one can easily violate referential
      transparency with a custom DNS, but Dhall can still try to guard against
      common unintentional violations.  To do this, Dhall enforces that a
      non-local import may not reference a local import.

      Local imports are defined as:

      * A file

      * A URL with a host of @localhost@ or @127.0.0.1@

      All other imports are defined to be non-local
  -}
  | ReferentiallyOpaque !Import

  -- | Exception thrown when an imported file is missing
  | MissingFile !FilePath

  -- | Exception thrown when an environment variable is missing
  | MissingEnvironmentVariable !Text

  -- | Exception thrown when a HTTP url is imported but dhall was built without
  -- the @with-http@ Cabal flag.
  | CannotImportHTTPURL !String

  -- | Exception thrown when encountering a "missing" import.
  | MissingImport

  -- | Exception thrown when an integrity check fails
  | HashMismatch { expectedHash :: Crypto.Hash.Digest SHA256
                 , actualHash   :: Crypto.Hash.Digest SHA256}

instance Exception ImportError

instance Show ImportError where
  show (Cycle import_) =
    "\nCyclic import: " ++ Dhall.Pretty.Internal.prettyToString import_
  show (ReferentiallyOpaque import_) =
    "\nReferentially opaque import: " ++ Dhall.Pretty.Internal.prettyToString import_
  show (MissingFile path) =
            "\n"
        <>  "\ESC[1;31mError\ESC[0m: Missing file "
        <>  path
  show (MissingEnvironmentVariable name) =
            "\n"
        <>  "\ESC[1;31mError\ESC[0m: Missing environment variable\n"
        <>  "\n"
        <>  "↳ " <> Text.unpack name
  show (CannotImportHTTPURL url) =
            "\n"
        <>  "\ESC[1;31mError\ESC[0m: Cannot import HTTP URL.\n"
        <>  "\n"
        <>  "Dhall was compiled without the 'with-http' flag.\n"
        <>  "\n"
        <>  "The requested URL was: "
        <>  url
        <>  "\n"
  show MissingImport = "Cannot import \"missing\""
  show (HashMismatch {..}) =
            "\n"
        <>  "\ESC[1;31mError\ESC[0m: Import integrity check failed\n"
        <>  "\n"
        <>  "Expected hash:\n"
        <>  "\n"
        <>  "↳ " <> show expectedHash <> "\n"
        <>  "\n"
        <>  "Actual hash:\n"
        <>  "\n"
        <>  "↳ " <> show actualHash <> "\n"


-- Type errors
--------------------------------------------------------------------------------

data TypeError
    = UnboundVariable !Text
    | InvalidInputType !Core
    | InvalidOutputType !Core
    | NotAFunction !Core !Core
    | TypeMismatch !Core !Core !Core !Core
    | AnnotMismatch !Core !Core !Core
    | Untyped
    | MissingListType
    | MismatchedListElements !Int !Core !Core !Core
    | InvalidListElement !Int !Core !Core !Core
    | InvalidListType !Core
    | InvalidOptionalElement !Core  !Core !Core
    | InvalidOptionalType !Core
    | InvalidSome !Core !Core !Core
    | InvalidPredicate !Core !Core
    | IfBranchMismatch !Core !Core !Core !Core
    | IfBranchMustBeTerm Bool !Core !Core !Core
    | InvalidFieldType !Text !Core
    | FieldAnnotationMismatch !Text !Core !Const !Text !Core !Const
    | FieldMismatch !Text !Core !Const !Text !Core !Const
    | InvalidAlternative !Text !Core
    | InvalidAlternativeType !Text !Core
    | AlternativeAnnotationMismatch !Text !Core !Const !Text !Core !Const
    | ListAppendMismatch !Core !Core
    | DuplicateAlternative !Text
    | MustCombineARecord Char !Core !Core
    | RecordMismatch Char !Core !Core !Const !Const
    | CombineTypesRequiresRecordType !Core !Core
    | RecordTypeMismatch !Const !Const !Core !Core
    | FieldCollision !Text
    | MustMergeARecord !Core !Core
    | MustMergeUnion !Core !Core
    | UnusedHandler !(Set Text)
    | MissingHandler !(Set Text)
    | HandlerInputTypeMismatch !Text !Core !Core
    | HandlerOutputTypeMismatch !Text !Core  !Text !Core
    | InvalidHandlerOutputType !Text !Core !Core
    | MissingMergeType
    | HandlerNotAFunction !Text !Core
    | CantAccess !Text !Core !Core
    | CantProject !Text !Core !Core
    | MissingField !Text !Core
    | MissingAlternative !Text !Core
    | CantAnd !Core !Core
    | CantOr !Core !Core
    | CantEQ !Core !Core
    | CantNE !Core !Core
    | CantInterpolate !Core !Core
    | CantTextAppend !Core !Core
    | CantListAppend !Core !Core
    | CantAdd !Core !Core
    | CantMultiply !Core !Core
    | NoDependentTypes !Core !Core

    | ExpectedAType !Core
    | UnexpectedRecordField !Text !Core
    | ConvError !Core !Core
    | MergeDependentHandler !Text !Core
    deriving (Show)

shortTypeMessage :: TypeError -> Doc Ann
shortTypeMessage msg =
    "\ESC[1;31mError\ESC[0m: " <> short <> "\n"
  where
    ErrorMessages {..} = prettyTypeMessage msg

longTypeMessage :: TypeError -> Doc Ann
longTypeMessage msg =
        "\ESC[1;31mError\ESC[0m: " <> short <> "\n"
    <>  "\n"
    <>  long
  where
    ErrorMessages {..} = prettyTypeMessage msg

data ErrorMessages = ErrorMessages
    { short :: Doc Ann
    -- ^ Default succinct 1-line explanation of what went wrong
    , long  :: Doc Ann
    -- ^ Longer and more detailed explanation of the error
    }

_NOT :: Doc ann
_NOT = "\ESC[1mnot\ESC[0m"

prettyTypeMessage :: TypeError -> ErrorMessages
prettyTypeMessage (UnboundVariable x) = ErrorMessages {..}
  -- We do not need to print variable name here. For the discussion see:
  -- https://github.com/dhall-lang/dhall-haskell/pull/116
  where
    short = "Unbound variable: " <> Pretty.pretty x

    long =
        "Explanation: Expressions can only reference previously introduced (i.e. “bound”)\n\
        \variables that are still “in scope”                                             \n\
        \                                                                                \n\
        \For example, the following valid expressions introduce a “bound” variable named \n\
        \❰x❱:                                                                            \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────┐                                                         \n\
        \    │ λ(x : Bool) → x │  Anonymous functions introduce “bound” variables        \n\
        \    └─────────────────┘                                                         \n\
        \        ⇧                                                                       \n\
        \        This is the bound variable                                              \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────┐                                                         \n\
        \    │ let x = 1 in x  │  ❰let❱ expressions introduce “bound” variables          \n\
        \    └─────────────────┘                                                         \n\
        \          ⇧                                                                     \n\
        \          This is the bound variable                                            \n\
        \                                                                                \n\
        \                                                                                \n\
        \However, the following expressions are not valid because they all reference a   \n\
        \variable that has not been introduced yet (i.e. an “unbound” variable):         \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────┐                                                         \n\
        \    │ λ(x : Bool) → y │  The variable ❰y❱ hasn't been introduced yet            \n\
        \    └─────────────────┘                                                         \n\
        \                    ⇧                                                           \n\
        \                    This is the unbound variable                                \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────────┐                                                \n\
        \    │ (let x = True in x) && x │  ❰x❱ is undefined outside the parentheses      \n\
        \    └──────────────────────────┘                                                \n\
        \                             ⇧                                                  \n\
        \                             This is the unbound variable                       \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────┐                                                          \n\
        \    │ let x = x in x │  The definition for ❰x❱ cannot reference itself          \n\
        \    └────────────────┘                                                          \n\
        \              ⇧                                                                 \n\
        \              This is the unbound variable                                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \Some common reasons why you might get this error:                               \n\
        \                                                                                \n\
        \● You misspell a variable name, like this:                                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────────────────────────────────┐                      \n\
        \    │ λ(empty : Bool) → if emty then \"Empty\" else \"Full\" │                      \n\
        \    └────────────────────────────────────────────────────┘                      \n\
        \                           ⇧                                                    \n\
        \                           Typo                                                 \n\
        \                                                                                \n\
        \                                                                                \n\
        \● You misspell a reserved identifier, like this:                                \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────────┐                                                \n\
        \    │ foral (a : Type) → a → a │                                                \n\
        \    └──────────────────────────┘                                                \n\
        \      ⇧                                                                         \n\
        \      Typo                                                                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \● You tried to define a recursive value, like this:                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────┐                                                      \n\
        \    │ let x = x + 1 in x │                                                      \n\
        \    └────────────────────┘                                                      \n\
        \              ⇧                                                                 \n\
        \              Recursive definitions are not allowed                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \● You accidentally forgot a ❰λ❱ or ❰∀❱/❰forall❱                                 \n\
        \                                                                                \n\
        \                                                                                \n\
        \        Unbound variable                                                        \n\
        \        ⇩                                                                       \n\
        \    ┌─────────────────┐                                                         \n\
        \    │  (x : Bool) → x │                                                         \n\
        \    └─────────────────┘                                                         \n\
        \      ⇧                                                                         \n\
        \      A ❰λ❱ here would transform this into a valid anonymous function           \n\
        \                                                                                \n\
        \                                                                                \n\
        \        Unbound variable                                                        \n\
        \        ⇩                                                                       \n\
        \    ┌────────────────────┐                                                      \n\
        \    │  (x : Bool) → Bool │                                                      \n\
        \    └────────────────────┘                                                      \n\
        \      ⇧                                                                         \n\
        \      A ❰∀❱ or ❰forall❱ here would transform this into a valid function type    \n\
        \                                                                                \n\
        \                                                                                \n\
        \● You forgot to prefix a file path with ❰./❱:                                   \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────┐                                                      \n\
        \    │ path/to/file.dhall │                                                      \n\
        \    └────────────────────┘                                                      \n\
        \      ⇧                                                                         \n\
        \      This should be ❰./path/to/file.dhall❱                                     \n"

prettyTypeMessage (InvalidInputType expr) = ErrorMessages {..}
  where
    short = "Expected a type for function input"

    long =
        "Explanation: A function can accept an input “term” that has a given “type”, like\n\
        \this:                                                                           \n\
        \                                                                                \n\
        \                                                                                \n\
        \        This is the input term that the function accepts                        \n\
        \        ⇩                                                                       \n\
        \    ┌───────────────────────┐                                                   \n\
        \    │ ∀(x : Natural) → Bool │  This is the type of a function that accepts an   \n\
        \    └───────────────────────┘  input term named ❰x❱ that has type ❰Natural❱     \n\
        \            ⇧                                                                   \n\
        \            This is the type of the input term                                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────┐                                                          \n\
        \    │ Bool → Natural │  This is the type of a function that accepts an anonymous\n\
        \    └────────────────┘  input term that has type ❰Bool❱                         \n\
        \      ⇧                                                                         \n\
        \      This is the type of the input term                                        \n\
        \                                                                                \n\
        \                                                                                \n\
        \... or a function can accept an input “type” that has a given “kind”, like this:\n\
        \                                                                                \n\
        \                                                                                \n\
        \        This is the input type that the function accepts                        \n\
        \        ⇩                                                                       \n\
        \    ┌────────────────────┐                                                      \n\
        \    │ ∀(a : Type) → Type │  This is the type of a function that accepts an input\n\
        \    └────────────────────┘  type named ❰a❱ that has kind ❰Type❱                 \n\
        \            ⇧                                                                   \n\
        \            This is the kind of the input type                                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────┐                                                    \n\
        \    │ (Type → Type) → Type │  This is the type of a function that accepts an    \n\
        \    └──────────────────────┘  anonymous input type that has kind ❰Type → Type❱  \n\
        \       ⇧                                                                        \n\
        \       This is the kind of the input type                                       \n\
        \                                                                                \n\
        \                                                                                \n\
        \Other function inputs are " <> _NOT <> " valid, like this:                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────┐                                                            \n\
        \    │ ∀(x : 1) → x │  ❰1❱ is a “term” and not a “type” nor a “kind” so ❰x❱      \n\
        \    └──────────────┘  cannot have “type” ❰1❱ or “kind” ❰1❱                      \n\
        \            ⇧                                                                   \n\
        \            This is not a type or kind                                          \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────┐                                                                \n\
        \    │ True → x │  ❰True❱ is a “term” and not a “type” nor a “kind” so the       \n\
        \    └──────────┘  anonymous input cannot have “type” ❰True❱ or “kind” ❰True❱    \n\
        \      ⇧                                                                         \n\
        \      This is not a type or kind                                                \n\
        \                                                                                \n\
        \                                                                                \n\
        \You annotated a function input with the following expression:                   \n\
        \                                                                                \n\
        \" <> txt <> "\n\
        \                                                                                \n\
        \... which is neither a type nor a kind                                          \n"
      where
        txt = insert expr

prettyTypeMessage (InvalidOutputType expr) = ErrorMessages {..}
  where
    short = "Invalid function output"

    long =
        "Explanation: A function can return an output “term” that has a given “type”,    \n\
        \like this:                                                                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────┐                                                      \n\
        \    │ ∀(x : Text) → Bool │  This is the type of a function that returns an      \n\
        \    └────────────────────┘  output term that has type ❰Bool❱                    \n\
        \                    ⇧                                                           \n\
        \                    This is the type of the output term                         \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────┐                                                          \n\
        \    │ Bool → Natural │  This is the type of a function that returns an output   \n\
        \    └────────────────┘  term that has type ❰Natural❱                            \n\
        \             ⇧                                                                  \n\
        \             This is the type of the output term                                \n\
        \                                                                                \n\
        \                                                                                \n\
        \... or a function can return an output “type” that has a given “kind”, like     \n\
        \this:                                                                           \n\
        \                                                                                \n\
        \    ┌────────────────────┐                                                      \n\
        \    │ ∀(a : Type) → Type │  This is the type of a function that returns an      \n\
        \    └────────────────────┘  output type that has kind ❰Type❱                    \n\
        \                    ⇧                                                           \n\
        \                    This is the kind of the output type                         \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────┐                                                    \n\
        \    │ (Type → Type) → Type │  This is the type of a function that returns an    \n\
        \    └──────────────────────┘  output type that has kind ❰Type❱                  \n\
        \                      ⇧                                                         \n\
        \                      This is the kind of the output type                       \n\
        \                                                                                \n\
        \                                                                                \n\
        \Other outputs are " <> _NOT <> " valid, like this:                              \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────┐                                                         \n\
        \    │ ∀(x : Bool) → x │  ❰x❱ is a “term” and not a “type” nor a “kind” so the   \n\
        \    └─────────────────┘  output cannot have “type” ❰x❱ or “kind” ❰x❱            \n\
        \                    ⇧                                                           \n\
        \                    This is not a type or kind                                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────┐                                                             \n\
        \    │ Text → True │  ❰True❱ is a “term” and not a “type” nor a “kind” so the    \n\
        \    └─────────────┘  output cannot have “type” ❰True❱ or “kind” ❰True❱          \n\
        \             ⇧                                                                  \n\
        \             This is not a type or kind                                         \n\
        \                                                                                \n\
        \                                                                                \n\
        \Some common reasons why you might get this error:                               \n\
        \                                                                                \n\
        \● You use ❰∀❱ instead of ❰λ❱ by mistake, like this:                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────┐                                                          \n\
        \    │ ∀(x: Bool) → x │                                                          \n\
        \    └────────────────┘                                                          \n\
        \      ⇧                                                                         \n\
        \      Using ❰λ❱ here instead of ❰∀❱ would transform this into a valid function  \n\
        \                                                                                \n\
        \                                                                                \n\
        \────────────────────────────────────────────────────────────────────────────────\n\
        \                                                                                \n\
        \You specified that your function outputs a:                                     \n\
        \                                                                                \n\
        \" <> txt <> "\n\
        \                                                                                \n\
        \... which is neither a type nor a kind:                                         \n"
      where
        txt = insert expr

prettyTypeMessage (NotAFunction expr0 expr1) = ErrorMessages {..}
  where
    short = "Not a function"

    long =
        "Explanation: Expressions separated by whitespace denote function application,   \n\
        \like this:                                                                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────┐                                                                     \n\
        \    │ f x │  This denotes the function ❰f❱ applied to an argument named ❰x❱     \n\
        \    └─────┘                                                                     \n\
        \                                                                                \n\
        \                                                                                \n\
        \A function is a term that has type ❰a → b❱ for some ❰a❱ or ❰b❱.  For example,   \n\
        \the following expressions are all functions because they have a function type:  \n\
        \                                                                                \n\
        \                                                                                \n\
        \                        The function's input type is ❰Bool❱                     \n\
        \                        ⇩                                                       \n\
        \    ┌───────────────────────────────┐                                           \n\
        \    │ λ(x : Bool) → x : Bool → Bool │  User-defined anonymous function          \n\
        \    └───────────────────────────────┘                                           \n\
        \                               ⇧                                                \n\
        \                               The function's output type is ❰Bool❱             \n\
        \                                                                                \n\
        \                                                                                \n\
        \                     The function's input type is ❰Natural❱                     \n\
        \                     ⇩                                                          \n\
        \    ┌───────────────────────────────┐                                           \n\
        \    │ Natural/even : Natural → Bool │  Built-in function                        \n\
        \    └───────────────────────────────┘                                           \n\
        \                               ⇧                                                \n\
        \                               The function's output type is ❰Bool❱             \n\
        \                                                                                \n\
        \                                                                                \n\
        \                        The function's input kind is ❰Type❱                     \n\
        \                        ⇩                                                       \n\
        \    ┌───────────────────────────────┐                                           \n\
        \    │ λ(a : Type) → a : Type → Type │  Type-level functions are still functions \n\
        \    └───────────────────────────────┘                                           \n\
        \                               ⇧                                                \n\
        \                               The function's output kind is ❰Type❱             \n\
        \                                                                                \n\
        \                                                                                \n\
        \             The function's input kind is ❰Type❱                                \n\
        \             ⇩                                                                  \n\
        \    ┌────────────────────┐                                                      \n\
        \    │ List : Type → Type │  Built-in type-level function                        \n\
        \    └────────────────────┘                                                      \n\
        \                    ⇧                                                           \n\
        \                    The function's output kind is ❰Type❱                        \n\
        \                                                                                \n\
        \                                                                                \n\
        \                        Function's input has kind ❰Type❱                        \n\
        \                        ⇩                                                       \n\
        \    ┌─────────────────────────────────────────────────┐                         \n\
        \    │ List/head : ∀(a : Type) → (List a → Optional a) │  A function can return  \n\
        \    └─────────────────────────────────────────────────┘  another function       \n\
        \                                ⇧                                               \n\
        \                                Function's output has type ❰List a → Optional a❱\n\
        \                                                                                \n\
        \                                                                                \n\
        \                       The function's input type is ❰List Text❱                 \n\
        \                       ⇩                                                        \n\
        \    ┌────────────────────────────────────────────┐                              \n\
        \    │ List/head Text : List Text → Optional Text │  A function applied to an    \n\
        \    └────────────────────────────────────────────┘  argument can be a function  \n\
        \                                   ⇧                                            \n\
        \                                   The function's output type is ❰Optional Text❱\n\
        \                                                                                \n\
        \                                                                                \n\
        \An expression is not a function if the expression's type is not of the form     \n\
        \❰a → b❱.  For example, these are " <> _NOT <> " functions:                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────┐                                                             \n\
        \    │ 1 : Natural │  ❰1❱ is not a function because ❰Natural❱ is not the type of \n\
        \    └─────────────┘  a function                                                 \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────┐                                                   \n\
        \    │ Natural/even 2 : Bool │  ❰Natural/even 2❱ is not a function because       \n\
        \    └───────────────────────┘  ❰Bool❱ is not the type of a function             \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────┐                                                        \n\
        \    │ List Text : Type │  ❰List Text❱ is not a function because ❰Type❱ is not   \n\
        \    └──────────────────┘  the type of a function                                \n\
        \                                                                                \n\
        \                                                                                \n\
        \Some common reasons why you might get this error:                               \n\
        \                                                                                \n\
        \● You tried to add two ❰Natural❱s without a space around the ❰+❱, like this:    \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────┐                                                                     \n\
        \    │ 2+2 │                                                                     \n\
        \    └─────┘                                                                     \n\
        \                                                                                \n\
        \                                                                                \n\
        \  The above code is parsed as:                                                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────┐                                                                  \n\
        \    │ 2 (+2) │                                                                  \n\
        \    └────────┘                                                                  \n\
        \      ⇧                                                                         \n\
        \      The compiler thinks that this ❰2❱ is a function whose argument is ❰+2❱    \n\
        \                                                                                \n\
        \                                                                                \n\
        \  This is because the ❰+❱ symbol has two meanings: you use ❰+❱ to add two       \n\
        \  numbers, but you also can prefix ❰Natural❱ literals with a ❰+❱ to turn them   \n\
        \  into ❰Integer❱ literals (like ❰+2❱)                                           \n\
        \                                                                                \n\
        \  To fix the code, you need to put spaces around the ❰+❱, like this:            \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────┐                                                                   \n\
        \    │ 2 + 2 │                                                                   \n\
        \    └───────┘                                                                   \n\
        \                                                                                \n\
        \                                                                                \n\
        \────────────────────────────────────────────────────────────────────────────────\n\
        \                                                                                \n\
        \You tried to use the following expression as a function:                        \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... but this expression's type is:                                              \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... which is not a function type                                                \n"
      where
        txt0 = insert expr0
        txt1 = insert expr1

prettyTypeMessage (TypeMismatch expr0 expr1 expr2 expr3) = ErrorMessages {..}
  where
    short = "Wrong type of function argument\n"
        <>  "\n"
        <>  Dhall.Diff.diffNormalized expr1 expr3

    long =
        "Explanation: Every function declares what type or kind of argument to accept    \n\
        \                                                                                \n\
        \For example:                                                                    \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────┐                                           \n\
        \    │ λ(x : Bool) → x : Bool → Bool │  This anonymous function only accepts     \n\
        \    └───────────────────────────────┘  arguments that have type ❰Bool❱          \n\
        \                        ⇧                                                       \n\
        \                        The function's input type                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────┐                                           \n\
        \    │ Natural/even : Natural → Bool │  This built-in function only accepts      \n\
        \    └───────────────────────────────┘  arguments that have type ❰Natural❱       \n\
        \                     ⇧                                                          \n\
        \                     The function's input type                                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────┐                                           \n\
        \    │ λ(a : Type) → a : Type → Type │  This anonymous function only accepts     \n\
        \    └───────────────────────────────┘  arguments that have kind ❰Type❱          \n\
        \                        ⇧                                                       \n\
        \                        The function's input kind                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────┐                                                      \n\
        \    │ List : Type → Type │  This built-in function only accepts arguments that  \n\
        \    └────────────────────┘  have kind ❰Type❱                                    \n\
        \             ⇧                                                                  \n\
        \             The function's input kind                                          \n\
        \                                                                                \n\
        \                                                                                \n\
        \For example, the following expressions are valid:                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────┐                                                  \n\
        \    │ (λ(x : Bool) → x) True │  ❰True❱ has type ❰Bool❱, which matches the type  \n\
        \    └────────────────────────┘  of argument that the anonymous function accepts \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────┐                                                          \n\
        \    │ Natural/even 2 │  ❰2❱ has type ❰Natural❱, which matches the type of       \n\
        \    └────────────────┘  argument that the ❰Natural/even❱ function accepts,      \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────┐                                                  \n\
        \    │ (λ(a : Type) → a) Bool │  ❰Bool❱ has kind ❰Type❱, which matches the kind  \n\
        \    └────────────────────────┘  of argument that the anonymous function accepts \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────┐                                                               \n\
        \    │ List Text │  ❰Text❱ has kind ❰Type❱, which matches the kind of argument   \n\
        \    └───────────┘  that that the ❰List❱ function accepts                        \n\
        \                                                                                \n\
        \                                                                                \n\
        \However, you can " <> _NOT <> " apply a function to the wrong type or kind of argument\n\
        \                                                                                \n\
        \For example, the following expressions are not valid:                           \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────┐                                                   \n\
        \    │ (λ(x : Bool) → x) \"A\" │  ❰\"A\"❱ has type ❰Text❱, but the anonymous function\n\
        \    └───────────────────────┘  expects an argument that has type ❰Bool❱         \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────┐                                                        \n\
        \    │ Natural/even \"A\" │  ❰\"A\"❱ has type ❰Text❱, but the ❰Natural/even❱ function\n\
        \    └──────────────────┘  expects an argument that has type ❰Natural❱           \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────┐                                                  \n\
        \    │ (λ(a : Type) → a) True │  ❰True❱ has type ❰Bool❱, but the anonymous       \n\
        \    └────────────────────────┘  function expects an argument of kind ❰Type❱     \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────┐                                                                  \n\
        \    │ List 1 │  ❰1❱ has type ❰Natural❱, but the ❰List❱ function expects an      \n\
        \    └────────┘  argument that has kind ❰Type❱                                   \n\
        \                                                                                \n\
        \                                                                                \n\
        \Some common reasons why you might get this error:                               \n\
        \                                                                                \n\
        \● You omit a function argument by mistake:                                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────┐                                                   \n\
        \    │ List/head   [1, 2, 3] │                                                   \n\
        \    └───────────────────────┘                                                   \n\
        \                ⇧                                                               \n\
        \                ❰List/head❱ is missing the first argument,                      \n\
        \                which should be: ❰Natural❱                                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \● You supply an ❰Integer❱ literal to a function that expects a ❰Natural❱        \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────┐                                                         \n\
        \    │ Natural/even +2 │                                                         \n\
        \    └─────────────────┘                                                         \n\
        \                   ⇧                                                            \n\
        \                   This should be ❰2❱                                           \n\
        \                                                                                \n\
        \                                                                                \n\
        \────────────────────────────────────────────────────────────────────────────────\n\
        \                                                                                \n\
        \You tried to invoke the following function:                                     \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... which expects an argument of type or kind:                                  \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... on the following argument:                                                  \n\
        \                                                                                \n\
        \" <> txt2 <> "\n\
        \                                                                                \n\
        \... which has a different type or kind:                                         \n\
        \                                                                                \n\
        \" <> txt3 <> "\n"
      where
        txt0 = insert expr0
        txt1 = insert expr1
        txt2 = insert expr2
        txt3 = insert expr3

prettyTypeMessage (AnnotMismatch expr0 expr1 expr2) = ErrorMessages {..}
  where
    short = "Expression doesn't match annotation\n"
        <>  "\n"
        <>  Dhall.Diff.diffNormalized expr1 expr2
    long =
        "Explanation: You can annotate an expression with its type or kind using the     \n\
        \❰:❱ symbol, like this:                                                          \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────┐                                                                   \n\
        \    │ x : t │  ❰x❱ is an expression and ❰t❱ is the annotated type or kind of ❰x❱\n\
        \    └───────┘                                                                   \n\
        \                                                                                \n\
        \The type checker verifies that the expression's type or kind matches the        \n\
        \provided annotation                                                             \n\
        \                                                                                \n\
        \For example, all of the following are valid annotations that the type checker   \n\
        \accepts:                                                                        \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────┐                                                             \n\
        \    │ 1 : Natural │  ❰1❱ is an expression that has type ❰Natural❱, so the type  \n\
        \    └─────────────┘  checker accepts the annotation                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────┐                                                   \n\
        \    │ Natural/even 2 : Bool │  ❰Natural/even 2❱ has type ❰Bool❱, so the type    \n\
        \    └───────────────────────┘  checker accepts the annotation                   \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────┐                                                      \n\
        \    │ List : Type → Type │  ❰List❱ is an expression that has kind ❰Type → Type❱,\n\
        \    └────────────────────┘  so the type checker accepts the annotation          \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────┐                                                        \n\
        \    │ List Text : Type │  ❰List Text❱ is an expression that has kind ❰Type❱, so \n\
        \    └──────────────────┘  the type checker accepts the annotation               \n\
        \                                                                                \n\
        \                                                                                \n\
        \However, the following annotations are " <> _NOT <> " valid and the type checker will\n\
        \reject them:                                                                    \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────┐                                                                \n\
        \    │ 1 : Text │  The type checker rejects this because ❰1❱ does not have type  \n\
        \    └──────────┘  ❰Text❱                                                        \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────┐                                                             \n\
        \    │ List : Type │  ❰List❱ does not have kind ❰Type❱                           \n\
        \    └─────────────┘                                                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \Some common reasons why you might get this error:                               \n\
        \                                                                                \n\
        \● The Haskell Dhall interpreter implicitly inserts a top-level annotation       \n\
        \  matching the expected type                                                    \n\
        \                                                                                \n\
        \  For example, if you run the following Haskell code:                           \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────┐                                           \n\
        \    │ >>> input auto \"1\" :: IO Text │                                         \n\
        \    └───────────────────────────────┘                                           \n\
        \                                                                                \n\
        \                                                                                \n\
        \  ... then the interpreter will actually type check the following annotated     \n\
        \  expression:                                                                   \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────┐                                                                \n\
        \    │ 1 : Text │                                                                \n\
        \    └──────────┘                                                                \n\
        \                                                                                \n\
        \                                                                                \n\
        \  ... and then type-checking will fail                                          \n\
        \                                                                                \n\
        \────────────────────────────────────────────────────────────────────────────────\n\
        \                                                                                \n\
        \You or the interpreter annotated this expression:                               \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... with this type or kind:                                                     \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... but the inferred type or kind of the expression is actually:                \n\
        \                                                                                \n\
        \" <> txt2 <> "\n"
      where
        txt0 = insert expr0
        txt1 = insert expr1
        txt2 = insert expr2

prettyTypeMessage Untyped = ErrorMessages {..}
  where
    short = "❰Sort❱ has no type, kind, or sort"

    long =
        "Explanation: There are five levels of expressions that form a hierarchy:        \n\
        \                                                                                \n\
        \● terms                                                                         \n\
        \● types                                                                         \n\
        \● kinds                                                                         \n\
        \● sorts                                                                         \n\
        \● orders                                                                        \n\
        \                                                                                \n\
        \The following example illustrates this hierarchy:                               \n\
        \                                                                                \n\
        \    ┌───────────────────────────────────┐                                       \n\
        \    │ \"ABC\" : Text : Type : Kind : Sort │                                     \n\
        \    └───────────────────────────────────┘                                       \n\
        \       ⇧      ⇧      ⇧      ⇧      ⇧                                            \n\
        \       term   type   kind   sort   order                                        \n\
        \                                                                                \n\
        \There is nothing above ❰Sort❱ in this hierarchy, so if you try to type check any\n\
        \expression containing ❰Sort❱ anywhere in the expression then type checking fails\n\
        \                                                                                \n\
        \Some common reasons why you might get this error:                               \n\
        \                                                                                \n\
        \● You supplied a sort where a kind was expected                                 \n\
        \                                                                                \n\
        \  For example, the following expression will fail to type check:                \n\
        \                                                                                \n\
        \    ┌──────────────────┐                                                        \n\
        \    │ f : Type -> Kind │                                                        \n\
        \    └──────────────────┘                                                        \n\
        \                  ⇧                                                             \n\
        \                  ❰Kind❱ is a sort, not a kind                                  \n"

prettyTypeMessage (InvalidPredicate expr0 expr1) = ErrorMessages {..}
  where
    short = "Invalid predicate for ❰if❱"

    long =
        "Explanation: Every ❰if❱ expression begins with a predicate which must have type \n\
        \❰Bool❱                                                                          \n\
        \                                                                                \n\
        \For example, these are valid ❰if❱ expressions:                                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────────────┐                                            \n\
        \    │ if True then \"Yes\" else \"No\" │                                        \n\
        \    └──────────────────────────────┘                                            \n\
        \         ⇧                                                                      \n\
        \         Predicate                                                              \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────────┐                                 \n\
        \    │ λ(x : Bool) → if x then False else True │                                 \n\
        \    └─────────────────────────────────────────┘                                 \n\
        \                       ⇧                                                        \n\
        \                       Predicate                                                \n\
        \                                                                                \n\
        \                                                                                \n\
        \... but these are " <> _NOT <> " valid ❰if❱ expressions:                        \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────┐                                               \n\
        \    │ if 0 then \"Yes\" else \"No\" │  ❰0❱ does not have type ❰Bool❱            \n\
        \    └───────────────────────────┘                                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────────┐                                              \n\
        \    │ if \"\" then False else True │  ❰\"\"❱ does not have type ❰Bool❱          \n\
        \    └────────────────────────────┘                                              \n\
        \                                                                                \n\
        \                                                                                \n\
        \Some common reasons why you might get this error:                               \n\
        \                                                                                \n\
        \● You might be used to other programming languages that accept predicates other \n\
        \  than ❰Bool❱                                                                   \n\
        \                                                                                \n\
        \  For example, some languages permit ❰0❱ or ❰\"\"❱ as valid predicates and treat\n\
        \  them as equivalent to ❰False❱.  However, the Dhall language does not permit   \n\
        \  this                                                                          \n\
        \                                                                                \n\
        \────────────────────────────────────────────────────────────────────────────────\n\
        \                                                                                \n\
        \Your ❰if❱ expression begins with the following predicate:                       \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... that has type:                                                              \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... but the predicate must instead have type ❰Bool❱                             \n"
      where
        txt0 = insert expr0
        txt1 = insert expr1

prettyTypeMessage (IfBranchMustBeTerm b expr0 expr1 expr2) =
    ErrorMessages {..}
  where
    short = "❰if❱ branch cannot be a type"

    long =
        "Explanation: Every ❰if❱ expression has a ❰then❱ and ❰else❱ branch, each of which\n\
        \is an expression:                                                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \                   Expression for ❰then❱ branch                                 \n\
        \                   ⇩                                                            \n\
        \    ┌────────────────────────────────┐                                          \n\
        \    │ if True then \"Hello, world!\"   │                                        \n\
        \    │         else \"Goodbye, world!\" │                                        \n\
        \    └────────────────────────────────┘                                          \n\
        \                   ⇧                                                            \n\
        \                   Expression for ❰else❱ branch                                 \n\
        \                                                                                \n\
        \                                                                                \n\
        \These expressions must be a “term”, where a “term” is defined as an expression  \n\
        \that has a type thas has kind ❰Type❱                                            \n\
        \                                                                                \n\
        \For example, the following expressions are all valid “terms”:                   \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────┐                                                      \n\
        \    │ 1 : Natural : Type │  ❰1❱ is a term with a type (❰Natural❱) of kind ❰Type❱\n\
        \    └────────────────────┘                                                      \n\
        \      ⇧                                                                         \n\
        \      term                                                                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────┐                                     \n\
        \    │ Natural/odd : Natural → Bool : Type │  ❰Natural/odd❱ is a term with a type\n\
        \    └─────────────────────────────────────┘  (❰Natural → Bool❱) of kind ❰Type❱  \n\
        \      ⇧                                                                         \n\
        \      term                                                                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \However, the following expressions are " <> _NOT <> " valid terms:              \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────┐                                                      \n\
        \    │ Text : Type : Kind │  ❰Text❱ has kind (❰Type❱) of sort ❰Kind❱ and is      \n\
        \    └────────────────────┘  therefore not a term                                \n\
        \      ⇧                                                                         \n\
        \      type                                                                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────┐                                               \n\
        \    │ List : Type → Type : Kind │  ❰List❱ has kind (❰Type → Type❱) of sort      \n\
        \    └───────────────────────────┘  ❰Kind❱ and is therefore not a term           \n\
        \      ⇧                                                                         \n\
        \      type-level function                                                       \n\
        \                                                                                \n\
        \                                                                                \n\
        \This means that you cannot define an ❰if❱ expression that returns a type.  For  \n\
        \example, the following ❰if❱ expression is " <> _NOT <> " valid:                 \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────┐                                             \n\
        \    │ if True then Text else Bool │  Invalid ❰if❱ expression                    \n\
        \    └─────────────────────────────┘                                             \n\
        \                   ⇧         ⇧                                                  \n\
        \                   type      type                                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \Your ❰" <> txt0 <> "❱ branch of your ❰if❱ expression is:                        \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... which has kind:                                                             \n\
        \                                                                                \n\
        \" <> txt2 <> "\n\
        \                                                                                \n\
        \... of sort:                                                                    \n\
        \                                                                                \n\
        \" <> txt3 <> "\n\
        \                                                                                \n\
        \... and is not a term.  Therefore your ❰if❱ expression is not valid             \n"
      where
        txt0 = if b then "then" else "else"
        txt1 = insert expr0
        txt2 = insert expr1
        txt3 = insert expr2

prettyTypeMessage (IfBranchMismatch expr0 expr1 expr2 expr3) =
    ErrorMessages {..}
  where
    short = "❰if❱ branches must have matching types\n"
        <>  "\n"
        <>  Dhall.Diff.diffNormalized expr2 expr3

    long =
        "Explanation: Every ❰if❱ expression has a ❰then❱ and ❰else❱ branch, each of which\n\
        \is an expression:                                                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \                   Expression for ❰then❱ branch                                 \n\
        \                   ⇩                                                            \n\
        \    ┌────────────────────────────────┐                                          \n\
        \    │ if True then \"Hello, world!\"   │                                        \n\
        \    │         else \"Goodbye, world!\" │                                        \n\
        \    └────────────────────────────────┘                                          \n\
        \                   ⇧                                                            \n\
        \                   Expression for ❰else❱ branch                                 \n\
        \                                                                                \n\
        \                                                                                \n\
        \These two expressions must have the same type.  For example, the following ❰if❱ \n\
        \expressions are all valid:                                                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────────────────┐                                        \n\
        \    │ λ(b : Bool) → if b then 0 else 1 │ Both branches have type ❰Natural❱      \n\
        \    └──────────────────────────────────┘                                        \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────────┐                                              \n\
        \    │ λ(b : Bool) →              │                                              \n\
        \    │     if b then Natural/even │ Both branches have type ❰Natural → Bool❱     \n\
        \    │          else Natural/odd  │                                              \n\
        \    └────────────────────────────┘                                              \n\
        \                                                                                \n\
        \                                                                                \n\
        \However, the following expression is " <> _NOT <> " valid:                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \                   This branch has type ❰Natural❱                               \n\
        \                   ⇩                                                            \n\
        \    ┌────────────────────────┐                                                  \n\
        \    │ if True then 0         │                                                  \n\
        \    │         else \"ABC\"     │                                                \n\
        \    └────────────────────────┘                                                  \n\
        \                   ⇧                                                            \n\
        \                   This branch has type ❰Text❱                                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \The ❰then❱ and ❰else❱ branches must have matching types, even if the predicate  \n\
        \is always ❰True❱ or ❰False❱                                                     \n\
        \                                                                                \n\
        \Your ❰if❱ expression has the following ❰then❱ branch:                           \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... which has type:                                                             \n\
        \                                                                                \n\
        \" <> txt2 <> "\n\
        \                                                                                \n\
        \... and the following ❰else❱ branch:                                            \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... which has a different type:                                                 \n\
        \                                                                                \n\
        \" <> txt3 <> "\n\
        \                                                                                \n\
        \Fix your ❰then❱ and ❰else❱ branches to have matching types                      \n"
      where
        txt0 = insert expr0
        txt1 = insert expr1
        txt2 = insert expr2
        txt3 = insert expr3

prettyTypeMessage (InvalidListType expr0) = ErrorMessages {..}
  where
    short = "Invalid type for ❰List❱ elements"

    long =
        "Explanation: ❰List❱s can optionally document the type of their elements with a  \n\
        \type annotation, like this:                                                     \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────────┐                                                \n\
        \    │ [1, 2, 3] : List Natural │  A ❰List❱ of three ❰Natural❱ numbers           \n\
        \    └──────────────────────────┘                                                \n\
        \                       ⇧                                                        \n\
        \                       The type of the ❰List❱'s elements, which are ❰Natural❱   \n\
        \                       numbers                                                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────┐                                                       \n\
        \    │ [] : List Natural │  An empty ❰List❱                                      \n\
        \    └───────────────────┘                                                       \n\
        \                ⇧                                                               \n\
        \                You must specify the type when the ❰List❱ is empty              \n\
        \                                                                                \n\
        \                                                                                \n\
        \The element type must be a type and not something else.  For example, the       \n\
        \following element types are " <> _NOT <> " valid:                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────┐                                                            \n\
        \    │ ... : List 1 │                                                            \n\
        \    └──────────────┘                                                            \n\
        \                 ⇧                                                              \n\
        \                 This is a ❰Natural❱ number and not a ❰Type❱                    \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────┐                                                         \n\
        \    │ ... : List Type │                                                         \n\
        \    └─────────────────┘                                                         \n\
        \                 ⇧                                                              \n\
        \                 This is a ❰Kind❱ and not a ❰Type❱                              \n\
        \                                                                                \n\
        \                                                                                \n\
        \You declared that the ❰List❱'s elements should have type:                       \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... which is not a ❰Type❱                                                       \n"
      where
        txt0 = insert expr0

prettyTypeMessage MissingListType = do
    ErrorMessages {..}
  where
    short = "Empty list requires a type annotation"

    long =
        "Explanation: Lists never require a type annotation if they have at least one   \n\
        \element:                                                                        \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────┐                                                               \n\
        \    │ [1, 2, 3] │  The compiler can infer that this list has type ❰List Natural❱\n\
        \    └───────────┘                                                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \However, empty lists may require a type annotation, if the element type is not  \n\
        \inferable from program context:                                                 \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────┐                                                       \n\
        \    │ [] : List Natural │  This type annotation is mandatory                    \n\
        \    └───────────────────┘                                                       \n\
        \                                                                                \n\
        \                                                                                \n"

prettyTypeMessage (MismatchedListElements i expr0 _expr1 expr2) =
    ErrorMessages {..}
  where
    short = "List elements should all have the same type\n"
        <>  "\n"
        <>  Dhall.Diff.diffNormalized expr0 expr2

    long =
        "Explanation: Every element in a list must have the same type                    \n\
        \                                                                                \n\
        \For example, this is a valid ❰List❱:                                            \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────┐                                                               \n\
        \    │ [1, 2, 3] │  Every element in this ❰List❱ is a ❰Natural❱ number           \n\
        \    └───────────┘                                                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \.. but this is " <> _NOT <> " a valid ❰List❱:                                   \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────┐                                                           \n\
        \    │ [1, \"ABC\", 3] │  The first and second element have different types      \n\
        \    └───────────────┘                                                           \n\
        \                                                                                \n\
        \                                                                                \n\
        \Your first ❰List❱ element has this type:                                        \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... but the element at index #" <> txt1 <> " has this type instead:             \n\
        \                                                                                \n\
        \" <> txt3 <> "\n"
      where
        txt0 = insert expr0
        txt1 = pretty i
        txt3 = insert expr2

prettyTypeMessage (InvalidListElement i expr0 _expr1 expr2) =
    ErrorMessages {..}
  where
    short = "List element has the wrong type\n"
        <>  "\n"
        <>  Dhall.Diff.diffNormalized expr0 expr2

    long =
        "Explanation: Every element in the list must have a type matching the type       \n\
        \annotation at the end of the list                                               \n\
        \                                                                                \n\
        \For example, this is a valid ❰List❱:                                            \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────────┐                                                \n\
        \    │ [1, 2, 3] : List Natural │  Every element in this ❰List❱ is an ❰Natural❱  \n\
        \    └──────────────────────────┘                                                \n\
        \                                                                                \n\
        \                                                                                \n\
        \.. but this is " <> _NOT <> " a valid ❰List❱:                                   \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────────────┐                                            \n\
        \    │ [1, \"ABC\", 3] : List Natural │  The second element is not an ❰Natural❱  \n\
        \    └──────────────────────────────┘                                            \n\
        \                                                                                \n\
        \                                                                                \n\
        \Your ❰List❱ elements should have this type:                                     \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... but the element at index #" <> txt1 <> " has this type instead:             \n\
        \                                                                                \n\
        \" <> txt3 <> "\n"
      where
        txt0 = insert expr0
        txt1 = pretty i
        txt3 = insert expr2

prettyTypeMessage (InvalidOptionalType expr0) = ErrorMessages {..}
  where
    short = "Invalid type for ❰Optional❱ element"

    long =
        "Explanation: The legacy ❰List❱-like syntax for ❰Optional❱ literals ends with a  \n\
        \type annotation for the element that might be present, like this:               \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────┐                                                  \n\
        \    │ [1] : Optional Natural │  An optional element that's present              \n\
        \    └────────────────────────┘                                                  \n\
        \                     ⇧                                                          \n\
        \                     The type of the ❰Optional❱ element, which is an ❰Natural❱  \n\
        \                     number                                                     \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────┐                                                  \n\
        \    │ [] : Optional Natural  │  An optional element that's absent               \n\
        \    └────────────────────────┘                                                  \n\
        \                    ⇧                                                           \n\
        \                    You still specify the type even when the element is absent  \n\
        \                                                                                \n\
        \                                                                                \n\
        \The element type must be a type and not something else.  For example, the       \n\
        \following element types are " <> _NOT <> " valid:                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────┐                                                        \n\
        \    │ ... : Optional 1 │                                                        \n\
        \    └──────────────────┘                                                        \n\
        \                     ⇧                                                          \n\
        \                     This is a ❰Natural❱ number and not a ❰Type❱                \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────┐                                                     \n\
        \    │ ... : Optional Type │                                                     \n\
        \    └─────────────────────┘                                                     \n\
        \                     ⇧                                                          \n\
        \                     This is a ❰Kind❱ and not a ❰Type❱                          \n\
        \                                                                                \n\
        \                                                                                \n\
        \Even if the element is absent you still must specify a valid type               \n\
        \                                                                                \n\
        \You declared that the ❰Optional❱ element should have type:                      \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... which is not a ❰Type❱                                                       \n"
      where
        txt0 = insert expr0

prettyTypeMessage (InvalidOptionalElement expr0 expr1 expr2) = ErrorMessages {..}
  where
    short = "❰Optional❱ element has the wrong type\n"
        <>  "\n"
        <>  Dhall.Diff.diffNormalized expr0 expr2

    long =
        "Explanation: An ❰Optional❱ element must have a type matching the type annotation\n\
        \                                                                                \n\
        \For example, this is a valid ❰Optional❱ value:                                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────┐                                                  \n\
        \    │ [1] : Optional Natural │  ❰1❱ is a ❰Natural❱ number, which matches the    \n\
        \    └────────────────────────┘  number                                          \n\
        \                                                                                \n\
        \                                                                                \n\
        \... but this is " <> _NOT <> " a valid ❰Optional❱ value:                        \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────────┐                                              \n\
        \    │ [\"ABC\"] : Optional Natural │  ❰\"ABC\"❱ is not a ❰Natural❱ number       \n\
        \    └────────────────────────────┘                                              \n\
        \                                                                                \n\
        \                                                                                \n\
        \Your ❰Optional❱ element should have this type:                                  \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... but the element you provided:                                               \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... has this type instead:                                                      \n\
        \                                                                                \n\
        \" <> txt2 <> "\n"
      where
        txt0 = insert expr0
        txt1 = insert expr1
        txt2 = insert expr2

prettyTypeMessage (InvalidSome expr0 expr1 expr2) = ErrorMessages {..}
  where
    short = "❰Some❱ argument has the wrong type"

    long =
        "Explanation: The ❰Some❱ constructor expects an argument that is a term, where   \n\
        \the type of the type of a term must be ❰Type❱                                   \n\
        \                                                                                \n\
        \For example, this is a valid use of ❰Some❱:                                     \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────┐                                                                  \n\
        \    │ Some 1 │  ❰1❱ is a valid term because ❰1 : Natural : Type❱                \n\
        \    └────────┘                                                                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \... but this is " <> _NOT <> " a valid ❰Optional❱ value:                        \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────┐                                                               \n\
        \    │ Some Text │  ❰Text❱ is not a valid term because ❰Text : Type : Kind ❱     \n\
        \    └───────────┘                                                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \The ❰Some❱ argument you provided:                                               \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... has this type:                                                              \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... but the type of that type is:                                               \n\
        \                                                                                \n\
        \" <> txt2 <> "\n\
        \                                                                                \n\
        \... which is not ❰Type❱                                                         \n"
      where
        txt0 = insert expr0
        txt1 = insert expr1
        txt2 = insert expr2

prettyTypeMessage (FieldAnnotationMismatch k0 expr0 c0 k1 expr1 c1) = ErrorMessages {..}
  where
    short = "Field annotation mismatch"

    long =
        "Explanation: Every record type annotates each field with a ❰Type❱ or a ❰Kind❱,  \n\
        \like this:                                                                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────────────────────────────┐                            \n\
        \    │ { foo : Natural, bar : Integer, baz : Text } │  Every field is annotated  \n\
        \    └──────────────────────────────────────────────┘  with a ❰Type❱             \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────────┐                                              \n\
        \    │ { foo : Type, bar : Type } │  Every field is annotated                    \n\
        \    └────────────────────────────┘  with a ❰Kind❱                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \However, you cannot have a record type with both a ❰Type❱ and ❰Kind❱ annotation:\n\
        \                                                                                \n\
        \                                                                                \n\
        \              This is a ❰Type❱ annotation                                       \n\
        \              ⇩                                                                 \n\
        \    ┌───────────────────────────────┐                                           \n\
        \    │ { foo : Natural, bar : Type } │  Invalid record type                      \n\
        \    └───────────────────────────────┘                                           \n\
        \                             ⇧                                                  \n\
        \                             ... but this is a ❰Kind❱ annotation                \n\
        \                                                                                \n\
        \                                                                                \n\
        \You provided a record type with a field named:                                  \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... annotated with the following expression:                                    \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... which is a " <> level c1 <> " whereas another field named:                  \n\
        \                                                                                \n\
        \" <> txt2 <> "\n\
        \                                                                                \n\
        \... annotated with the following expression:                                    \n\
        \                                                                                \n\
        \" <> txt3 <> "\n\
        \                                                                                \n\
        \... is a " <> level c0 <> ", which does not match                               \n"
      where
        txt0 = insert k0
        txt1 = insert expr0
        txt2 = insert k1
        txt3 = insert expr1

        level Type = "❰Type❱"
        level Kind = "❰Kind❱"
        level Sort = "❰Sort❱"

prettyTypeMessage (FieldMismatch k0 expr0 c0 k1 expr1 c1) = ErrorMessages {..}
  where
    short = "Field kind mismatch"

    long =
        "Explanation: Every record has fields that can be either terms or types, like    \n\
        \this:                                                                           \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────────────────────┐                                    \n\
        \    │ { foo = 1, bar = True, baz = \"ABC\" } │  Every field is a term           \n\
        \    └──────────────────────────────────────┘                                    \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────┐                                           \n\
        \    │ { foo = Natural, bar = Text } │  Every field is a type                    \n\
        \    └───────────────────────────────┘                                           \n\
        \                                                                                \n\
        \                                                                                \n\
        \However, you cannot have a record that stores both terms and types:             \n\
        \                                                                                \n\
        \                                                                                \n\
        \              This is a term                                                    \n\
        \              ⇩                                                                 \n\
        \    ┌─────────────────────────┐                                                 \n\
        \    │ { foo = 1, bar = Bool } │  Invalid record                                 \n\
        \    └─────────────────────────┘                                                 \n\
        \                       ⇧                                                        \n\
        \                       ... but this is a type                                   \n\
        \                                                                                \n\
        \                                                                                \n\
        \You provided a record with a field named:                                       \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... whose value was:                                                            \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... which is a " <> level c1 <> " whereas another field named:                  \n\
        \                                                                                \n\
        \" <> txt2 <> "\n\
        \                                                                                \n\
        \... whose value was:                                                            \n\
        \                                                                                \n\
        \" <> txt3 <> "\n\
        \                                                                                \n\
        \... is a " <> level c0 <> ", which does not match                               \n"
      where
        txt0 = insert k0
        txt1 = insert expr0
        txt2 = insert k1
        txt3 = insert expr1

        level Type = "term"
        level Kind = "type"
        level Sort = "kind"

prettyTypeMessage (InvalidField k expr0) = ErrorMessages {..}
  where
    short = "Invalid field"

    long =
        "Explanation: Every record literal is a set of fields assigned to values, like   \n\
        \this:                                                                           \n\
        \                                                                                \n\
        \    ┌────────────────────────────────────────┐                                  \n\
        \    │ { foo = 100, bar = True, baz = \"ABC\" } │                                \n\
        \    └────────────────────────────────────────┘                                  \n\
        \                                                                                \n\
        \However, fields can only be terms and or ❰Type❱s and not ❰Kind❱s                \n\
        \                                                                                \n\
        \For example, the following record literal is " <> _NOT <> " valid:              \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────┐                                                          \n\
        \    │ { foo = Type } │                                                          \n\
        \    └────────────────┘                                                          \n\
        \              ⇧                                                                 \n\
        \              ❰Type❱ is a ❰Kind❱, which is not allowed                          \n\
        \                                                                                \n\
        \                                                                                \n\
        \You provided a record literal with a field named:                               \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... whose value is:                                                             \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... which is not a term or ❰Type❱                                               \n"
      where
        txt0 = insert k
        txt1 = insert expr0

prettyTypeMessage (InvalidAlternativeType k expr0) = ErrorMessages {..}
  where
    short = "Invalid alternative type"

    long =
        "Explanation: Every union type specifies the type of each alternative, like this:\n\
        \                                                                                \n\
        \                                                                                \n\
        \               The type of the first alternative is ❰Bool❱                      \n\
        \               ⇩                                                                \n\
        \    ┌──────────────────────────────────┐                                        \n\
        \    │ < Left : Bool, Right : Natural > │  A union type with two alternatives    \n\
        \    └──────────────────────────────────┘                                        \n\
        \                             ⇧                                                  \n\
        \                             The type of the second alternative is ❰Natural❱    \n\
        \                                                                                \n\
        \                                                                                \n\
        \However, these alternatives can only be annotated with ❰Type❱s, ❰Kind❱s, or     \n\
        \❰Sort❱s.  For example, the following union types are " <> _NOT <> " valid:      \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────────┐                                              \n\
        \    │ < Left : Bool, Right : 1 > │  Invalid union type                          \n\
        \    └────────────────────────────┘                                              \n\
        \                             ⇧                                                  \n\
        \                             This is a ❰Natural❱ and not a ❰Type❱, ❰Kind❱, or   \n\
        \                             ❰Sort❱                                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \Some common reasons why you might get this error:                               \n\
        \                                                                                \n\
        \● You accidentally typed ❰:❱ instead of ❰=❱ for a union literal with one        \n\
        \  alternative:                                                                  \n\
        \                                                                                \n\
        \    ┌─────────────────┐                                                         \n\
        \    │ < Example : 1 > │                                                         \n\
        \    └─────────────────┘                                                         \n\
        \                ⇧                                                               \n\
        \                This could be ❰=❱ instead                                       \n\
        \                                                                                \n\
        \                                                                                \n\
        \────────────────────────────────────────────────────────────────────────────────\n\
        \                                                                                \n\
        \You provided a union type with an alternative named:                            \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... annotated with the following expression which is not a ❰Type❱, ❰Kind❱, or   \n\
        \❰Sort❱:                                                                         \n\
        \                                                                                \n\
        \" <> txt1 <> "\n"
      where
        txt0 = insert k
        txt1 = insert expr0

prettyTypeMessage (InvalidAlternative k expr0) = ErrorMessages {..}
  where
    short = "Invalid alternative"

    long =
        "Explanation: Every union literal begins by selecting one alternative and        \n\
        \specifying the value for that alternative, like this:                           \n\
        \                                                                                \n\
        \                                                                                \n\
        \        Select the ❰Left❱ alternative, whose value is ❰True❱                    \n\
        \        ⇩                                                                       \n\
        \    ┌──────────────────────────────────┐                                        \n\
        \    │ < Left = True, Right : Natural > │  A union literal with two alternatives \n\
        \    └──────────────────────────────────┘                                        \n\
        \                                                                                \n\
        \                                                                                \n\
        \However, this value must be a term, type, or kind.  For example, the following  \n\
        \values are " <> _NOT <> " valid:                                                \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────┐                                                         \n\
        \    │ < Left = Sort > │  Invalid union literal                                  \n\
        \    └─────────────────┘                                                         \n\
        \               ⇧                                                                \n\
        \               This is not a term, type, or kind                                \n\
        \                                                                                \n\
        \                                                                                \n\
        \────────────────────────────────────────────────────────────────────────────────\n\
        \                                                                                \n\
        \You provided a union literal with an alternative named:                         \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... whose value is:                                                             \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... which is not a term, type, or kind.                                         \n"
      where
        txt0 = insert k
        txt1 = insert expr0

prettyTypeMessage (AlternativeAnnotationMismatch k0 expr0 c0 k1 expr1 c1) = ErrorMessages {..}
  where
    short = "Alternative annotation mismatch"

    long =
        "Explanation: Every union type annotates each alternative with a ❰Type❱ or a     \n\
        \❰Kind❱, like this:                                                              \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────────┐                                       \n\
        \    │ < Left : Natural | Right : Bool > │  Every alternative is annotated with a\n\
        \    └───────────────────────────────────┘  ❰Type❱                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────────────────┐                                      \n\
        \    │ < Foo : Type → Type | Bar : Type > │  Every alternative is annotated with \n\
        \    └────────────────────────────────────┘  a ❰Kind❱                            \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────┐                                                          \n\
        \    │ < Baz : Kind > │  Every alternative is annotated with a ❰Sort❱            \n\
        \    └────────────────┘                                                          \n\
        \                                                                                \n\
        \                                                                                \n\
        \However, you cannot have a union type that mixes ❰Type❱s, ❰Kind❱s, or ❰Sort❱s   \n\
        \for the annotations:                                                            \n\
        \                                                                                \n\
        \                                                                                \n\
        \              This is a ❰Type❱ annotation                                       \n\
        \              ⇩                                                                 \n\
        \    ┌───────────────────────────────┐                                           \n\
        \    │ { foo : Natural, bar : Type } │  Invalid union type                       \n\
        \    └───────────────────────────────┘                                           \n\
        \                             ⇧                                                  \n\
        \                             ... but this is a ❰Kind❱ annotation                \n\
        \                                                                                \n\
        \                                                                                \n\
        \You provided a union type with an alternative named:                            \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... annotated with the following expression:                                    \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... which is a " <> level c0 <> " whereas another alternative named:            \n\
        \                                                                                \n\
        \" <> txt2 <> "\n\
        \                                                                                \n\
        \... annotated with the following expression:                                    \n\
        \                                                                                \n\
        \" <> txt3 <> "\n\
        \                                                                                \n\
        \... is a " <> level c1 <> ", which does not match                               \n"
      where
        txt0 = insert k0
        txt1 = insert expr0
        txt2 = insert k1
        txt3 = insert expr1

        level Type = "❰Type❱"
        level Kind = "❰Kind❱"
        level Sort = "❰Sort❱"

prettyTypeMessage (ListAppendMismatch expr0 expr1) = ErrorMessages {..}
  where
    short = "You can only append ❰List❱s with matching element types\n"
        <>  "\n"
        <>  Dhall.Diff.diffNormalized expr0 expr1

    long =
        "Explanation: You can append two ❰List❱s using the ❰#❱ operator, like this:      \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────┐                                                      \n\
        \    │ [1, 2, 3] # [4, 5] │                                                      \n\
        \    └────────────────────┘                                                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \... but you cannot append two ❰List❱s if they have different element types.     \n\
        \For example, the following expression is " <> _NOT <> " valid:                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \       These elements have type ❰Natural❱                                       \n\
        \       ⇩                                                                        \n\
        \    ┌───────────────────────────┐                                               \n\
        \    │ [1, 2, 3] # [True, False] │  Invalid: the element types don't match       \n\
        \    └───────────────────────────┘                                               \n\
        \                  ⇧                                                             \n\
        \                  These elements have type ❰Bool❱                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \────────────────────────────────────────────────────────────────────────────────\n\
        \                                                                                \n\
        \You tried to append a ❰List❱ thas has elements of type:                         \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... with another ❰List❱ that has elements of type:                              \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... and those two types do not match                                            \n"
      where
        txt0 = insert expr0
        txt1 = insert expr1

prettyTypeMessage (DuplicateAlternative k) = ErrorMessages {..}
  where
    short = "Duplicate union alternative"

    long =
        "Explanation: Unions may not have two alternatives that share the same name      \n\
        \                                                                                \n\
        \For example, the following expressions are " <> _NOT <> " valid:                \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────┐                                             \n\
        \    │ < foo = True | foo : Text > │  Invalid: ❰foo❱ appears twice               \n\
        \    └─────────────────────────────┘                                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────────────┐                                   \n\
        \    │ < foo = 1 | bar : Bool | bar : Text > │  Invalid: ❰bar❱ appears twice     \n\
        \    └───────────────────────────────────────┘                                   \n\
        \                                                                                \n\
        \                                                                                \n\
        \You have more than one alternative named:                                       \n\
        \                                                                                \n\
        \" <> txt0 <> "\n"
      where
        txt0 = insert k

prettyTypeMessage (MustCombineARecord c expr0 expr1) = ErrorMessages {..}
  where
    short = "You can only combine records"

    long =
        "Explanation: You can combine records using the ❰" <> op <> "❱ operator, like this:\n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────────────────┐                               \n\
        \    │ { foo = 1, bar = \"ABC\" } " <> op <> " { baz = True } │                  \n\
        \    └───────────────────────────────────────────┘                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────────────┐                             \n\
        \    │ λ(r : { foo : Bool }) → r " <> op <> " { bar = \"ABC\" } │                \n\
        \    └─────────────────────────────────────────────┘                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \... but you cannot combine values that are not records.                         \n\
        \                                                                                \n\
        \For example, the following expressions are " <> _NOT <> " valid:                \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────────────┐                                            \n\
        \    │ { foo = 1, bar = \"ABC\" } " <> op <> " 1 │                               \n\
        \    └──────────────────────────────┘                                            \n\
        \                                 ⇧                                              \n\
        \                                 Invalid: Not a record                          \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────────────────┐                               \n\
        \    │ { foo = 1, bar = \"ABC\" } " <> op <> " { baz : Bool } │                  \n\
        \    └───────────────────────────────────────────┘                               \n\
        \                                 ⇧                                              \n\
        \                                 Invalid: This is a record type and not a record\n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────────────────┐                               \n\
        \    │ { foo = 1, bar = \"ABC\" } " <> op <> " < baz = True > │                  \n\
        \    └───────────────────────────────────────────┘                               \n\
        \                                 ⇧                                              \n\
        \                                 Invalid: This is a union and not a record      \n\
        \                                                                                \n\
        \                                                                                \n\
        \You tried to combine the following value:                                       \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... which is not a record, but is actually a:                                   \n\
        \                                                                                \n\
        \" <> txt1 <> "\n"
      where
        op   = pretty c
        txt0 = insert expr0
        txt1 = insert expr1

prettyTypeMessage (RecordMismatch c expr0 expr1 const0 const1) = ErrorMessages {..}
  where
    short = "Record mismatch"

    long =
        "Explanation: You can only use the ❰" <> op <> "❱ operator to combine records if they both  \n\
        \store terms or both store types.                                                \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────────────┐                                            \n\
        \    │ { foo = 1 } " <> op <> " { baz = True } │  Valid: Both records store terms           \n\
        \    └──────────────────────────────┘                                            \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────┐                                         \n\
        \    │ { foo = Bool } " <> op <> " { bar = Text } │  Valid: Both records store types        \n\
        \    └─────────────────────────────────┘                                         \n\
        \                                                                                \n\
        \                                                                                \n\
        \... but you cannot combine two records if one of the records stores types and   \n\
        \the other record stores terms.                                                  \n\
        \                                                                                \n\
        \For example, the following expression is " <> _NOT <> " valid:                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \      Record of terms                                                           \n\
        \      ⇩                                                                         \n\
        \    ┌──────────────────────────────┐                                            \n\
        \    │ { foo = 1 } " <> op <> " { bar = Text } │  Invalid: Mixing terms and types           \n\
        \    └──────────────────────────────┘                                            \n\
        \                               ⇧                                                \n\
        \                               Record of types                                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \You tried to combine the following record:                                      \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... which stores " <> class0 <> ", with the following record:                   \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... which stores " <> class1 <> ".                                              \n"
      where
        op   = pretty c

        txt0 = insert expr0
        txt1 = insert expr1

        toClass Type = "terms"
        toClass Kind = "types"
        toClass Sort = "kinds"

        class0 = toClass const0
        class1 = toClass const1

prettyTypeMessage (CombineTypesRequiresRecordType expr0 expr1) =
    ErrorMessages {..}
  where
    short = "❰⩓❱ requires arguments that are record types"

    long =
        "Explanation: You can only use the ❰⩓❱ operator on arguments that are record type\n\
        \literals, like this:                                                            \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────┐                                     \n\
        \    │ { age : Natural } ⩓ { name : Text } │                                     \n\
        \    └─────────────────────────────────────┘                                     \n\
        \                                                                                \n\
        \                                                                                \n\
        \... but you cannot use the ❰⩓❱ operator on any other type of arguments.  For    \n\
        \example, you cannot use variable arguments:                                     \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────────┐                                       \n\
        \    │ λ(t : Type) → t ⩓ { name : Text } │  Invalid: ❰t❱ might not be a record   \n\
        \    └───────────────────────────────────┘  type                                 \n\
        \                                                                                \n\
        \                                                                                \n\
        \────────────────────────────────────────────────────────────────────────────────\n\
        \                                                                                \n\
        \You tried to supply the following argument:                                     \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... which normalized to:                                                        \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... which is not a record type literal                                          \n"
      where
        txt0 = insert expr0
        txt1 = insert expr1

prettyTypeMessage (RecordTypeMismatch const0 const1 expr0 expr1) =
    ErrorMessages {..}
  where
    short = "Record type mismatch"

    long =
        "Explanation: You can only use the ❰⩓❱ operator on record types if they are both \n\
        \ ❰Type❱s or ❰Kind❱s:                                                            \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────┐                                     \n\
        \    │ { age : Natural } ⩓ { name : Text } │  Valid: Both arguments are ❰Type❱s  \n\
        \    └─────────────────────────────────────┘                                     \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────────────────────┐                                    \n\
        \    │ { Input : Type } ⩓ { Output : Type } │  Valid: Both arguments are ❰Kind❱s \n\
        \    └──────────────────────────────────────┘                                    \n\
        \                                                                                \n\
        \                                                                                \n\
        \... but you cannot combine a ❰Type❱ and a ❰Kind❱:                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────────────────┐                                      \n\
        \    │ { Input : Type } ⩓ { name : Text } │  Invalid: The arguments do not match \n\
        \    └────────────────────────────────────┘                                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \────────────────────────────────────────────────────────────────────────────────\n\
        \                                                                                \n\
        \You tried to combine the following record type:                                 \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... with this record types:                                                     \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... but the former record type is a:                                            \n\
        \                                                                                \n\
        \" <> txt2 <> "\n\
        \                                                                                \n\
        \... but the latter record type is a:                                            \n\
        \                                                                                \n\
        \" <> txt3 <> "\n"
      where
        txt0 = insert expr0
        txt1 = insert expr1
        txt2 = insert const0
        txt3 = insert const1

prettyTypeMessage (FieldCollision k) = ErrorMessages {..}
  where
    short = "Field collision"

    long =
        "Explanation: You can combine records or record types if they don't share any    \n\
        \fields in common, like this:                                                    \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────────────────┐                               \n\
        \    │ { foo = 1, bar = \"ABC\" } ∧ { baz = True } │                             \n\
        \    └───────────────────────────────────────────┘                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────┐                                         \n\
        \    │ { foo : Text } ⩓ { bar : Bool } │                                         \n\
        \    └─────────────────────────────────┘                                         \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────────────────────┐                                  \n\
        \    │ λ(r : { baz : Bool}) → { foo = 1 } ∧ r │                                  \n\
        \    └────────────────────────────────────────┘                                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \... but you cannot merge two records that share the same field unless the field \n\
        \is a record on both sides.                                                      \n\
        \                                                                                \n\
        \For example, the following expressions are " <> _NOT <> " valid:                \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────────────────┐                               \n\
        \    │ { foo = 1, bar = \"ABC\" } ∧ { foo = True } │  Invalid: Colliding ❰foo❱   \n\
        \    └───────────────────────────────────────────┘  fields                       \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────┐                                         \n\
        \    │ { foo : Bool } ∧ { foo : Text } │  Invalid: Colliding ❰foo❱ fields        \n\
        \    └─────────────────────────────────┘                                         \n\
        \                                                                                \n\
        \                                                                                \n\
        \... but the following expressions are valid:                                    \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────────────────────────────────┐                        \n\
        \    │ { foo = { bar = True } } ∧ { foo = { baz = 1 } } │  Valid: Both ❰foo❱     \n\
        \    └──────────────────────────────────────────────────┘  fields are records    \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────────────────────┐                     \n\
        \    │ { foo : { bar : Bool } } ⩓ { foo : { baz : Text } } │  Valid: Both ❰foo❱  \n\
        \    └─────────────────────────────────────────────────────┘  fields are records \n\
        \                                                                                \n\
        \                                                                                \n\
        \Some common reasons why you might get this error:                               \n\
        \                                                                                \n\
        \● You tried to use ❰∧❱ to update a field's value, like this:                    \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────────────────────┐                                  \n\
        \    │ { foo = 1, bar = \"ABC\" } ∧ { foo = 2 } │                                \n\
        \    └────────────────────────────────────────┘                                  \n\
        \                                   ⇧                                            \n\
        \                                  Invalid attempt to update ❰foo❱'s value to ❰2❱\n\
        \                                                                                \n\
        \  Field updates are intentionally not allowed as the Dhall language discourages \n\
        \  patch-oriented programming                                                    \n\
        \                                                                                \n\
        \────────────────────────────────────────────────────────────────────────────────\n\
        \                                                                                \n\
        \You combined two records that share the following field:                        \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... which is not allowed                                                        \n"
      where
        txt0 = insert k

prettyTypeMessage (MustMergeARecord expr0 expr1) = ErrorMessages {..}
  where
    short = "❰merge❱ expects a record of handlers"

    long =
        "Explanation: You can ❰merge❱ the alternatives of a union using a record with one\n\
        \handler per alternative, like this:                                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────────────────────────────────────┐     \n\
        \    │     let union    = < Left = 2 | Right : Bool >                      │     \n\
        \    │ in  let handlers = { Left = Natural/even, Right = λ(x : Bool) → x } │     \n\
        \    │ in  merge handlers union : Bool                                     │     \n\
        \    └─────────────────────────────────────────────────────────────────────┘     \n\
        \                                                                                \n\
        \                                                                                \n\
        \... but the first argument to ❰merge❱ must be a record and not some other type. \n\
        \                                                                                \n\
        \For example, the following expression is " <> _NOT <> " valid:                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────────┐                                 \n\
        \    │ let handler = λ(x : Bool) → x           │                                 \n\
        \    │ in  merge handler < Foo = True > : True │                                 \n\
        \    └─────────────────────────────────────────┘                                 \n\
        \                ⇧                                                               \n\
        \                Invalid: ❰handler❱ isn't a record                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \Some common reasons why you might get this error:                               \n\
        \                                                                                \n\
        \● You accidentally provide an empty record type instead of an empty record when \n\
        \  you ❰merge❱ an empty union:                                                   \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────────────────────────┐                                \n\
        \    │ λ(x : <>) → λ(a : Type) → merge {} x : a │                                \n\
        \    └──────────────────────────────────────────┘                                \n\
        \                                      ⇧                                         \n\
        \                                      This should be ❰{=}❱ instead              \n\
        \                                                                                \n\
        \                                                                                \n\
        \────────────────────────────────────────────────────────────────────────────────\n\
        \                                                                                \n\
        \You provided the following handler:                                             \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... which is not a record, but is actually a value of type:                     \n\
        \                                                                                \n\
        \" <> txt1 <> "\n"
      where
        txt0 = insert expr0
        txt1 = insert expr1

prettyTypeMessage (MustMergeUnion expr0 expr1) = ErrorMessages {..}
  where
    short = "❰merge❱ expects a union"

    long =
        "Explanation: You can ❰merge❱ the alternatives of a union using a record with one\n\
        \handler per alternative, like this:                                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────────────────────────────────────┐     \n\
        \    │     let union    = < Left = 2 | Right : Bool >                      │     \n\
        \    │ in  let handlers = { Left = Natural/even, Right = λ(x : Bool) → x } │     \n\
        \    │ in  merge handlers union : Bool                                     │     \n\
        \    └─────────────────────────────────────────────────────────────────────┘     \n\
        \                                                                                \n\
        \                                                                                \n\
        \... but the second argument to ❰merge❱ must be a union and not some other type. \n\
        \                                                                                \n\
        \For example, the following expression is " <> _NOT <> " valid:                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────────────────────────┐                                \n\
        \    │ let handlers = { Foo = λ(x : Bool) → x } │                                \n\
        \    │ in  merge handlers True : True           │                                \n\
        \    └──────────────────────────────────────────┘                                \n\
        \                         ⇧                                                      \n\
        \                         Invalid: ❰True❱ isn't a union                          \n\
        \                                                                                \n\
        \                                                                                \n\
        \You tried to ❰merge❱ this expression:                                           \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... which is not a union, but is actually a value of type:                      \n\
        \                                                                                \n\
        \" <> txt1 <> "\n"
      where
        txt0 = insert expr0
        txt1 = insert expr1

prettyTypeMessage (UnusedHandler ks) = ErrorMessages {..}
  where
    short = "Unused handler"

    long =
        "Explanation: You can ❰merge❱ the alternatives of a union using a record with one\n\
        \handler per alternative, like this:                                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────────────────────────────────────┐     \n\
        \    │     let union    = < Left = 2 | Right : Bool >                      │     \n\
        \    │ in  let handlers = { Left = Natural/even, Right = λ(x : Bool) → x } │     \n\
        \    │ in  merge handlers union : Bool                                     │     \n\
        \    └─────────────────────────────────────────────────────────────────────┘     \n\
        \                                                                                \n\
        \                                                                                \n\
        \... but you must provide exactly one handler per alternative in the union.  You \n\
        \cannot supply extra handlers                                                    \n\
        \                                                                                \n\
        \For example, the following expression is " <> _NOT <> " valid:                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────────────┐                                   \n\
        \    │     let union    = < Left = 2 >       │  The ❰Right❱ alternative is       \n\
        \    │ in  let handlers =                    │  missing                          \n\
        \    │             { Left  = Natural/even    │                                   \n\
        \    │             , Right = λ(x : Bool) → x │  Invalid: ❰Right❱ handler isn't   \n\
        \    │             }                         │           used                    \n\
        \    │ in  merge handlers union : Bool       │                                   \n\
        \    └───────────────────────────────────────┘                                   \n\
        \                                                                                \n\
        \                                                                                \n\
        \You provided the following handlers:                                            \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... which had no matching alternatives in the union you tried to ❰merge❱        \n"
      where
        txt0 = insert (Text.intercalate ", " (Data.Set.toList ks))

prettyTypeMessage (MissingHandler ks) = ErrorMessages {..}
  where
    short = "Missing handler"

    long =
        "Explanation: You can ❰merge❱ the alternatives of a union using a record with one\n\
        \handler per alternative, like this:                                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────────────────────────────────────┐     \n\
        \    │     let union    = < Left = 2 | Right : Bool >                      │     \n\
        \    │ in  let handlers = { Left = Natural/even, Right = λ(x : Bool) → x } │     \n\
        \    │ in  merge handlers union : Bool                                     │     \n\
        \    └─────────────────────────────────────────────────────────────────────┘     \n\
        \                                                                                \n\
        \                                                                                \n\
        \... but you must provide exactly one handler per alternative in the union.  You \n\
        \cannot omit any handlers                                                        \n\
        \                                                                                \n\
        \For example, the following expression is " <> _NOT <> " valid:                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \                                              Invalid: Missing ❰Right❱ handler  \n\
        \                                              ⇩                                 \n\
        \    ┌─────────────────────────────────────────────────┐                         \n\
        \    │     let handlers = { Left = Natural/even }      │                         \n\
        \    │ in  let union    = < Left = 2 | Right : Bool >  │                         \n\
        \    │ in  merge handlers union : Bool                 │                         \n\
        \    └─────────────────────────────────────────────────┘                         \n\
        \                                                                                \n\
        \                                                                                \n\
        \Note that you need to provide handlers for other alternatives even if those     \n\
        \alternatives are never used                                                     \n\
        \                                                                                \n\
        \You need to supply the following handlers:                                      \n\
        \                                                                                \n\
        \" <> txt0 <> "\n"
      where
        txt0 = insert (Text.intercalate ", " (Data.Set.toList ks))

prettyTypeMessage MissingMergeType =
    ErrorMessages {..}
  where
    short = "Empty ❰merge❱ requires a type annotation"

    long =
        "Explanation: A ❰merge❱ does not require a type annotation if the union has at   \n\
        \least one alternative, like this                                                \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────────────────────────────────────┐     \n\
        \    │     let union    = < Left = 2 | Right : Bool >                      │     \n\
        \    │ in  let handlers = { Left = Natural/even, Right = λ(x : Bool) → x } │     \n\
        \    │ in  merge handlers union                                            │     \n\
        \    └─────────────────────────────────────────────────────────────────────┘     \n\
        \                                                                                \n\
        \                                                                                \n\
        \However, you may need to provide a type annotation when merging an empty union  \n\
        \if the return type is not inferable from context:                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────────────┐                                          \n\
        \    │ λ(a : <>) → merge {=} a : Bool │                                          \n\
        \    └────────────────────────────────┘                                          \n\
        \                                ⇧                                               \n\
        \                                This can be any type                            \n\
        \                                                                                \n\
        \                                                                                \n\
        \You can provide any type at all as the annotation, since merging an empty       \n\
        \union can produce output of any type                                            \n"

prettyTypeMessage (HandlerInputTypeMismatch expr0 expr1 expr2) =
    ErrorMessages {..}
  where
    short = "Wrong handler input type\n"
        <>  "\n"
        <>  Dhall.Diff.diffNormalized expr1 expr2

    long =
        "Explanation: You can ❰merge❱ the alternatives of a union using a record with one\n\
        \handler per alternative, like this:                                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────────────────────────────────────┐     \n\
        \    │     let union    = < Left = 2 | Right : Bool >                      │     \n\
        \    │ in  let handlers = { Left = Natural/even, Right = λ(x : Bool) → x } │     \n\
        \    │ in  merge handlers union : Bool                                     │     \n\
        \    └─────────────────────────────────────────────────────────────────────┘     \n\
        \                                                                                \n\
        \                                                                                \n\
        \... as long as the input type of each handler function matches the type of the  \n\
        \corresponding alternative:                                                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────────────────────────────────┐               \n\
        \    │ union    : < Left : Natural       | Right : Bool        > │               \n\
        \    └───────────────────────────────────────────────────────────┘               \n\
        \                          ⇧                       ⇧                             \n\
        \                   These must match        These must match                     \n\
        \                          ⇩                       ⇩                             \n\
        \    ┌───────────────────────────────────────────────────────────┐               \n\
        \    │ handlers : { Left : Natural → Bool, Right : Bool → Bool } │               \n\
        \    └───────────────────────────────────────────────────────────┘               \n\
        \                                                                                \n\
        \                                                                                \n\
        \For example, the following expression is " <> _NOT <> " valid:                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \      Invalid: Doesn't match the type of the ❰Right❱ alternative                \n\
        \                                                               ⇩                \n\
        \    ┌──────────────────────────────────────────────────────────────────────┐    \n\
        \    │     let handlers = { Left = Natural/even | Right = λ(x : Text) → x } │    \n\
        \    │ in  let union    = < Left = 2 | Right : Bool >                       │    \n\
        \    │ in  merge handlers union : Bool                                      │    \n\
        \    └──────────────────────────────────────────────────────────────────────┘    \n\
        \                                                                                \n\
        \                                                                                \n\
        \Your handler for the following alternative:                                     \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... needs to accept an input value of type:                                     \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... but actually accepts an input value of a different type:                    \n\
        \                                                                                \n\
        \" <> txt2 <> "\n"
      where
        txt0 = insert expr0
        txt1 = insert expr1
        txt2 = insert expr2

prettyTypeMessage (InvalidHandlerOutputType expr0 expr1 expr2) =
    ErrorMessages {..}
  where
    short = "Wrong handler output type\n"
        <>  "\n"
        <>  Dhall.Diff.diffNormalized expr1 expr2

    long =
        "Explanation: You can ❰merge❱ the alternatives of a union using a record with one\n\
        \handler per alternative, like this:                                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────────────────────────────────────┐     \n\
        \    │     let union    = < Left = 2 | Right : Bool >                      │     \n\
        \    │ in  let handlers = { Left = Natural/even, Right = λ(x : Bool) → x } │     \n\
        \    │ in  merge handlers union : Bool                                     │     \n\
        \    └─────────────────────────────────────────────────────────────────────┘     \n\
        \                                                                                \n\
        \                                                                                \n\
        \... as long as the output type of each handler function matches the declared    \n\
        \type of the result:                                                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────────────────────────────────┐               \n\
        \    │ handlers : { Left : Natural → Bool, Right : Bool → Bool } │               \n\
        \    └───────────────────────────────────────────────────────────┘               \n\
        \                                    ⇧                    ⇧                      \n\
        \                                    These output types ...                      \n\
        \                                                                                \n\
        \                             ... must match the declared type of the ❰merge❱    \n\
        \                             ⇩                                                  \n\
        \    ┌─────────────────────────────┐                                             \n\
        \    │ merge handlers union : Bool │                                             \n\
        \    └─────────────────────────────┘                                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \For example, the following expression is " <> _NOT <> " valid:                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────────────────────────────────────────────────────┐    \n\
        \    │     let union    = < Left = 2 | Right : Bool >                       │    \n\
        \    │ in  let handlers = { Left = Natural/even, Right = λ(x : Bool) → x }  │    \n\
        \    │ in  merge handlers union : Text                                      │    \n\
        \    └──────────────────────────────────────────────────────────────────────┘    \n\
        \                                 ⇧                                              \n\
        \                                 Invalid: Doesn't match output of either handler\n\
        \                                                                                \n\
        \                                                                                \n\
        \Your handler for the following alternative:                                     \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... needs to return an output value of type:                                    \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... but actually returns an output value of a different type:                   \n\
        \                                                                                \n\
        \" <> txt2 <> "\n"
      where
        txt0 = insert expr0
        txt1 = insert expr1
        txt2 = insert expr2

prettyTypeMessage (HandlerOutputTypeMismatch key0 expr0 key1 expr1) =
    ErrorMessages {..}
  where
    short = "Handlers should have the same output type\n"
        <>  "\n"
        <>  Dhall.Diff.diffNormalized expr0 expr1

    long =
        "Explanation: You can ❰merge❱ the alternatives of a union using a record with one\n\
        \handler per alternative, like this:                                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────────────────────────────────────┐     \n\
        \    │     let union    = < Left = 2 | Right : Bool >                      │     \n\
        \    │ in  let handlers = { Left = Natural/even, Right = λ(x : Bool) → x } │     \n\
        \    │ in  merge handlers union                                            │     \n\
        \    └─────────────────────────────────────────────────────────────────────┘     \n\
        \                                                                                \n\
        \                                                                                \n\
        \... as long as the output type of each handler function is the same:            \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────────────────────────────────┐               \n\
        \    │ handlers : { Left : Natural → Bool, Right : Bool → Bool } │               \n\
        \    └───────────────────────────────────────────────────────────┘               \n\
        \                                    ⇧                    ⇧                      \n\
        \                                These output types both match                   \n\
        \                                                                                \n\
        \                                                                                \n\
        \For example, the following expression is " <> _NOT <> " valid:                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────────────────┐                         \n\
        \    │     let union    = < Left = 2 | Right : Bool >  │                         \n\
        \    │ in  let handlers =                              │                         \n\
        \    │              { Left  = λ(x : Natural) → x       │  This outputs ❰Natural❱ \n\
        \    │              , Right = λ(x : Bool   ) → x       │  This outputs ❰Bool❱    \n\
        \    │              }                                  │                         \n\
        \    │ in  merge handlers union                        │                         \n\
        \    └─────────────────────────────────────────────────┘                         \n\
        \                ⇧                                                               \n\
        \                Invalid: The handlers in this record don't have matching outputs\n\
        \                                                                                \n\
        \                                                                                \n\
        \The handler for the ❰" <> txt0 <> "❱ alternative has this output type:          \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... but the handler for the ❰" <> txt2 <> "❱ alternative has this output type instead:\n\
        \                                                                                \n\
        \" <> txt3 <> "\n"
      where
        txt0 = pretty key0
        txt1 = insert expr0
        txt2 = pretty key1
        txt3 = insert expr1

prettyTypeMessage (HandlerNotAFunction k expr0) = ErrorMessages {..}
  where
    short = "Handler is not a function"

    long =
        "Explanation: You can ❰merge❱ the alternatives of a union using a record with one\n\
        \handler per alternative, like this:                                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────────────────────────────────────┐     \n\
        \    │     let union    = < Left = 2 | Right : Bool >                      │     \n\
        \    │ in  let handlers = { Left = Natural/even, Right = λ(x : Bool) → x } │     \n\
        \    │ in  merge handlers union : Bool                                     │     \n\
        \    └─────────────────────────────────────────────────────────────────────┘     \n\
        \                                                                                \n\
        \                                                                                \n\
        \... as long as each handler is a function                                       \n\
        \                                                                                \n\
        \For example, the following expression is " <> _NOT <> " valid:                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────────┐                                 \n\
        \    │ merge { Foo = True } < Foo = 1 > : Bool │                                 \n\
        \    └─────────────────────────────────────────┘                                 \n\
        \                    ⇧                                                           \n\
        \                    Invalid: Not a function                                     \n\
        \                                                                                \n\
        \                                                                                \n\
        \Your handler for this alternative:                                              \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... has the following type:                                                     \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... which is not the type of a function                                         \n"
      where
        txt0 = insert k
        txt1 = insert expr0

prettyTypeMessage (CantAccess lazyText0 expr0 expr1) = ErrorMessages {..}
  where
    short = "Not a record or a union"

    long =
        "Explanation: You can only access fields on records or unions, like this:        \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────────┐                                       \n\
        \    │ { foo = True, bar = \"ABC\" }.foo │  This is valid ...                    \n\
        \    └───────────────────────────────────┘                                       \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────────────────┐                               \n\
        \    │ λ(r : { foo : Bool, bar : Text }) → r.foo │  ... and so is this           \n\
        \    └───────────────────────────────────────────┘                               \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────┐                                         \n\
        \    │ < foo : Bool | bar : Text >.foo │  ... and so is this                     \n\
        \    └─────────────────────────────────┘                                         \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────────────────────────┐                              \n\
        \    │ λ(r : < foo : Bool | bar : Text >) → r.foo │  ... and so is this          \n\
        \    └────────────────────────────────────────────┘                              \n\
        \                                                                                \n\
        \                                                                                \n\
        \... but you cannot access fields on non-record expressions                      \n\
        \                                                                                \n\
        \For example, the following expression is " <> _NOT <> " valid:                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────┐                                                                   \n\
        \    │ 1.foo │                                                                   \n\
        \    └───────┘                                                                   \n\
        \      ⇧                                                                         \n\
        \      Invalid: Not a record                                                     \n\
        \                                                                                \n\
        \                                                                                \n\
        \────────────────────────────────────────────────────────────────────────────────\n\
        \                                                                                \n\
        \You tried to access the field:                                                  \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... on the following expression which is not a record nor a union type:         \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... but is actually an expression of type:                                      \n\
        \                                                                                \n\
        \" <> txt2 <> "\n"
      where
        txt0 = insert lazyText0
        txt1 = insert expr0
        txt2 = insert expr1

prettyTypeMessage (CantProject lazyText0 expr0 expr1) = ErrorMessages {..}
  where
    short = "Expected a record for projection"

    long =
        "Explanation: You can only project fields on records, like this:                 \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────────────────────┐                     \n\
        \    │ { foo = True, bar = \"ABC\", baz = 1 }.{ foo, bar } │  This is valid ...  \n\
        \    └─────────────────────────────────────────────────────┘                     \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────────────────────────────────────────────────┐      \n\
        \    │ λ(r : { foo : Bool, bar : Text , baz : Natural }) → r.{ foo, bar } │  ... and so is this           \n\
        \    └────────────────────────────────────────────────────────────────────┘      \n\
        \                                                                                \n\
        \                                                                                \n\
        \... but you cannot project fields on non-record expressions                     \n\
        \                                                                                \n\
        \For example, the following expression is " <> _NOT <> " valid:                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────┐                                                          \n\
        \    │ 1.{ foo, bar } │                                                          \n\
        \    └────────────────┘                                                          \n\
        \      ⇧                                                                         \n\
        \      Invalid: Not a record                                                     \n\
        \                                                                                \n\
        \                                                                                \n\
        \Some common reasons why you might get this error:                               \n\
        \                                                                                \n\
        \● You accidentally try to project fields of a union instead of a record, like   \n\
        \  this:                                                                         \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────────────────┐                                      \n\
        \    │ < foo : a | bar : b >.{ foo, bar } │                                      \n\
        \    └────────────────────────────────────┘                                      \n\
        \      ⇧                                                                         \n\
        \      This is a union, not a record                                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \────────────────────────────────────────────────────────────────────────────────\n\
        \                                                                                \n\
        \You tried to access the fields:                                               \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... on the following expression which is not a record:                          \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... but is actually an expression of type:                                      \n\
        \                                                                                \n\
        \" <> txt2 <> "\n"
      where
        txt0 = insert lazyText0
        txt1 = insert expr0
        txt2 = insert expr1

prettyTypeMessage (MissingAlternative k _) = ErrorMessages short short
  where
    short = "Missing union constructor: " <> pretty k

prettyTypeMessage (MissingField k expr0) = ErrorMessages {..}
        where
          short = "Missing record field: " <> pretty k

          long =
              "Explanation: You can only access fields on records, like this:                  \n\
              \                                                                                \n\
              \                                                                                \n\
              \    ┌─────────────────────────────────┐                                         \n\
              \    │ { foo = True, bar = \"ABC\" }.foo │  This is valid ...                    \n\
              \    └─────────────────────────────────┘                                         \n\
              \                                                                                \n\
              \                                                                                \n\
              \    ┌───────────────────────────────────────────┐                               \n\
              \    │ λ(r : { foo : Bool, bar : Text }) → r.foo │  ... and so is this           \n\
              \    └───────────────────────────────────────────┘                               \n\
              \                                                                                \n\
              \                                                                                \n\
              \... but you can only access fields if they are present                          \n\
              \                                                                                \n\
              \For example, the following expression is " <> _NOT <> " valid:                  \n\
              \                                                                                \n\
              \    ┌─────────────────────────────────┐                                         \n\
              \    │ { foo = True, bar = \"ABC\" }.qux │                                       \n\
              \    └─────────────────────────────────┘                                         \n\
              \                                  ⇧                                             \n\
              \                                  Invalid: the record has no ❰qux❱ field        \n\
              \                                                                                \n\
              \You tried to access a field named:                                              \n\
              \                                                                                \n\
              \" <> txt0 <> "\n\
              \                                                                                \n\
              \... but the field is missing because the record only defines the following      \n\
              \fields:                                                                         \n\
              \                                                                                \n\
              \" <> txt1 <> "\n"
            where
              txt0 = insert k
              txt1 = insert expr0

prettyTypeMessage (CantAnd expr0 expr1) =
        buildBooleanOperator "&&" expr0 expr1

prettyTypeMessage (CantOr expr0 expr1) =
        buildBooleanOperator "||" expr0 expr1

prettyTypeMessage (CantEQ expr0 expr1) =
        buildBooleanOperator "==" expr0 expr1

prettyTypeMessage (CantNE expr0 expr1) =
        buildBooleanOperator "!=" expr0 expr1

prettyTypeMessage (CantInterpolate expr0 expr1) = ErrorMessages {..}
  where
    short = "You can only interpolate ❰Text❱"

    long =
        "Explanation: Text interpolation only works on expressions of type ❰Text❱        \n\
        \                                                                                \n\
        \For example, these are all valid uses of string interpolation:                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────┐                                                        \n\
        \    │ \"ABC${\"DEF\"}GHI\" │                                                        \n\
        \    └──────────────────┘                                                        \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────────┐                                              \n\
        \    │ λ(x : Text) → \"ABC${x}GHI\" │                                              \n\
        \    └────────────────────────────┘                                              \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────────────────────┐                           \n\
        \    │ λ(age : Natural) → \"Age: ${Natural/show age}\" │                           \n\
        \    └───────────────────────────────────────────────┘                           \n\
        \                                                                                \n\
        \                                                                                \n\
        \Some common reasons why you might get this error:                               \n\
        \                                                                                \n\
        \● You might have thought that string interpolation automatically converts the   \n\
        \  interpolated value to a ❰Text❱ representation of that value:                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌──────────────────────────────────┐                                        \n\
        \    │ λ(age : Natural) → \"Age: ${age}\" │                                        \n\
        \    └──────────────────────────────────┘                                        \n\
        \                                  ⇧                                             \n\
        \                                  Invalid: ❰age❱ has type ❰Natural❱             \n\
        \                                                                                \n\
        \                                                                                \n\
        \● You might have forgotten to escape a string interpolation that you wanted     \n\
        \  Dhall to ignore and pass through:                                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────┐                                                          \n\
        \    │ \"echo ${HOME}\" │                                                          \n\
        \    └────────────────┘                                                          \n\
        \             ⇧                                                                  \n\
        \             ❰HOME❱ is not in scope and this might have meant to use ❰\\${HOME}❱\n\
        \                                                                                \n\
        \                                                                                \n\
        \────────────────────────────────────────────────────────────────────────────────\n\
        \                                                                                \n\
        \You interpolated this expression:                                               \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... which does not have type ❰Text❱ but instead has type:                       \n\
        \                                                                                \n\
        \" <> txt1 <> "\n"
      where
        txt0 = insert expr0
        txt1 = insert expr1

prettyTypeMessage (CantTextAppend expr0 expr1) = ErrorMessages {..}
  where
    short = "❰++❱ only works on ❰Text❱"

    long =
        "Explanation: The ❰++❱ operator expects two arguments that have type ❰Text❱      \n\
        \                                                                                \n\
        \For example, this is a valid use of ❰++❱:                                       \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────┐                                                          \n\
        \    │ \"ABC\" ++ \"DEF\" │                                                          \n\
        \    └────────────────┘                                                          \n\
        \                                                                                \n\
        \                                                                                \n\
        \Some common reasons why you might get this error:                               \n\
        \                                                                                \n\
        \● You might have thought that ❰++❱ was the operator to combine two lists:       \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌────────────────────────┐                                                  \n\
        \    │ [1, 2, 3] ++ [4, 5, 6] │  Not valid                                       \n\
        \    └────────────────────────┘                                                  \n\
        \                                                                                \n\
        \                                                                                \n\
        \  ... but the list concatenation operator is actually ❰#❱:                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────┐                                                   \n\
        \    │ [1, 2, 3] # [4, 5, 6] │  Valid                                            \n\
        \    └───────────────────────┘                                                   \n\
        \                                                                                \n\
        \                                                                                \n\
        \────────────────────────────────────────────────────────────────────────────────\n\
        \                                                                                \n\
        \You provided this argument:                                                     \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... which does not have type ❰Text❱ but instead has type:                       \n\
        \                                                                                \n\
        \" <> txt1 <> "\n"
      where
        txt0 = insert expr0
        txt1 = insert expr1

prettyTypeMessage (CantListAppend expr0 expr1) = ErrorMessages {..}
  where
    short = "❰#❱ only works on ❰List❱s"

    long =
        "Explanation: The ❰#❱ operator expects two arguments that are both ❰List❱s       \n\
        \                                                                                \n\
        \For example, this is a valid use of ❰#❱:                                        \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────┐                                                   \n\
        \    │ [1, 2, 3] # [4, 5, 6] │                                                   \n\
        \    └───────────────────────┘                                                   \n\
        \                                                                                \n\
        \                                                                                \n\
        \────────────────────────────────────────────────────────────────────────────────\n\
        \                                                                                \n\
        \You provided this argument:                                                     \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... which is not a ❰List❱ but instead has type:                                 \n\
        \                                                                                \n\
        \" <> txt1 <> "\n"
      where
        txt0 = insert expr0
        txt1 = insert expr1

prettyTypeMessage (CantAdd expr0 expr1) =
        buildNaturalOperator "+" expr0 expr1

prettyTypeMessage (CantMultiply expr0 expr1) =
        buildNaturalOperator "*" expr0 expr1

prettyTypeMessage (NoDependentTypes expr0 expr1) = ErrorMessages {..}
  where
    short = "No dependent types"

    long =
        "Explanation: The Dhall programming language does not allow functions from terms \n\
        \to types.  These function types are also known as “dependent function types”    \n\
        \because you have a type whose value “depends” on the value of a term.           \n\
        \                                                                                \n\
        \For example, this is " <> _NOT <> " a legal function type:                      \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────┐                                                             \n\
        \    │ Bool → Type │                                                             \n\
        \    └─────────────┘                                                             \n\
        \                                                                                \n\
        \                                                                                \n\
        \Similarly, this is " <> _NOT <> " legal code:                                   \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────────────────────────────────────────┐                       \n\
        \    │ λ(Vector : Natural → Type → Type) → Vector 0 Text │                       \n\
        \    └───────────────────────────────────────────────────┘                       \n\
        \                 ⇧                                                              \n\
        \                 Invalid dependent type                                         \n\
        \                                                                                \n\
        \                                                                                \n\
        \Your function type is invalid because the input has type:                       \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... and the output has kind:                                                    \n\
        \                                                                                \n\
        \" <> txt1 <> "\n\
        \                                                                                \n\
        \... which makes this a forbidden dependent function type                        \n"
      where
        txt0 = insert expr0
        txt1 = insert expr1

prettyTypeMessage (ExpectedAType got) = ErrorMessages short "" where
  short = "Expected a type or a kind\n"
      <>  "\n"
      <>  "inferred type:\n\n"
      <>  "    " <> pretty got

prettyTypeMessage (UnexpectedRecordField k a) = ErrorMessages short "" where
  short = "Unexpected record field: " <> pretty k
      <>  "\n\n"
      <>  "Expected an expression with type:\n\n"
      <>  "    " <> pretty a

prettyTypeMessage (ConvError has want) = ErrorMessages short "" where
  short =  "Type mismatch\n"
        <> "expected type:\n\n"
        <> "    " <> pretty want
        <> "\n\ninferred type:\n\n"
        <> "    " <> pretty has

prettyTypeMessage (MergeDependentHandler k a) = ErrorMessages short "" where
  short =  "Merge handler cannot have dependent function type.\n"
        <> "Inferred type for handler of alternative " <> pretty k <> ":\n\n"
        <> "    " <> pretty a


buildBooleanOperator :: Text -> Core -> Core -> ErrorMessages
buildBooleanOperator operator expr0 expr1 = ErrorMessages {..}
  where
    short = "❰" <> txt2 <> "❱ only works on ❰Bool❱s"

    long =
        "Explanation: The ❰" <> txt2 <> "❱ operator expects two arguments that have type ❰Bool❱\n\
        \                                                                                \n\
        \For example, this is a valid use of ❰" <> txt2 <> "❱:                           \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────────────┐                                                           \n\
        \    │ True " <> txt2 <> " False │                                               \n\
        \    └───────────────┘                                                           \n\
        \                                                                                \n\
        \                                                                                \n\
        \You provided this argument:                                                     \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... which does not have type ❰Bool❱ but instead has type:                       \n\
        \                                                                                \n\
        \" <> txt1 <> "\n"
      where
        txt0 = insert expr0
        txt1 = insert expr1

    txt2 = pretty operator

buildNaturalOperator :: Text -> Core -> Core -> ErrorMessages
buildNaturalOperator operator expr0 expr1 = ErrorMessages {..}
  where
    short = "❰" <> txt2 <> "❱ only works on ❰Natural❱s"

    long =
        "Explanation: The ❰" <> txt2 <> "❱ operator expects two arguments that have type ❰Natural❱\n\
        \                                                                                \n\
        \For example, this is a valid use of ❰" <> txt2 <> "❱:                           \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────┐                                                                   \n\
        \    │ 3 " <> txt2 <> " 5 │                                                      \n\
        \    └───────┘                                                                   \n\
        \                                                                                \n\
        \                                                                                \n\
        \Some common reasons why you might get this error:                               \n\
        \                                                                                \n\
        \● You might have tried to use an ❰Integer❱, which is " <> _NOT <> " allowed:    \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────────────────────────────────────┐                                 \n\
        \    │ λ(x : Integer) → λ(y : Integer) → x " <> txt2 <> " y │  Not valid         \n\
        \    └─────────────────────────────────────────┘                                 \n\
        \                                                                                \n\
        \                                                                                \n\
        \  You can only use ❰Natural❱ numbers                                            \n\
        \                                                                                \n\
        \                                                                                \n\
        \● You might have mistakenly used an ❰Integer❱ literal, which is " <> _NOT <> " allowed:\n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌─────────┐                                                                 \n\
        \    │ +2 " <> txt2 <> " +2 │  Not valid                                         \n\
        \    └─────────┘                                                                 \n\
        \                                                                                \n\
        \                                                                                \n\
        \  You need to remove the leading ❰+❱ to transform them into ❰Natural❱ literals, \n\
        \  like this:                                                                    \n\
        \                                                                                \n\
        \                                                                                \n\
        \    ┌───────┐                                                                   \n\
        \    │ 2 " <> txt2 <> " 2 │  Valid                                               \n\
        \    └───────┘                                                                   \n\
        \                                                                                \n\
        \                                                                                \n\
        \────────────────────────────────────────────────────────────────────────────────\n\
        \                                                                                \n\
        \You provided this argument:                                                     \n\
        \                                                                                \n\
        \" <> txt0 <> "\n\
        \                                                                                \n\
        \... which does not have type ❰Natural❱ but instead has type:                    \n\
        \                                                                                \n\
        \" <> txt1 <> "\n"
      where
        txt0 = insert expr0
        txt1 = insert expr1

    txt2 = pretty operator

insert :: Pretty a => a -> Doc Ann
insert = Dhall.Util.insert

