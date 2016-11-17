{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# OPTIONS_GHC -Wall #-}

module Dhall.TypeCheck (
    -- * Type-checking
      typeWith
    , typeOf

    -- * Types
    , X(..)
    , TypeError(..)
    , DetailedTypeError(..)
    , TypeMessage(..)
    ) where

import Control.Exception (Exception)
import Data.Foldable (forM_)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Text.Buildable (Buildable(..))
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Typeable (Typeable)
import Dhall.Core (Const(..), Expr(..), Var(..))
import Dhall.Context (Context)

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Lazy                   as Text
import qualified Data.Text.Lazy.Builder           as Builder
import qualified Data.Vector
import qualified Dhall.Context
import qualified Dhall.Core
import qualified NeatInterpolation

axiom :: Const -> Either (TypeError s) Const
axiom Type = return Kind
axiom Kind = Left (TypeError Dhall.Context.empty (Const Kind) Untyped)

rule :: Const -> Const -> Either () Const
rule Type Kind = Left ()
rule Type Type = return Type
rule Kind Kind = return Kind
rule Kind Type = return Type

match :: Var -> Var -> [(Text, Text)] -> Bool
match (V xL nL) (V xR nR)             []  =
    xL == xR  && nL == nR
match (V xL 0 ) (V xR 0 ) ((xL', xR'):_ )
    | xL == xL' && xR == xR' = True
match (V xL nL) (V xR nR) ((xL', xR'):xs) =
    match (V xL nL') (V xR nR') xs
  where
    nL' = if xL == xL' then nL - 1 else nL
    nR' = if xR == xR' then nR - 1 else nR

propEqual :: Expr s X -> Expr t X -> Bool
propEqual eL0 eR0 =
    State.evalState
        (go (Dhall.Core.normalize eL0) (Dhall.Core.normalize eR0))
        []
  where
    go (Const Type) (Const Type) = return True
    go (Const Kind) (Const Kind) = return True
    go (Var vL) (Var vR) = do
        ctx <- State.get
        return (match vL vR ctx)
    go (Pi xL tL bL) (Pi xR tR bR) = do
        ctx <- State.get
        eq1 <- go tL tR
        if eq1
            then do
                State.put ((xL, xR):ctx)
                eq2 <- go bL bR
                State.put ctx
                return eq2
            else return False
    go (App fL aL) (App fR aR) = do
        b1 <- go fL fR
        if b1 then go aL aR else return False
    go Bool Bool = return True
    go Natural Natural = return True
    go Integer Integer = return True
    go Double Double = return True
    go Text Text = return True
    go List List = return True
    go Optional Optional = return True
    go (Record ktsL0) (Record ktsR0) = do
        let loop ((kL, tL):ktsL) ((kR, tR):ktsR)
                | kL == kR = do
                    b <- go tL tR
                    if b
                        then loop ktsL ktsR
                        else return False
            loop [] [] = return True
            loop _  _  = return False
        loop (Data.Map.toList ktsL0) (Data.Map.toList ktsR0)
    go (Union ktsL0) (Union ktsR0) = do
        let loop ((kL, tL):ktsL) ((kR, tR):ktsR)
                | kL == kR = do
                    b <- go tL tR
                    if b
                        then loop ktsL ktsR
                        else return False
            loop [] [] = return True
            loop _  _  = return False
        loop (Data.Map.toList ktsL0) (Data.Map.toList ktsR0)
    go _ _ = return False

{-| Type-check an expression and return the expression's type if type-checking
    suceeds or an error if type-checking fails

    `typeWith` does not necessarily normalize the type since full normalization
    is not necessary for just type-checking.  If you actually care about the
    returned type then you may want to `normalize` it afterwards.
-}
typeWith :: Context (Expr s X) -> Expr s X -> Either (TypeError s) (Expr s X)
typeWith _     (Const c         ) = do
    fmap Const (axiom c)
typeWith ctx e@(Var (V x n)     ) = do
    case Dhall.Context.lookup x n ctx of
        Nothing -> Left (TypeError ctx e UnboundVariable)
        Just a  -> return a
typeWith ctx   (Lam x _A  b     ) = do
    let ctx' = fmap (Dhall.Core.shift 1 (V x 0)) (Dhall.Context.insert x _A ctx)
    _B <- typeWith ctx' b
    let p = Pi x _A _B
    _t <- typeWith ctx p
    return p
typeWith ctx e@(Pi  x _A _B     ) = do
    tA <- fmap Dhall.Core.normalize (typeWith ctx _A)
    kA <- case tA of
        Const k -> return k
        _       -> Left (TypeError ctx e (InvalidInputType _A))

    let ctx' = fmap (Dhall.Core.shift 1 (V x 0)) (Dhall.Context.insert x _A ctx)
    tB <- fmap Dhall.Core.normalize (typeWith ctx' _B)
    kB <- case tB of
        Const k -> return k
        _       -> Left (TypeError ctx' e (InvalidOutputType _B))

    case rule kA kB of
        Left () -> Left (TypeError ctx e (NoDependentTypes _A _B))
        Right k -> Right (Const k)
typeWith ctx e@(App f a         ) = do
    tf <- fmap Dhall.Core.normalize (typeWith ctx f)
    (x, _A, _B) <- case tf of
        Pi x _A _B -> return (x, _A, _B)
        _          -> Left (TypeError ctx e (NotAFunction f tf))
    _A' <- typeWith ctx a
    if propEqual _A _A'
        then do
            let a'   = Dhall.Core.shift   1  (V x 0) a
            let _B'  = Dhall.Core.subst (V x 0) a' _B
            let _B'' = Dhall.Core.shift (-1) (V x 0) _B'
            return _B''
        else do
            let nf_A  = Dhall.Core.normalize _A
            let nf_A' = Dhall.Core.normalize _A'
            Left (TypeError ctx e (TypeMismatch f nf_A a nf_A'))
typeWith ctx e@(Let f mt r b ) = do
    tR  <- typeWith ctx r
    ttR <- fmap Dhall.Core.normalize (typeWith ctx tR)
    kR  <- case ttR of
        Const k -> return k
        -- Don't bother to provide a `let`-specific version of this error
        -- message because this should never happen anyway
        _       -> Left (TypeError ctx e (InvalidInputType tR))

    let ctx' = Dhall.Context.insert f tR ctx
    tB  <- typeWith ctx' b
    ttB <- fmap Dhall.Core.normalize (typeWith ctx tB)
    kB  <- case ttB of
        Const k -> return k
        -- Don't bother to provide a `let`-specific version of this error
        -- message because this should never happen anyway
        _       -> Left (TypeError ctx e (InvalidOutputType tB))

    case rule kR kB of
        Left () -> Left (TypeError ctx e (NoDependentLet tR tB))
        Right _ -> return ()

    case mt of
        Nothing -> do
            return ()
        Just t  -> do
            let nf_t  = Dhall.Core.normalize t
            let nf_tR = Dhall.Core.normalize tR
            if propEqual nf_tR nf_t
                then return ()
                else Left (TypeError ctx e (AnnotMismatch r nf_t nf_tR))

    return tB
typeWith ctx e@(Annot x t       ) = do
    -- This is mainly just to check that `t` is not `Kind`
    _ <- typeWith ctx t

    t' <- typeWith ctx x
    if propEqual t t'
        then do
            return t
        else do
            let nf_t  = Dhall.Core.normalize t
            let nf_t' = Dhall.Core.normalize t'
            Left (TypeError ctx e (AnnotMismatch x nf_t nf_t'))
typeWith _      Bool              = do
    return (Const Type)
typeWith _     (BoolLit _       ) = do
    return Bool
typeWith ctx e@(BoolAnd l r     ) = do
    tl <- fmap Dhall.Core.normalize (typeWith ctx l)
    case tl of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantAnd l tl))

    tr <- fmap Dhall.Core.normalize (typeWith ctx r)
    case tr of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantAnd r tr))

    return Bool
typeWith ctx e@(BoolOr  l r     ) = do
    tl <- fmap Dhall.Core.normalize (typeWith ctx l)
    case tl of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantOr l tl))

    tr <- fmap Dhall.Core.normalize (typeWith ctx r)
    case tr of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantOr r tr))

    return Bool
typeWith ctx e@(BoolEQ  l r     ) = do
    tl <- fmap Dhall.Core.normalize (typeWith ctx l)
    case tl of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantEQ l tl))

    tr <- fmap Dhall.Core.normalize (typeWith ctx r)
    case tr of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantEQ r tr))

    return Bool
typeWith ctx e@(BoolNE  l r     ) = do
    tl <- fmap Dhall.Core.normalize (typeWith ctx l)
    case tl of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantNE l tl))

    tr <- fmap Dhall.Core.normalize (typeWith ctx r)
    case tr of
        Bool -> return ()
        _    -> Left (TypeError ctx e (CantNE r tr))

    return Bool
typeWith ctx e@(BoolIf x y z    ) = do
    tx <- fmap Dhall.Core.normalize (typeWith ctx x)
    case tx of
        Bool -> return ()
        _    -> Left (TypeError ctx e (InvalidPredicate x tx))
    ty  <- fmap Dhall.Core.normalize (typeWith ctx y )
    tty <- fmap Dhall.Core.normalize (typeWith ctx ty)
    case tty of
        Const Type -> return ()
        _          -> Left (TypeError ctx e (IfBranchMustBeTerm True y ty tty))

    tz <- fmap Dhall.Core.normalize (typeWith ctx z)
    ttz <- fmap Dhall.Core.normalize (typeWith ctx tz)
    case ttz of
        Const Type -> return ()
        _          -> Left (TypeError ctx e (IfBranchMustBeTerm False z tz ttz))

    if propEqual ty tz
        then return ()
        else Left (TypeError ctx e (IfBranchMismatch y z ty tz))
    return ty
typeWith _      Natural           = do
    return (Const Type)
typeWith _     (NaturalLit _    ) = do
    return Natural
typeWith _      NaturalFold       = do
    return
        (Pi "_" Natural
            (Pi "natural" (Const Type)
                (Pi "succ" (Pi "_" "natural" "natural")
                    (Pi "zero" "natural" "natural") ) ) )
typeWith _      NaturalBuild      = do
    return
        (Pi "_"
            (Pi "natural" (Const Type)
                (Pi "succ" (Pi "_" "natural" "natural")
                    (Pi "zero" "natural" "natural") ) )
            Natural )
typeWith _      NaturalIsZero     = do
    return (Pi "_" Natural Bool)
typeWith _      NaturalEven       = do
    return (Pi "_" Natural Bool)
typeWith _      NaturalOdd        = do
    return (Pi "_" Natural Bool)
typeWith ctx e@(NaturalPlus  l r) = do
    tl <- fmap Dhall.Core.normalize (typeWith ctx l)
    case tl of
        Natural -> return ()
        _       -> Left (TypeError ctx e (CantAdd l tl))

    tr <- fmap Dhall.Core.normalize (typeWith ctx r)
    case tr of
        Natural -> return ()
        _       -> Left (TypeError ctx e (CantAdd r tr))
    return Natural
typeWith ctx e@(NaturalTimes l r) = do
    tl <- fmap Dhall.Core.normalize (typeWith ctx l)
    case tl of
        Natural -> return ()
        _       -> Left (TypeError ctx e (CantMultiply l tl))

    tr <- fmap Dhall.Core.normalize (typeWith ctx r)
    case tr of
        Natural -> return ()
        _       -> Left (TypeError ctx e (CantMultiply r tr))
    return Natural
typeWith _      Integer           = do
    return (Const Type)
typeWith _     (IntegerLit _    ) = do
    return Integer
typeWith _      Double            = do
    return (Const Type)
typeWith _     (DoubleLit _     ) = do
    return Double
typeWith _      Text              = do
    return (Const Type)
typeWith _     (TextLit _       ) = do
    return Text
typeWith ctx e@(TextAppend l r  ) = do
    tl <- fmap Dhall.Core.normalize (typeWith ctx l)
    case tl of
        Text -> return ()
        _    -> Left (TypeError ctx e (CantTextAppend l tl))

    tr <- fmap Dhall.Core.normalize (typeWith ctx r)
    case tr of
        Text -> return ()
        _    -> Left (TypeError ctx e (CantTextAppend r tr))
    return Text
typeWith _      List              = do
    return (Pi "_" (Const Type) (Const Type))
typeWith ctx e@(ListLit t xs    ) = do
    s <- fmap Dhall.Core.normalize (typeWith ctx t)
    case s of
        Const Type -> return ()
        _ -> Left (TypeError ctx e (InvalidListType t))
    flip Data.Vector.imapM_ xs (\i x -> do
        t' <- typeWith ctx x
        if propEqual t t'
            then return ()
            else do
                let nf_t  = Dhall.Core.normalize t
                let nf_t' = Dhall.Core.normalize t'
                Left (TypeError ctx e (InvalidListElement i nf_t x nf_t')) )
    return (App List t)
typeWith _      ListBuild         = do
    return
        (Pi "a" (Const Type)
            (Pi "_"
                (Pi "list" (Const Type)
                    (Pi "cons" (Pi "_" "a" (Pi "_" "list" "list"))
                        (Pi "nil" "list" "list") ) )
                (App List "a") ) )
typeWith _      ListFold          = do
    return
        (Pi "a" (Const Type)
            (Pi "_" (App List "a")
                (Pi "list" (Const Type)
                    (Pi "cons" (Pi "_" "a" (Pi "_" "list" "list"))
                        (Pi "nil" "list" "list")) ) ) )
typeWith _      ListLength        = do
    return (Pi "a" (Const Type) (Pi "_" (App List "a") Natural))
typeWith _      ListHead          = do
    return (Pi "a" (Const Type) (Pi "_" (App List "a") (App Optional "a")))
typeWith _      ListLast          = do
    return (Pi "a" (Const Type) (Pi "_" (App List "a") (App Optional "a")))
typeWith _      ListIndexed       = do
    let kts = [("index", Natural), ("value", "a")]
    return
        (Pi "a" (Const Type)
            (Pi "_" (App List "a")
                (App List (Record (Data.Map.fromList kts))) ) )
typeWith _      ListReverse       = do
    return (Pi "a" (Const Type) (Pi "_" (App List "a") (App List "a")))
typeWith _      Optional          = do
    return (Pi "_" (Const Type) (Const Type))
typeWith ctx e@(OptionalLit t xs) = do
    s <- fmap Dhall.Core.normalize (typeWith ctx t)
    case s of
        Const Type -> return ()
        _ -> Left (TypeError ctx e (InvalidOptionalType t))
    let n = Data.Vector.length xs
    if 2 <= n
        then Left (TypeError ctx e (InvalidOptionalLiteral n))
        else return ()
    forM_ xs (\x -> do
        t' <- typeWith ctx x
        if propEqual t t'
            then return ()
            else do
                let nf_t  = Dhall.Core.normalize t
                let nf_t' = Dhall.Core.normalize t'
                Left (TypeError ctx e (InvalidOptionalElement nf_t x nf_t')) )
    return (App Optional t)
typeWith _      OptionalFold      = do
    return
        (Pi "a" (Const Type)
            (Pi "_" (App Optional "a")
                (Pi "maybe" (Const Type)
                    (Pi "just" (Pi "_" "a" "maybe")
                        (Pi "nothing" "maybe" "maybe") ) ) ) )
typeWith ctx e@(Record    kts   ) = do
    let process (k, t) = do
            s <- fmap Dhall.Core.normalize (typeWith ctx t)
            case s of
                Const Type -> return ()
                _          -> Left (TypeError ctx e (InvalidFieldType k t))
    mapM_ process (Data.Map.toList kts)
    return (Const Type)
typeWith ctx   (RecordLit kvs   ) = do
    let process (k, v) = do
            t <- typeWith ctx v
            return (k, t)
    kts <- mapM process (Data.Map.toAscList kvs)
    return (Record (Data.Map.fromAscList kts))
typeWith ctx e@(Union     kts   ) = do
    let process (k, t) = do
            s <- fmap Dhall.Core.normalize (typeWith ctx t)
            case s of
                Const Type -> return ()
                _          -> Left (TypeError ctx e (InvalidAlternativeType k t))
    mapM_ process (Data.Map.toList kts)
    return (Const Type)
typeWith ctx e@(UnionLit k v kts) = do
    case Data.Map.lookup k kts of
        Just _  -> Left (TypeError ctx e (DuplicateAlternative k))
        Nothing -> return ()
    t   <- typeWith ctx v
    return (Union (Data.Map.insert k t kts))
typeWith ctx e@(Combine kvsX kvsY) = do
    tKvsX <- fmap Dhall.Core.normalize (typeWith ctx kvsX)
    ktsX  <- case tKvsX of
        Record kts -> return kts
        _          -> Left (TypeError ctx e (MustCombineARecord kvsX tKvsX))
    let ksX = Data.Map.keysSet ktsX

    tKvsY <- fmap Dhall.Core.normalize (typeWith ctx kvsY)
    ktsY  <- case tKvsY of
        Record kts -> return kts
        _          -> Left (TypeError ctx e (MustCombineARecord kvsY tKvsY))
    let ksY = Data.Map.keysSet ktsY

    let ks = Data.Set.intersection ksX ksY
    if Data.Set.null ks
        then return ()
        else Left (TypeError ctx e (FieldCollision ks))
    return (Record (Data.Map.union ktsX ktsY))
typeWith ctx e@(Merge kvsX kvsY t) = do
    tKvsX <- fmap Dhall.Core.normalize (typeWith ctx kvsX)
    ktsX  <- case tKvsX of
        Record kts -> return kts
        _          -> Left (TypeError ctx e (MustMergeARecord kvsX tKvsX))
    let ksX = Data.Map.keysSet ktsX

    tKvsY <- fmap Dhall.Core.normalize (typeWith ctx kvsY)
    ktsY  <- case tKvsY of
        Union kts -> return kts
        _         -> Left (TypeError ctx e (MustMergeUnion kvsY tKvsY))
    let ksY = Data.Map.keysSet ktsY

    let diffX = Data.Set.difference ksX ksY
    let diffY = Data.Set.difference ksY ksX

    if Data.Set.null diffX
        then return ()
        else Left (TypeError ctx e (UnusedHandler diffX))

    let process (kY, tY) = do
            case Data.Map.lookup kY ktsX of
                Nothing  -> Left (TypeError ctx e (MissingHandler diffY))
                Just tX  ->
                    case tX of
                        Pi _ tY' t' -> do
                            if propEqual tY tY'
                                then return ()
                                else Left (TypeError ctx e (HandlerInputTypeMismatch kY tY tY'))
                            if propEqual t t'
                                then return ()
                                else Left (TypeError ctx e (HandlerOutputTypeMismatch kY t t'))
                        _ -> Left (TypeError ctx e (HandlerNotAFunction kY tX))
    mapM_ process (Data.Map.toList ktsY)
    return t
typeWith ctx e@(Field r x       ) = do
    t <- fmap Dhall.Core.normalize (typeWith ctx r)
    case t of
        Record kts ->
            case Data.Map.lookup x kts of
                Just t' -> return t'
                Nothing -> Left (TypeError ctx e (MissingField x t))
        _          -> Left (TypeError ctx e (NotARecord x r t))
typeWith ctx   (Note s e'       ) = case typeWith ctx e' of
    Left (TypeError ctx' (Note s' e'') m) -> Left (TypeError ctx' (Note s' e'') m)
    Left (TypeError ctx'          e''  m) -> Left (TypeError ctx' (Note s  e'') m)
    Right r                               -> Right r
typeWith _     (Embed p         ) = do
    absurd p

{-| `typeOf` is the same as `typeWith` with an empty context, meaning that the
    expression must be closed (i.e. no free variables), otherwise type-checking
    will fail.
-}
typeOf :: Expr s X -> Either (TypeError s) (Expr s X)
typeOf = typeWith Dhall.Context.empty

-- | Like `Data.Void.Void`, except with a shorter inferred type
newtype X = X { absurd :: forall a . a }

instance Show X where
    show = absurd

instance Buildable X where
    build = absurd

-- | The specific type error
data TypeMessage s
    = UnboundVariable
    | InvalidInputType (Expr s X)
    | InvalidOutputType (Expr s X)
    | NotAFunction (Expr s X) (Expr s X)
    | TypeMismatch (Expr s X) (Expr s X) (Expr s X) (Expr s X)
    | AnnotMismatch (Expr s X) (Expr s X) (Expr s X)
    | Untyped
    | InvalidListElement Int (Expr s X) (Expr s X) (Expr s X)
    | InvalidListType (Expr s X)
    | InvalidOptionalElement (Expr s X) (Expr s X) (Expr s X)
    | InvalidOptionalLiteral Int
    | InvalidOptionalType (Expr s X)
    | InvalidPredicate (Expr s X) (Expr s X)
    | IfBranchMismatch (Expr s X) (Expr s X) (Expr s X) (Expr s X)
    | IfBranchMustBeTerm Bool (Expr s X) (Expr s X) (Expr s X)
    | InvalidFieldType Text (Expr s X)
    | InvalidAlternativeType Text (Expr s X)
    | DuplicateAlternative Text
    | MustCombineARecord (Expr s X) (Expr s X)
    | FieldCollision (Set Text)
    | MustMergeARecord (Expr s X) (Expr s X)
    | MustMergeUnion (Expr s X) (Expr s X)
    | UnusedHandler (Set Text)
    | MissingHandler (Set Text)
    | HandlerInputTypeMismatch Text (Expr s X) (Expr s X)
    | HandlerOutputTypeMismatch Text (Expr s X) (Expr s X)
    | HandlerNotAFunction Text (Expr s X)
    | NotARecord Text (Expr s X) (Expr s X)
    | MissingField Text (Expr s X)
    | CantAnd (Expr s X) (Expr s X)
    | CantOr (Expr s X) (Expr s X)
    | CantEQ (Expr s X) (Expr s X)
    | CantNE (Expr s X) (Expr s X)
    | CantTextAppend (Expr s X) (Expr s X)
    | CantAdd (Expr s X) (Expr s X)
    | CantMultiply (Expr s X) (Expr s X)
    | NoDependentLet (Expr s X) (Expr s X)
    | NoDependentTypes (Expr s X) (Expr s X)
    deriving (Show)

shortTypeMessage :: TypeMessage s -> Builder
shortTypeMessage msg =
    "\ESC[1;31mError:\ESC[0m " <> build short <> "\n"
  where
    ErrorMessages {..} = prettyTypeMessage msg

longTypeMessage :: TypeMessage s -> Builder
longTypeMessage msg =
        "\ESC[1;31mError\ESC[0m: " <> build short <> "\n"
    <>  "\n"
    <>  long
  where
    ErrorMessages {..} = prettyTypeMessage msg

data ErrorMessages = ErrorMessages
    { short :: Builder
    -- ^ Default succinct 1-line explanation of what went wrong
    , long  :: Builder
    -- ^ Longer and more detailed explanation of the error
    }

instance Buildable ErrorMessages where
    build (ErrorMessages {..}) =
            "Error: " <> build short <> "\n"
        <>  "\n"
        <>  long

_NOT :: Data.Text.Text
_NOT = "\ESC[1mnot\ESC[0m"

prettyTypeMessage :: TypeMessage s -> ErrorMessages
prettyTypeMessage UnboundVariable = ErrorMessages {..}
  where
    short = "Unbound variable"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: Expressions can only reference previously introduced (i.e. "bound")
variables that are still "in scope"

For example, the following valid expressions introduce a "bound" variable named
❰x❱:


    ┌─────────────────┐
    │ λ(x : Bool) → x │  Anonymous functions introduce "bound" variables
    └─────────────────┘
        ⇧
        This is the bound variable


    ┌─────────────────┐
    │ let x = 1 in x  │  ❰let❱ expressions introduce "bound" variables
    └─────────────────┘
          ⇧
          This is the bound variable


However, the following expressions are not valid because they all reference a
variable that has not been introduced yet (i.e. an "unbound" variable):


    ┌─────────────────┐
    │ λ(x : Bool) → y │  The variable ❰y❱ hasn't been introduced yet
    └─────────────────┘
                    ⇧
                    This is the unbound variable


    ┌──────────────────────────┐
    │ (let x = True in x) && x │  ❰x❱ is undefined outside the parentheses
    └──────────────────────────┘
                             ⇧
                             This is the unbound variable


    ┌────────────────┐
    │ let x = x in x │  The definition for ❰x❱ cannot reference itself
    └────────────────┘
              ⇧
              This is the unbound variable


Some common reasons why you might get this error:

● You misspell a variable name, like this:


    ┌────────────────────────────────────────────────────┐
    │ λ(empty : Bool) → if emty then "Empty" else "Full" │
    └────────────────────────────────────────────────────┘
                           ⇧
                           Typo


● You misspell a reserved identifier, like this:


    ┌──────────────────────────┐
    │ foral (a : Type) → a → a │
    └──────────────────────────┘
      ⇧
      Typo


● You tried to define a recursive value, like this:


    ┌─────────────────────┐
    │ let x = x + +1 in x │
    └─────────────────────┘
              ⇧
              Recursive definitions are not allowed


● You accidentally forgot a ❰λ❱ or ❰∀❱/❰forall❱


        Unbound variable
        ⇩
    ┌─────────────────┐
    │  (x : Bool) → x │
    └─────────────────┘
      ⇧
      A ❰λ❱ here would transform this into a valid anonymous function 


        Unbound variable
        ⇩
    ┌────────────────────┐
    │  (x : Bool) → Bool │
    └────────────────────┘
      ⇧
      A ❰∀❱ or ❰forall❱ here would transform this into a valid function type
|]

prettyTypeMessage (InvalidInputType expr) = ErrorMessages {..}
  where
    short = "Invalid function input"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: A function can accept an input "term" that has a given "type", like
this:


        This is the input term that the function accepts
        ⇩
    ┌───────────────────────┐
    │ ∀(x : Natural) → Bool │  This is the type of a function that accepts an
    └───────────────────────┘  input term named ❰x❱ that has type ❰Natural❱
            ⇧
            This is the type of the input term


    ┌────────────────┐
    │ Bool → Integer │  This is the type of a function that accepts an anonymous
    └────────────────┘  input term that has type ❰Bool❱
      ⇧
      This is the type of the input term


... or a function can accept an input "type" that has a given "kind", like this:


        This is the input type that the function accepts
        ⇩
    ┌────────────────────┐
    │ ∀(a : Type) → Type │  This is the type of a function that accepts an input
    └────────────────────┘  type named ❰a❱ that has kind ❰Type❱
            ⇧
            This is the kind of the input type


    ┌──────────────────────┐
    │ (Type → Type) → Type │  This is the type of a function that accepts an
    └──────────────────────┘  anonymous input type that has kind ❰Type → Type❱
       ⇧
       This is the kind of the input type


Other function inputs are $_NOT valid, like this:


    ┌──────────────┐
    │ ∀(x : 1) → x │  ❰1❱ is a "term" and not a "type" nor a "kind" so ❰x❱
    └──────────────┘  cannot have "type" ❰1❱ or "kind" ❰1❱
            ⇧
            This is not a type or kind


    ┌──────────┐
    │ True → x │  ❰True❱ is a "term" and not a "type" nor a "kind" so the
    └──────────┘  anonymous input cannot have "type" ❰True❱ or "kind" ❰True❱
      ⇧
      This is not a type or kind


You annotated a function input with the following expression:

↳ $txt

... which is neither a type nor a kind
|]
      where
        txt  = Text.toStrict (Dhall.Core.pretty expr)

prettyTypeMessage (InvalidOutputType expr) = ErrorMessages {..}
  where
    short = "Invalid function output"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: A function can return an output "term" that has a given "type",
like this:


    ┌────────────────────┐
    │ ∀(x : Text) → Bool │  This is the type of a function that returns an
    └────────────────────┘  output term that has type ❰Bool❱
                    ⇧
                    This is the type of the output term


    ┌────────────────┐
    │ Bool → Integer │  This is the type of a function that returns an output
    └────────────────┘  term that has type ❰Int❱
             ⇧
             This is the type of the output term


... or a function can return an output "type" that has a given "kind", like
this:

    ┌────────────────────┐
    │ ∀(a : Type) → Type │  This is the type of a function that returns an
    └────────────────────┘  output type that has kind ❰Type❱
                    ⇧
                    This is the kind of the output type


    ┌──────────────────────┐
    │ (Type → Type) → Type │  This is the type of a function that returns an
    └──────────────────────┘  output type that has kind ❰Type❱
                      ⇧
                      This is the kind of the output type


Other outputs are $_NOT valid, like this:


    ┌─────────────────┐
    │ ∀(x : Bool) → x │  ❰x❱ is a "term" and not a "type" nor a "kind" so the
    └─────────────────┘  output cannot have "type" ❰x❱ or "kind" ❰x❱
                    ⇧
                    This is not a type or kind


    ┌─────────────┐
    │ Text → True │  ❰True❱ is a "term" and not a "type" nor a "kind" so the
    └─────────────┘  output cannot have "type" ❰True❱ or "kind" ❰True❱
             ⇧
             This is not a type or kind


You specified that your function outputs a:

↳ $txt

... which is neither a type nor a kind:

Some common reasons why you might get this error:

● You use ❰∀❱ instead of ❰λ❱ by mistake, like this:


    ┌────────────────┐
    │ ∀(x: Bool) → x │
    └────────────────┘
      ⇧
      Using ❰λ❱ here instead of ❰∀❱ would transform this into a valid function
|]
      where
        txt = Text.toStrict (Dhall.Core.pretty expr)

prettyTypeMessage (NotAFunction expr0 expr1) = ErrorMessages {..}
  where
    short = "Not a function"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: Expressions separated by whitespace denote function application,
like this:


    ┌─────┐
    │ f x │  This denotes the function ❰f❱ applied to an argument named ❰x❱ 
    └─────┘


A function is a term that has type ❰a → b❱ for some ❰a❱ or ❰b❱.  For example,
the following expressions are all functions because they have a function type:


                        The function's input type is ❰Bool❱
                        ⇩
    ┌───────────────────────────────┐
    │ λ(x : Bool) → x : Bool → Bool │  User-defined anonymous function
    └───────────────────────────────┘
                               ⇧
                               The function's output type is ❰Bool❱


                     The function's input type is ❰Natural❱
                     ⇩
    ┌───────────────────────────────┐
    │ Natural/even : Natural → Bool │  Built-in function
    └───────────────────────────────┘
                               ⇧
                               The function's output type is ❰Bool❱


                        The function's input kind is ❰Type❱
                        ⇩
    ┌───────────────────────────────┐
    │ λ(a : Type) → a : Type → Type │  Type-level functions are still functions
    └───────────────────────────────┘
                               ⇧
                               The function's output kind is ❰Type❱


             The function's input kind is ❰Type❱
             ⇩
    ┌────────────────────┐
    │ List : Type → Type │  Built-in type-level function
    └────────────────────┘
                    ⇧
                    The function's output kind is ❰Type❱


                        Function's input has kind ❰Type❱
                        ⇩
    ┌─────────────────────────────────────────────────┐
    │ List/head : ∀(a : Type) → (List a → Optional a) │  A function can return
    └─────────────────────────────────────────────────┘  another function
                                ⇧
                                Function's output has type ❰List a → Optional a❱


                       The function's input type is ❰List Text❱
                       ⇩
    ┌────────────────────────────────────────────┐
    │ List/head Text : List Text → Optional Text │  A function applied to an
    └────────────────────────────────────────────┘  argument can be a function
                                   ⇧
                                   The function's output type is ❰Optional Text❱


An expression is not a function if the expression's type is not of the form
❰a → b❱.  For example, these are $_NOT functions:


    ┌─────────────┐
    │ 1 : Integer │  ❰1❱ is not a function because ❰Integer❱ is not the type of
    └─────────────┘  a function


    ┌────────────────────────┐
    │ Natural/even +2 : Bool │  ❰Natural/even +2❱ is not a function because
    └────────────────────────┘  ❰Bool❱ is not the type of a function


    ┌──────────────────┐
    │ List Text : Type │  ❰List Text❱ is not a function because ❰Type❱ is not
    └──────────────────┘  the type of a function


You tried to use the following expression as a function:

↳ $txt0

... but this expression's type is:

↳ $txt1

... which is not a function type
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)

prettyTypeMessage (TypeMismatch expr0 expr1 expr2 expr3) = ErrorMessages {..}
  where
    short = "Wrong function argument"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: Every function declares what type or kind of argument to accept

For example:


    ┌───────────────────────────────┐
    │ λ(x : Bool) → x : Bool → Bool │  This anonymous function only accepts
    └───────────────────────────────┘  arguments that have type ❰Bool❱
                        ⇧
                        The function's input type


    ┌───────────────────────────────┐
    │ Natural/even : Natural → Bool │  This built-in function only accepts
    └───────────────────────────────┘  arguments that have type ❰Natural❱
                     ⇧
                     The function's input type


    ┌───────────────────────────────┐
    │ λ(a : Type) → a : Type → Type │  This anonymous function only accepts
    └───────────────────────────────┘  arguments that have kind ❰Type❱
                        ⇧
                        The function's input kind


    ┌────────────────────┐
    │ List : Type → Type │  This built-in function only accepts arguments that
    └────────────────────┘  have kind ❰Type❱
             ⇧
             The function's input kind


For example, the following expressions are valid:


    ┌────────────────────────┐
    │ (λ(x : Bool) → x) True │  ❰True❱ has type ❰Bool❱, which matches the type
    └────────────────────────┘  of argument that the anonymous function accepts


    ┌─────────────────┐
    │ Natural/even +2 │  ❰+2❱ has type ❰Natural❱, which matches the type of
    └─────────────────┘  argument that the ❰Natural/even❱ function accepts,


    ┌────────────────────────┐
    │ (λ(a : Type) → a) Bool │  ❰Bool❱ has kind ❰Type❱, which matches the kind
    └────────────────────────┘  of argument that the anonymous function accepts


    ┌───────────┐
    │ List Text │  ❰Text❱ has kind ❰Type❱, which matches the kind of argument
    └───────────┘  that that the ❰List❱ function accepts


However, you can $_NOT apply a function to the wrong type or kind of argument

For example, the following expressions are not valid:


    ┌───────────────────────┐
    │ (λ(x : Bool) → x) "A" │  ❰"A"❱ has type ❰Text❱, but the anonymous function
    └───────────────────────┘  expects an argument that has type ❰Bool❱


    ┌──────────────────┐
    │ Natural/even "A" │  ❰"A"❱ has type ❰Text❱, but the ❰Natural/even❱ function
    └──────────────────┘  expects an argument that has type ❰Natural❱


    ┌────────────────────────┐
    │ (λ(a : Type) → a) True │  ❰True❱ has type ❰Bool❱, but the anonymous
    └────────────────────────┘  function expects an argument of kind ❰Type❱


    ┌────────┐
    │ List 1 │  ❰1❱ has type ❰Integer❱, but the ❰List❱ function expects an
    └────────┘  argument that has kind ❰Type❱


You tried to invoke the following function:

↳ $txt0

... which expects an argument of type or kind:

↳ $txt1

... on the following argument:

↳ $txt2

... which has a different type or kind:

↳ $txt3

Some common reasons why you might get this error:

● You omit a function argument by mistake:


    ┌────────────────────────────────────────┐
    │ List/head   ([1, 2, 3] : List Integer) │
    └────────────────────────────────────────┘
                ⇧
                ❰List/head❱ is missing the first argument,
                which should be: ❰Integer❱


● You supply an ❰Integer❱ literal to a function that expects a ❰Natural❱

    ┌────────────────┐
    │ Natural/even 2 │
    └────────────────┘
                   ⇧
                   This should be ❰+2❱
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)
        txt2 = Text.toStrict (Dhall.Core.pretty expr2)
        txt3 = Text.toStrict (Dhall.Core.pretty expr3)

prettyTypeMessage (AnnotMismatch expr0 expr1 expr2) = ErrorMessages {..}
  where
    short = "Expression doesn't match annotation"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: You can annotate an expression with its type or kind using the
❰:❱ symbol, like this:


    ┌───────┐
    │ x : t │  ❰x❱ is an expression and ❰t❱ is the annotated type or kind of ❰x❱
    └───────┘

The type checker verifies that the expression's type or kind matches the
provided annotation

For example, all of the following are valid annotations that the type checker
accepts:


    ┌─────────────┐
    │ 1 : Integer │  ❰1❱ is an expression that has type ❰Integer❱, so the type
    └─────────────┘  checker accepts the annotation


    ┌────────────────────────┐
    │ Natural/even +2 : Bool │  ❰Natural/even +2❱ has type ❰Bool❱, so the type
    └────────────────────────┘  checker accepts the annotation


    ┌────────────────────┐
    │ List : Type → Type │  ❰List❱ is an expression that has kind ❰Type → Type❱,
    └────────────────────┘  so the type checker accepts the annotation


    ┌──────────────────┐
    │ List Text : Type │  ❰List Text❱ is an expression that has kind ❰Type❱, so
    └──────────────────┘  the type checker accepts the annotation


However, the following annotations are $_NOT valid and the type checker will
reject them:


    ┌──────────┐
    │ 1 : Text │  The type checker rejects this because ❰1❱ does not have type
    └──────────┘  ❰Text❱


    ┌─────────────┐
    │ List : Type │  ❰List❱ does not have kind ❰Type❱
    └─────────────┘


You or the interpreter annotated this expression:

↳ $txt0

... with this type or kind:

↳ $txt1

... but the inferred type or kind of the expression is actually:

↳ $txt2

Some common reasons why you might get this error:

● The Haskell Dhall interpreter implicitly inserts a top-level annotation
  matching the expected type

  For example, if you run the following Haskell code:


    ┌───────────────────────────────┐
    │ >>> input auto "1" :: IO Text │
    └───────────────────────────────┘


  ... then the interpreter will actually type check the following annotated
  expression:


    ┌──────────┐
    │ 1 : Text │
    └──────────┘


  ... and then type-checking will fail
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)
        txt2 = Text.toStrict (Dhall.Core.pretty expr2)

prettyTypeMessage Untyped = ErrorMessages {..}
  where
    short = "❰Kind❱ has no type or kind"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: There are four levels of expressions that form a heirarchy:

● terms
● types
● kinds
● sorts

The following example illustrates this heirarchy:

    ┌────────────────────────────┐
    │ "ABC" : Text : Type : Kind │
    └────────────────────────────┘
       ⇧      ⇧      ⇧      ⇧
       term   type   kind   sort

There is nothing above ❰Kind❱ in this hierarchy, so if you try to type check any
expression containing ❰Kind❱ anywhere in the expression then type checking fails

Some common reasons why you might get this error:

● You supplied a kind where a type was expected

  For example, the following expression will fail to type check:

    ┌────────────────┐
    │ [] : List Type │
    └────────────────┘
                ⇧
                ❰Type❱ is a kind, not a type
|]

prettyTypeMessage (InvalidPredicate expr0 expr1) = ErrorMessages {..}
  where
    short = "Invalid predicate for ❰if❱"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: Every ❰if❱ expression begins with a predicate which must have type
❰Bool❱

For example, these are valid ❰if❱ expressions:


    ┌──────────────────────────────┐
    │ if True then "Yes" else "No" │
    └──────────────────────────────┘
         ⇧
         Predicate


    ┌─────────────────────────────────────────┐
    │ λ(x : Bool) → if x then False else True │
    └─────────────────────────────────────────┘
                       ⇧
                       Predicate


... but these are $_NOT valid ❰if❱ expressions:


    ┌───────────────────────────┐
    │ if 0 then "Yes" else "No" │  ❰0❱ does not have type ❰Bool❱
    └───────────────────────────┘


    ┌────────────────────────────┐
    │ if "" then False else True │  ❰""❱ does not have type ❰Bool❱
    └────────────────────────────┘


Your ❰if❱ expression begins with the following predicate:

↳ $txt0

... that has type:

↳ $txt1

... but the predicate must instead have type ❰Bool❱

Some common reasons why you might get this error:

● You might be used to other programming languages that accept predicates other
  than ❰Bool❱

  For example, some languages permit ❰0❱ or ❰""❱ as valid predicates and treat
  them as equivalent to ❰False❱.  However, the Dhall language does not permit
  this
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)

prettyTypeMessage (IfBranchMustBeTerm b expr0 expr1 expr2) =
    ErrorMessages {..}
  where
    short = "❰if❱ branch is not a term"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: Every ❰if❱ expression has a ❰then❱ and ❰else❱ branch, each of which
is an expression:


                   Expression for ❰then❱ branch
                   ⇩
    ┌────────────────────────────────┐
    │ if True then "Hello, world!"   │
    │         else "Goodbye, world!" │
    └────────────────────────────────┘
                   ⇧
                   Expression for ❰else❱ branch


These expressions must be a "term", where a "term" is defined as an expression
that has a type thas has kind ❰Type❱

For example, the following expressions are all valid "terms":


    ┌────────────────────┐
    │ 1 : Integer : Type │  ❰1❱ is a term with a type (❰Integer❱) of kind ❰Type❱
    └────────────────────┘
      ⇧
      term


    ┌─────────────────────────────────────┐
    │ Natural/odd : Natural → Bool : Type │  ❰Natural/odd❱ is a term with a type
    └─────────────────────────────────────┘  (❰Natural → Bool❱) of kind ❰Type❱
      ⇧
      term


However, the following expressions are $_NOT valid terms:


    ┌────────────────────┐
    │ Text : Type : Kind │  ❰Text❱ has kind (❰Type❱) of sort ❰Kind❱ and is
    └────────────────────┘  therefore not a term
      ⇧
      type


    ┌───────────────────────────┐
    │ List : Type → Type : Kind │  ❰List❱ has kind (❰Type → Type❱) of sort
    └───────────────────────────┘  ❰Kind❱ and is therefore not a term
      ⇧
      type-level function


This means that you cannot define an ❰if❱ expression that returns a type.  For
example, the following ❰if❱ expression is $_NOT valid:


    ┌─────────────────────────────┐
    │ if True then Text else Bool │  Invalid ❰if❱ expression
    └─────────────────────────────┘
                   ⇧         ⇧
                   type      type


Your ❰$txt0❱ branch of your ❰if❱ expression is:

↳ $txt1

... which has kind:

↳ $txt2

... of sort:

↳ $txt3

... and is not a term.  Therefore your ❰if❱ expression is not valid
|]
      where
        txt0 = if b then "then" else "else"
        txt1 = Text.toStrict (Dhall.Core.pretty expr0)
        txt2 = Text.toStrict (Dhall.Core.pretty expr1)
        txt3 = Text.toStrict (Dhall.Core.pretty expr2)

prettyTypeMessage (IfBranchMismatch expr0 expr1 expr2 expr3) =
    ErrorMessages {..}
  where
    short = "❰if❱ branches must have matching types"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: Every ❰if❱ expression has a ❰then❱ and ❰else❱ branch, each of which
is an expression:


                   Expression for ❰then❱ branch
                   ⇩
    ┌────────────────────────────────┐
    │ if True then "Hello, world!"   │
    │         else "Goodbye, world!" │
    └────────────────────────────────┘
                   ⇧
                   Expression for ❰else❱ branch


These two expressions must have the same type.  For example, the following ❰if❱
expressions are all valid:


    ┌──────────────────────────────────┐
    │ λ(b : Bool) → if b then 0 else 1 │ Both branches have type ❰Integer❱
    └──────────────────────────────────┘


    ┌────────────────────────────┐
    │ λ(b : Bool) →              │
    │     if b then Natural/even │ Both branches have type ❰Natural → Bool❱
    │          else Natural/odd  │
    └────────────────────────────┘


However, the following expression is $_NOT valid:


                   This branch has type ❰Integer❱
                   ⇩
    ┌────────────────────────┐
    │ if True then 0         │
    │         else "ABC"     │
    └────────────────────────┘
                   ⇧
                   This branch has type ❰Text❱


The ❰then❱ and ❰else❱ branches must have matching types, even if the predicate is
always ❰True❱ or ❰False❱

Your ❰if❱ expression has the following ❰then❱ branch:

↳ $txt0

... which has type:

↳ $txt2

... and the following ❰else❱ branch:

↳ $txt1

... which has a different type:

↳ $txt3

Fix your ❰then❱ and ❰else❱ branches to have matching types
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)
        txt2 = Text.toStrict (Dhall.Core.pretty expr2)
        txt3 = Text.toStrict (Dhall.Core.pretty expr3)

prettyTypeMessage (InvalidListType expr0) = ErrorMessages {..}
  where
    short = "Invalid type for ❰List❱ elements"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: Every ❰List❱ documents the type of its elements with a type
annotation, like this:


    ┌──────────────────────────┐
    │ [1, 2, 3] : List Integer │  A ❰List❱ of three ❰Integer❱s
    └──────────────────────────┘
                       ⇧
                       The type of the ❰List❱'s elements, which are ❰Integer❱s


    ┌───────────────────┐
    │ [] : List Integer │  An empty ❰List❱
    └───────────────────┘
                ⇧
                You still specify the type even when the ❰List❱ is empty


The element type must be a type and not something else.  For example, the
following element types are $_NOT valid:


    ┌──────────────┐
    │ ... : List 1 │
    └──────────────┘
                 ⇧
                 This is an ❰Integer❱ and not a ❰Type❱


    ┌─────────────────┐
    │ ... : List Type │
    └─────────────────┘
                 ⇧
                 This is a ❰Kind❱ and not a ❰Type❱


Even if the ❰List❱ is empty you still must specify a valid type

You declared that the ❰List❱'s elements should have type:

↳ $txt0

... which is not a ❰Type❱
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)

prettyTypeMessage (InvalidListElement i expr0 expr1 expr2) =
    ErrorMessages {..}
  where
    short = "List element has the wrong type"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: Every element in the list must have a type matching the type
annotation at the end of the list

For example, this is a valid ❰List❱:


    ┌──────────────────────────┐
    │ [1, 2, 3] : List Integer │  Every element in this ❰List❱ is an ❰Integer❱
    └──────────────────────────┘


.. but this is $_NOT a valid ❰List❱:


    ┌──────────────────────────────┐
    │ [1, "ABC", 3] : List Integer │  The second element is not an ❰Integer❱
    └──────────────────────────────┘


Your ❰List❱ elements should have this type:

↳ $txt0

... but the following element at index $txt1:

↳ $txt2

... has this type instead:

↳ $txt3
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty i    )
        txt2 = Text.toStrict (Dhall.Core.pretty expr1)
        txt3 = Text.toStrict (Dhall.Core.pretty expr2)

prettyTypeMessage (InvalidOptionalType expr0) = ErrorMessages {..}
  where
    short = "Invalid type for ❰Optional❱ element"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: Every optional element ends with a type annotation for the element
that might be present, like this:


    ┌────────────────────────┐
    │ [1] : Optional Integer │  An optional element that's present
    └────────────────────────┘
                     ⇧
                     The type of the ❰Optional❱ element, which is an ❰Integer❱


    ┌────────────────────────┐
    │ [] : Optional Integer  │  An optional element that's absent
    └────────────────────────┘
                    ⇧
                    You still specify the type even when the element is absent


The element type must be a type and not something else.  For example, the
following element types are $_NOT valid:


    ┌──────────────────┐
    │ ... : Optional 1 │
    └──────────────────┘
                     ⇧
                     This is an ❰Integer❱ and not a ❰Type❱


    ┌─────────────────────┐
    │ ... : Optional Type │
    └─────────────────────┘
                     ⇧
                     This is a ❰Kind❱ and not a ❰Type❱


Even if the element is absent you still must specify a valid type

You declared that the ❰Optional❱ element should have type:

↳ $txt0

... which is not a ❰Type❱

|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)

prettyTypeMessage (InvalidOptionalElement expr0 expr1 expr2) = ErrorMessages {..}
  where
    short = "❰Optional❱ element has the wrong type"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: An ❰Optional❱ element must have a type matching the type annotation

For example, this is a valid ❰Optional❱ value:


    ┌────────────────────────┐
    │ [1] : Optional Integer │  ❰1❱ is an ❰Integer❱, which matches the type
    └────────────────────────┘


... but this is $_NOT a valid ❰Optional❱ value:


    ┌────────────────────────────┐
    │ ["ABC"] : Optional Integer │  ❰"ABC"❱ is not an ❰Integer❱
    └────────────────────────────┘


Your ❰Optional❱ element should have this type:

↳ $txt0

... but the element you provided:

↳ $txt1

... has this type instead:

↳ $txt2
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)
        txt2 = Text.toStrict (Dhall.Core.pretty expr2)

prettyTypeMessage (InvalidOptionalLiteral n) = ErrorMessages {..}
  where
    short = "Multiple ❰Optional❱ elements not allowed"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: The syntax for ❰Optional❱ values resembles the syntax for ❰List❱s:


    ┌───────────────────────┐
    │ [] : Optional Integer │  An ❰Optional❱ value which is absent
    └───────────────────────┘


    ┌───────────────────────┐
    │ [] : List     Integer │  An empty (0-element) ❰List❱
    └───────────────────────┘


    ┌────────────────────────┐
    │ [1] : Optional Integer │  An ❰Optional❱ value which is present
    └────────────────────────┘


    ┌────────────────────────┐
    │ [1] : List     Integer │  A singleton (1-element) ❰List❱
    └────────────────────────┘


However, an ❰Optional❱ value can $_NOT have more than one element, whereas a
❰List❱ can have multiple elements:


    ┌───────────────────────────┐
    │ [1, 2] : Optional Integer │  Invalid: multiple elements $_NOT allowed
    └───────────────────────────┘


    ┌───────────────────────────┐
    │ [1, 2] : List     Integer │  Valid: multiple elements allowed
    └───────────────────────────┘


Your ❰Optional❱ value had this many elements:

↳ $txt0

... when an ❰Optional❱ value can only have at most one element

Some common reasons why you might get this error:

● You accidentally typed ❰Optional❱ when you meant ❰List❱, like this:


    ┌────────────────────────────────────────────────────┐
    │ List/length Integer ([1, 2, 3] : Optional Integer) │
    └────────────────────────────────────────────────────┘
                                       ⇧
                                       This should be ❰List❱ instead
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty n)

prettyTypeMessage (InvalidFieldType k expr0) = ErrorMessages {..}
  where
    short = "Invalid field type"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: Every record type documents the type of each field, like this:

    ┌──────────────────────────────────────────────┐
    │ { foo : Integer, bar : Integer, baz : Text } │
    └──────────────────────────────────────────────┘

However, fields cannot be annotated with expressions other than types

For example, these record types are $_NOT valid:


    ┌────────────────────────────┐
    │ { foo : Integer, bar : 1 } │
    └────────────────────────────┘
                             ⇧
                             ❰1❱ is an ❰Integer❱ and not a ❰Type❱


    ┌───────────────────────────────┐
    │ { foo : Integer, bar : Type } │
    └───────────────────────────────┘
                             ⇧
                             ❰Type❱ is a ❰Kind❱ and not a ❰Type❱


You provided a record type with a key named:

↳ $txt0

... annotated with the following expression:

↳ $txt1

... which is not a type
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty k    )
        txt1 = Text.toStrict (Dhall.Core.pretty expr0)

prettyTypeMessage (InvalidAlternativeType k expr0) = ErrorMessages {..}
  where
    short = "Invalid type of alternative"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: Every union type specifies the type of each alternative, like this:


               The type of the first alternative is ❰Bool❱
               ⇩
    ┌──────────────────────────────────┐
    │ < Left : Bool, Right : Natural > │  A union type with two alternatives
    └──────────────────────────────────┘
                             ⇧
                             The type of the second alternative is ❰Natural❱


However, these alternatives can only be annotated with types.  For example, the
following union types are $_NOT valid:


    ┌────────────────────────────┐
    │ < Left : Bool, Right : 1 > │  Invalid union type
    └────────────────────────────┘
                             ⇧
                             This is a term and not a type


    ┌───────────────────────────────┐
    │ < Left : Bool, Right : Type > │  Invalid union type
    └───────────────────────────────┘
                             ⇧
                             This is a kind and not a type


You provided a union type with an alternative named:

↳ $txt0

... annotated with the following expression which is not a type:

↳ $txt1

Some common reasons why you might get this error:

● You accidentally typed ❰:❱ instead of ❰=❱ for a union literal with one
  alternative:

    ┌─────────────────┐
    │ < Example : 1 > │
    └─────────────────┘
                ⇧
                This could be ❰=❱ instead
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty k    )
        txt1 = Text.toStrict (Dhall.Core.pretty expr0)

prettyTypeMessage (DuplicateAlternative k) = ErrorMessages {..}
  where
    short = "Duplicate union alternative"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: Unions may not have two alternatives that share the same name

For example, the following expressions are $_NOT valid:


    ┌─────────────────────────────┐
    │ < foo = True | foo : Text > │  Invalid: ❰foo❱ appears twice
    └─────────────────────────────┘


    ┌───────────────────────────────────────┐
    │ < foo = 1 | bar : Bool | bar : Text > │  Invalid: ❰bar❱ appears twice
    └───────────────────────────────────────┘


You have more than one alternative named:

↳ $txt0
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty k)

prettyTypeMessage (MustCombineARecord expr0 expr1) = ErrorMessages {..}
  where
    short = "You can only combine records"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: You can combine records using the ❰∧❱ operator, like this:


    ┌───────────────────────────────────────────┐
    │ { foo = 1, bar = "ABC" } ∧ { baz = True } │
    └───────────────────────────────────────────┘


    ┌─────────────────────────────────────────────┐
    │ λ(r : { foo : Bool }) → r ∧ { bar = "ABC" } │
    └─────────────────────────────────────────────┘


... but you cannot combine values that are not records.

For example, the following expressions are $_NOT valid:


    ┌──────────────────────────────┐
    │ { foo = 1, bar = "ABC" } ∧ 1 │
    └──────────────────────────────┘
                                 ⇧
                                 Invalid: Not a record


    ┌───────────────────────────────────────────┐
    │ { foo = 1, bar = "ABC" } ∧ { baz : Bool } │
    └───────────────────────────────────────────┘
                                 ⇧
                                 Invalid: This is a record type and not a record


    ┌───────────────────────────────────────────┐
    │ { foo = 1, bar = "ABC" } ∧ < baz = True > │
    └───────────────────────────────────────────┘
                                 ⇧
                                 Invalid: This is a union and not a record


You tried to combine the following value:

↳ $txt0

... which is not a record, but is actually a value of type:

↳ $txt1
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)

prettyTypeMessage (FieldCollision ks) = ErrorMessages {..}
  where
    short = "Field collision"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: You can combine records if they don't share any fields in common,
like this:


    ┌───────────────────────────────────────────┐
    │ { foo = 1, bar = "ABC" } ∧ { baz = True } │
    └───────────────────────────────────────────┘


    ┌────────────────────────────────────────┐
    │ λ(r : { baz : Bool}) → { foo = 1 } ∧ r │
    └────────────────────────────────────────┘


... but you cannot merge two records that share the same field

For example, the following expression is $_NOT valid:


    ┌───────────────────────────────────────────┐
    │ { foo = 1, bar = "ABC" } ∧ { foo = True } │  Invalid: Colliding ❰foo❱ fields
    └───────────────────────────────────────────┘


You combined two records that share the following field:

↳ $txt0

... which is not allowed

Some common reasons why you might get this error:

● You tried to use ❰∧❱ to update a field's value, like this:


    ┌────────────────────────────────────────┐
    │ { foo = 1, bar = "ABC" } ∧ { foo = 2 } │
    └────────────────────────────────────────┘
                                   ⇧
                                   Invalid attempt to update ❰foo❱'s value to ❰2❱

  Field updates are intentionally not allowed as the Dhall language discourages
  patch-oriented programming
|]
      where
        txt0 = Text.toStrict (Text.intercalate ", " (Data.Set.toList ks))

prettyTypeMessage (MustMergeARecord expr0 expr1) = ErrorMessages {..}
  where
    short = "❰merge❱ expects a record of handlers"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: You can ❰merge❱ the alternatives of a union using a record with one
handler per alternative, like this:


    ┌─────────────────────────────────────────────────────────────────────┐
    │     let union    = < Left = +2 | Right : Bool >                     │
    │ in  let handlers = { Left = Natural/even, Right = λ(x : Bool) → x } │
    │ in  merge handlers union : Bool                                     │
    └─────────────────────────────────────────────────────────────────────┘


... but the first argument to ❰merge❱ must be a record and not some other type.

For example, the following expression is $_NOT valid:


    ┌─────────────────────────────────────────┐
    │ let handler = λ(x : Bool) → x           │
    │ in  merge handler < Foo = True > : True │
    └─────────────────────────────────────────┘
                ⇧
                Invalid: ❰handler❱ isn't a record


You provided the following handler:

↳ $txt0

... which is not a record, but is actually a value of type:

↳ $txt1

Some common reasons why you might get this error:

● You accidentally provide an empty record type instead of an empty record when
  you ❰merge❱ an empty union:


    ┌──────────────────────────────────────────┐
    │ λ(x : <>) → λ(a : Type) → merge {} x : a │
    └──────────────────────────────────────────┘
                                      ⇧
                                      This should be ❰{=}❱ instead
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)

prettyTypeMessage (MustMergeUnion expr0 expr1) = ErrorMessages {..}
  where
    short = "❰merge❱ expects a union"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: You can ❰merge❱ the alternatives of a union using a record with one
handler per alternative, like this:


    ┌─────────────────────────────────────────────────────────────────────┐
    │     let union    = < Left = +2 | Right : Bool >                     │
    │ in  let handlers = { Left = Natural/even, Right = λ(x : Bool) → x } │
    │ in  merge handlers union : Bool                                     │
    └─────────────────────────────────────────────────────────────────────┘


... but the second argument to ❰merge❱ must be a union and not some other type.

For example, the following expression is $_NOT valid:


    ┌──────────────────────────────────────────┐
    │ let handlers = { Foo = λ(x : Bool) → x } │
    │ in  merge handlers True : True           │
    └──────────────────────────────────────────┘
                         ⇧
                         Invalid: ❰True❱ isn't a union


You tried to ❰merge❱ this expression:

↳ $txt0

... which is not a union, but is actually a value of type:

↳ $txt1
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)

prettyTypeMessage (UnusedHandler ks) = ErrorMessages {..}
  where
    short = "Unused handler"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: You can ❰merge❱ the alternatives of a union using a record with one
handler per alternative, like this:


    ┌─────────────────────────────────────────────────────────────────────┐
    │     let union    = < Left = +2 | Right : Bool >                     │
    │ in  let handlers = { Left = Natural/even, Right = λ(x : Bool) → x } │
    │ in  merge handlers union : Bool                                     │
    └─────────────────────────────────────────────────────────────────────┘


... but you must provide exactly one handler per alternative in the union.  You
cannot supply extra handlers

For example, the following expression is $_NOT valid:


    ┌───────────────────────────────────────┐
    │     let union    = < Left = +2 >      │  The ❰Right❱ alternative is missing
    │ in  let handlers =                    │ 
    │             { Left  = Natural/even    │
    │             , Right = λ(x : Bool) → x │  Invalid: ❰Right❱ handler isn't used
    │             }                         │
    │ in  merge handlers union : Bool       │
    └───────────────────────────────────────┘


You provided the following handlers:

↳ $txt0

... which had no matching alternatives in the union you tried to ❰merge❱
|]
      where
        txt0 = Text.toStrict (Text.intercalate ", " (Data.Set.toList ks))

prettyTypeMessage (MissingHandler ks) = ErrorMessages {..}
  where
    short = "Missing handler"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: You can ❰merge❱ the alternatives of a union using a record with one
handler per alternative, like this:


    ┌─────────────────────────────────────────────────────────────────────┐
    │     let union    = < Left = +2 | Right : Bool >                     │
    │ in  let handlers = { Left = Natural/even, Right = λ(x : Bool) → x } │
    │ in  merge handlers union : Bool                                     │
    └─────────────────────────────────────────────────────────────────────┘


... but you must provide exactly one handler per alternative in the union.  You
cannot omit any handlers

For example, the following expression is $_NOT valid:


                                              Invalid: Missing ❰Right❱ handler
                                              ⇩
    ┌─────────────────────────────────────────────────┐
    │     let handlers = { Left = Natural/even }      │
    │ in  let union    = < Left = +2 | Right : Bool > │
    │ in  merge handlers union : Bool                 │
    └─────────────────────────────────────────────────┘


Note that you need to provide handlers for other alternatives even if those
alternatives are never used

You need to supply the following handlers:

↳ $txt0
|]
      where
        txt0 = Text.toStrict (Text.intercalate ", " (Data.Set.toList ks))

prettyTypeMessage (HandlerInputTypeMismatch expr0 expr1 expr2) =
    ErrorMessages {..}
  where
    short = "Wrong handler input type"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: You can ❰merge❱ the alternatives of a union using a record with one
handler per alternative, like this:


    ┌─────────────────────────────────────────────────────────────────────┐
    │     let union    = < Left = +2 | Right : Bool >                     │
    │ in  let handlers = { Left = Natural/even, Right = λ(x : Bool) → x } │
    │ in  merge handlers union : Bool                                     │
    └─────────────────────────────────────────────────────────────────────┘


... as long as the input type of each handler function matches the type of the
corresponding alternative:


    ┌───────────────────────────────────────────────────────────┐
    │ union    : < Left : Natural       | Right : Bool        > │
    └───────────────────────────────────────────────────────────┘
                          ⇧                       ⇧
                   These must match        These must match
                          ⇩                       ⇩
    ┌───────────────────────────────────────────────────────────┐
    │ handlers : { Left : Natural → Bool, Right : Bool → Bool } │
    └───────────────────────────────────────────────────────────┘


For example, the following expression is $_NOT valid:


      Invalid: Doesn't match the type of the ❰Right❱ alternative
                                                               ⇩
    ┌──────────────────────────────────────────────────────────────────────┐
    │     let handlers = { Left = Natural/even | Right = λ(x : Text) → x } │
    │ in  let union    = < Left = +2 | Right : Bool >                      │
    │ in  merge handlers union : Bool                                      │
    └──────────────────────────────────────────────────────────────────────┘


Your handler for the following alternative:

↳ $txt0

... needs to accept an input value of type:

↳ $txt1

... but actually accepts an input value of a different type:

↳ $txt2
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)
        txt2 = Text.toStrict (Dhall.Core.pretty expr2)

prettyTypeMessage (HandlerOutputTypeMismatch expr0 expr1 expr2) =
    ErrorMessages {..}
  where
    short = "Wrong handler output type"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: You can ❰merge❱ the alternatives of a union using a record with one
handler per alternative, like this:


    ┌─────────────────────────────────────────────────────────────────────┐
    │     let union    = < Left = +2 | Right : Bool >                     │
    │ in  let handlers = { Left = Natural/even, Right = λ(x : Bool) → x } │
    │ in  merge handlers union : Bool                                     │
    └─────────────────────────────────────────────────────────────────────┘


... as long as the output type of each handler function matches the declared type
of the result:


    ┌───────────────────────────────────────────────────────────┐
    │ handlers : { Left : Natural → Bool, Right : Bool → Bool } │
    └───────────────────────────────────────────────────────────┘
                                    ⇧                    ⇧
                                    These output types ...

                             ... must match the declared type of the ❰merge❱
                             ⇩
    ┌─────────────────────────────┐
    │ merge handlers union : Bool │
    └─────────────────────────────┘


For example, the following expression is $_NOT valid:


    ┌──────────────────────────────────────────────────────────────────────┐
    │     let union    = < Left = +2 | Right : Bool >                     │
    │ in  let handlers = { Left = Natural/even, Right = λ(x : Bool) → x } │
    │ in  merge handlers union : Text                                      │
    └──────────────────────────────────────────────────────────────────────┘
                                 ⇧
                                 Invalid: Doesn't match output of either handler


Your handler for the following alternative:

↳ $txt0

... needs to return an output value of type:

↳ $txt1

... but actually returns an output value of a different type:

↳ $txt2
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)
        txt2 = Text.toStrict (Dhall.Core.pretty expr2)

prettyTypeMessage (HandlerNotAFunction k expr0) = ErrorMessages {..}
  where
    short = "Handler is not a function"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: You can ❰merge❱ the alternatives of a union using a record with one
handler per alternative, like this:


    ┌─────────────────────────────────────────────────────────────────────┐
    │     let union    = < Left = +2 | Right : Bool >                     │
    │ in  let handlers = { Left = Natural/even, Right = λ(x : Bool) → x } │
    │ in  merge handlers union : Bool                                     │
    └─────────────────────────────────────────────────────────────────────┘


... as long as each handler is a function

For example, the following expression is $_NOT valid:


    ┌─────────────────────────────────────────┐
    │ merge { Foo = True } < Foo = 1 > : Bool │
    └─────────────────────────────────────────┘
                    ⇧
                    Invalid: Not a function


Your handler for this alternative:

↳ $txt0

... has the following type:

↳ $txt1

... which is not the type of a function
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty k)
        txt1 = Text.toStrict (Dhall.Core.pretty expr0)

prettyTypeMessage (NotARecord k expr0 expr1) = ErrorMessages {..}
  where
    short = "Not a record"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: You can only access fields on records, like this:


    ┌─────────────────────────────────┐
    │ { foo = True, bar = "ABC" }.foo │  This is valid ...
    └─────────────────────────────────┘


    ┌───────────────────────────────────────────┐
    │ λ(r : { foo : Bool, bar : Text }) → r.foo │  ... and so is this
    └───────────────────────────────────────────┘


... but you cannot access fields on non-record expressions

For example, the following expression is $_NOT valid:


    ┌───────┐
    │ 1.foo │
    └───────┘
      ⇧
      Invalid: Not a record


You tried to access a field named:

↳ $txt0

... on the following expression which is not a record:

↳ $txt1

... but is actually an expression of type:

↳ $txt2

Some common reasons why you might get this error:

● You accidentally try to access a field of a union instead of a record, like
  this:


    ┌─────────────────┐
    │ < foo : a >.foo │
    └─────────────────┘
      ⇧
      This is a union, not a record
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty k    )
        txt1 = Text.toStrict (Dhall.Core.pretty expr0)
        txt2 = Text.toStrict (Dhall.Core.pretty expr1)

prettyTypeMessage (MissingField k expr0) = ErrorMessages {..}
  where
    short = "Missing record field"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: You can only access fields on records, like this:


    ┌─────────────────────────────────┐
    │ { foo = True, bar = "ABC" }.foo │  This is valid ...
    └─────────────────────────────────┘


    ┌───────────────────────────────────────────┐
    │ λ(r : { foo : Bool, bar : Text }) → r.foo │  ... and so is this
    └───────────────────────────────────────────┘


... but you can only access fields if they are present

For example, the following expression is $_NOT valid:

    ┌─────────────────────────────────┐
    │ { foo = True, bar = "ABC" }.qux │
    └─────────────────────────────────┘
                                  ⇧
                                  Invalid: the record has no ❰qux❱ field

You tried to access a field named:

↳ $txt0

... but the field is missing because the record only defines the following fields:

↳ $txt1
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty k    )
        txt1 = Text.toStrict (Dhall.Core.pretty expr0)

prettyTypeMessage (CantAnd expr0 expr1) =
        buildBooleanOperator "&&" expr0 expr1

prettyTypeMessage (CantOr expr0 expr1) =
        buildBooleanOperator "||" expr0 expr1

prettyTypeMessage (CantEQ expr0 expr1) =
        buildBooleanOperator "==" expr0 expr1

prettyTypeMessage (CantNE expr0 expr1) =
        buildBooleanOperator "/=" expr0 expr1

prettyTypeMessage (CantTextAppend expr0 expr1) = ErrorMessages {..}
  where
    short = "❰++❱ only works on ❰Text❱"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: The ❰++❱ operator expects two arguments that have type ❰Text❱

For example, this is a valid use of ❰++❱: 


    ┌────────────────┐
    │ "ABC" ++ "DEF" │
    └────────────────┘


You provided this argument:

↳ $txt0

... which does not have type ❰Text❱ but instead has type:

↳ $txt1

Some common reasons why you might get this error:

● You might have thought that ❰++❱ was the operator to combine two lists:

    ┌───────────────────────────────────────────────────────────┐
    │ ([1, 2, 3] : List Integer) ++ ([4, 5, 6] : List Integer ) │  Not valid
    └───────────────────────────────────────────────────────────┘

  The Dhall programming language does not provide a built-in operator for
  combining two lists
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)

prettyTypeMessage (CantAdd expr0 expr1) =
        buildNaturalOperator "+" expr0 expr1

prettyTypeMessage (CantMultiply expr0 expr1) =
        buildNaturalOperator "*" expr0 expr1

prettyTypeMessage (NoDependentTypes expr0 expr1) = ErrorMessages {..}
  where
    short = "No dependent types"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: The Dhall programming language does not allow functions from terms
to types.  These function types are also known as "dependent function types"
because you have a type whose value "depends" on the value of a term.

For example, this is $_NOT a legal function type:


    ┌─────────────┐
    │ Bool → Type │
    └─────────────┘


Similarly, this is $_NOT legal code:


    ┌────────────────────────────────────────────────────┐
    │ λ(Vector : Natural → Type → Type) → Vector +0 Text │
    └────────────────────────────────────────────────────┘
                 ⇧
                 Invalid dependent type


Your function type is invalid because the input has type:

↳ $txt0

... and the output has kind:

↳ $txt1

... which makes this a forbidden dependent function type
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)

prettyTypeMessage (NoDependentLet expr0 expr1) = ErrorMessages {..}
  where
    short = "No dependent ❰let❱"

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: The Dhall programming language does not allow ❰let❱ expressions
from terms to types.  These ❰let❱ expressions are also known as "dependent ❰let❱
expressions" because you have a type whose value depends on the value of a term.

The Dhall language forbids these dependent ❰let❱ expressions in order to
guarantee that ❰let❱ expressions of the form:


    ┌────────────────────┐
    │ let x : t = r in e │
    └────────────────────┘


... are always equivalent to:


    ┌──────────────────┐
    │ (λ(x : t) → e) r │
    └──────────────────┘


This means that both expressions should normalize to the same result and if one
of the two fails to type check then the other should fail to type check, too.

For this reason, the following is $_NOT legal code:


    ┌───────────────────┐
    │ let x = 2 in Text │
    └───────────────────┘


... because the above ❰let❱ expression is equivalent to:


    ┌─────────────────────────────┐
    │ let x : Integer = 2 in Text │
    └─────────────────────────────┘


... which in turn must be equivalent to:


    ┌───────────────────────────┐
    │ (λ(x : Integer) → Text) 2 │
    └───────────────────────────┘


... which in turn fails to type check because this sub-expression:


    ┌───────────────────────┐
    │ λ(x : Integer) → Text │
    └───────────────────────┘


... has type:


    ┌───────────────────────┐
    │ ∀(x : Integer) → Text │
    └───────────────────────┘


... which is a forbidden dependent function type (i.e. a function from a term to
a type).  Therefore the equivalent ❰let❱ expression is also forbidden.

Your ❰let❱ expression is invalid because the input has type:

↳ $txt0

... and the output has kind:

↳ $txt1

... which makes this a forbidden dependent ❰let❱ expression
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)

buildBooleanOperator :: Text -> Expr s X -> Expr s X -> ErrorMessages
buildBooleanOperator operator expr0 expr1 = ErrorMessages {..}
  where
    short =
        Builder.fromText
            (Data.Text.strip
                [NeatInterpolation.text|❰$txt2❱ only works on ❰Bool❱s|] )

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: The ❰$txt2❱ operator expects two arguments that have type ❰Bool❱

For example, this is a valid use of ❰$txt2❱: 


    ┌───────────────┐
    │ True $txt2 False │
    └───────────────┘


You provided this argument:

↳ $txt0

... which does not have type ❰Bool❱ but instead has type:

↳ $txt1
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)

    txt2 = Text.toStrict operator

buildNaturalOperator :: Text -> Expr s X -> Expr s X -> ErrorMessages
buildNaturalOperator operator expr0 expr1 = ErrorMessages {..}
  where
    short =
        Builder.fromText
            (Data.Text.strip
                [NeatInterpolation.text|❰$txt2❱ only works on ❰Natural❱s|] )

    long =
        Builder.fromText [NeatInterpolation.text|
Explanation: The ❰$txt2❱ operator expects two arguments that have type ❰Natural❱

For example, this is a valid use of ❰$txt2❱: 


    ┌─────────┐
    │ +3 $txt2 +5 │
    └─────────┘


You provided this argument:

↳ $txt0

... which does not have type ❰Natural❱ but instead has type:

↳ $txt1

Some common reasons why you might get this error:

● You might have tried to use an ❰Integer❱, which is $_NOT allowed:


    ┌─────────────────────────────────────────┐
    │ λ(x : Integer) → λ(y : Integer) → x $txt2 y │  Not valid
    └─────────────────────────────────────────┘


  You can only use ❰Natural❱ numbers


● You might have mistakenly used an ❰Integer❱ literal, which is $_NOT allowed:


    ┌───────┐
    │ 2 $txt2 2 │  Not valid
    └───────┘


  You need to prefix each literal with a ❰+❱ to transform them into ❰Natural❱
  literals, like this:


    ┌─────────┐
    │ +2 $txt2 +2 │  Valid
    └─────────┘
|]
      where
        txt0 = Text.toStrict (Dhall.Core.pretty expr0)
        txt1 = Text.toStrict (Dhall.Core.pretty expr1)

    txt2 = Text.toStrict operator

-- | A structured type error that includes context
data TypeError s = TypeError
    { context     :: Context (Expr s X)
    , current     :: Expr s X
    , typeMessage :: TypeMessage s
    } deriving (Typeable)

instance Buildable s => Show (TypeError s) where
    show = Text.unpack . Dhall.Core.pretty

instance (Buildable s, Typeable s) => Exception (TypeError s)

instance Buildable s => Buildable (TypeError s) where
    build (TypeError ctx expr msg)
        =   (   if  Text.null (Builder.toLazyText (buildContext ctx))
                then ""
                else buildContext ctx <> "\n"
            )
        <>  shortTypeMessage msg <> "\n"
        <>  source
      where
        buildKV (key, val) = build key <> " : " <> build val

        buildContext =
                build
            .   Text.unlines
            .   map (Builder.toLazyText . buildKV)
            .   reverse
            .   Dhall.Context.toList

        source = case expr of
            Note s _ -> build s
            _        -> mempty

newtype DetailedTypeError s = DetailedTypeError (TypeError s)
    deriving (Typeable)

instance Buildable s => Show (DetailedTypeError s) where
    show = Text.unpack . Dhall.Core.pretty

instance (Buildable s, Typeable s) => Exception (DetailedTypeError s)

instance Buildable s => Buildable (DetailedTypeError s) where
    build (DetailedTypeError (TypeError ctx expr msg))
        =   (   if  Text.null (Builder.toLazyText (buildContext ctx))
                then ""
                else buildContext ctx <> "\n"
            )
        <>  longTypeMessage msg <> "\n"
        <>  "────────────────────────────────────────────────────────────────────────────────\n"
        <>  "\n"
        <>  source
      where
        buildKV (key, val) = build key <> " : " <> build val

        buildContext =
                build
            .   Text.unlines
            .   map (Builder.toLazyText . buildKV)
            .   reverse
            .   Dhall.Context.toList

        source = case expr of
            Note s _ -> build s
            _        -> mempty
