{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

{-| This module exports the `dhallToToml` function for translating a
    Dhall syntax tree to a TOML syntax tree (`TOML`) for the @tomland@
    library.

    For converting source code into a Dhall syntax tree see the @dhall@
    package, and for converting the TOML syntax tree to source code see
    the @tomland@ package.

    This module also exports `dhallToTomlMain` which implements the
    @dhall-to-toml@ command which converts Dhall source directly into
    TOML source.

    Not all Dhall expressions can be converted to TOML since TOML is not a
    programming language. The only things you can convert are:

    * @Bool@s
    * @Natural@s
    * @Integer@s
    * @Double@s
    * @Text@ values
    * @List@s
    * @Optional@ values
    * unions
    * records

    Additionally the Dhall top-level value being converted **must be a record**
    since TOML cannot represent bare values (ex. a single boolean or integer)

    Dhall @Bool@s translates to TOML bools:

> $ dhall-to-toml <<< ' { t = True, f = False }'
> f = false
> t = true

    Dhall numbers translate to TOML numbers:

> $ dhall-to-toml <<< '{ i = 1, d = 1.2 }'
> d = 1.2
> i = 1

    Dhall @Text@ translates to TOML text:

> $ dhall-to-toml <<< '{ t = "Hello!" }'
> t = "Hello!"

    Dhall @List@s of records translates to TOML array of tables:

> $ dhall-to-toml <<< '{ l = [ { a = 1 } , { a = 2 }] }'
> [[l]]
>   a = 1
>
> [[l]]
>   a = 2

    All other @List@s are translated to TOML inline lists:

> $ dhall-to-toml <<< '{ l1 = [1, 2, 3], l2 = [[1, 1], [2, 2]] }'
> l1 = [1, 2, 3]
> l2 = [[1, 1], [2, 2]]

    Note, [lists of lists of objects are currently not supported](https://github.com/kowainik/tomland/issues/373), for example, @[[{a = 1}]]@ will not be converted.

    Dhall @Optional@ values are ignored if @None@ or the unwraped value if @Some@

> $ dhall-to-toml <<< '{ n = None Natural, s = Some 1 }'
> s = 1

    Dhall records translate to TOML tables:

> $ dhall-to-toml <<< '{ v = 1, r1 = { a = 1, b = 2, nested = { a = 3 } } }'
> v = 1
>
> [r]
>   a = 1
>   b = 2
>
>   [r.nested]
>     c = 3

    Dhall unions translate to the wrapped value, or a string if the alternative is empty:

> $ dhall-to-toml <<< '{ u = < A | B >.A }'
> u = "A"
> $ dhall-to-toml <<< '{ u = < A : Natural | B >.A 10}'
> u = 10

    Also, all Dhall expressions are normalized before translation:

> $ dhall-to-toml <<< ' { b = True == False }'
> b = false

-}

module Dhall.DhallToToml
    ( -- * Dhall To TOML
      dhallToToml
    , dhallToTomlMain
    -- * Exceptions
    , CompileError
    ) where

import Control.Exception  (Exception)
import Control.Monad      (foldM)
import Data.Foldable      (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text          (Text)
import Data.Version       (showVersion)
import Data.Void          (Void)
import Dhall.Core         (DhallDouble (..), Expr)
import Dhall.Map          (Map)
import Dhall.Toml.Utils   (fileToDhall, inputToDhall)
import Prettyprinter      (Pretty)
import Toml.Type.Key      (Key(..), Piece (Piece))
import Toml.Type.AnyValue (AnyValue(..))
import Toml.Type.TOML     (TOML)

import qualified Data.List.NonEmpty        as NonEmpty
import qualified Data.Sequence             as Seq
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text.IO
import qualified Dhall.Core                as Core
import qualified Dhall.Map                 as Map
import qualified Dhall.Pretty
import qualified Dhall.Util
import qualified Options.Applicative       as Options
import qualified Paths_dhall_toml          as Meta
import qualified Prettyprinter.Render.Text as Pretty
import qualified Toml.Type.AnyValue        as AnyValue
import qualified Toml.Type.Printer         as Printer
import qualified Toml.Type.TOML            as TOML
import qualified Toml.Type.Value           as Value

-- $setup
--
-- >>> import Toml.Type.TOML (TOML(..))
-- >>> import Toml.Type.AnyValue (AnyValue(..))
-- >>> import qualified Data.HashMap.Strict as HashMap

data CompileError
    = Unsupported (Expr Void Void)
    -- | tomland does not support records in multi-dimensional arrays, though it
    --   is allowed by the spec
    | UnsupportedArray (Expr Void Void)
    | NotARecord (Expr Void Void)
    -- | the latest TOML spec, v1.0.0 allows this but tomland has not
    --   implemented it yet
    --   NOTE: the only way to get this error is through unions
    | HeterogeneousArray (Expr Void Void)
    deriving (Eq)

instance Show CompileError where
    show (Unsupported e) =
        _ERROR <> ": Cannot translate to TOML                                            \n\
        \                                                                                \n\
        \                                                                                \n\
        \Explanation: Only primitive values, records, unions, ❰List❱s, and ❰Optional❱    \n\
        \values can be translated from Dhall to TOML                                     \n\
        \                                                                                \n\
        \The following Dhall expression could not be translated to TOML:                 \n\
        \                                                                                \n\
        \" <> insert e

    show (UnsupportedArray e) =
        _ERROR <> ": Records cannot be nested in multi-dimentional arrays                \n\
        \                                                                                \n\
        \Explanation: The tomland library cannot handle records in nested arrays. You    \n\
        \can check the status of this feature at:                                        \n\
        \   https://github.com/kowainik/tomland/issues/385                               \n\
        \                                                                                \n\
        \For example:                                                                    \n\
        \    ┌─────────────────────────┐                                                 \n\
        \    | { x = [[ { a = 1 } ]] } |                                                 \n\
        \    └─────────────────────────┘                                                 \n\
        \                                                                                \n\
        \" <> insert e

    show (NotARecord e) =
        _ERROR <> ": The root object converted to TOML must be a record                  \n\
        \                                                                                \n\
        \Explanation: A TOML file must represent a table, so primitive values and        \n\
        \❰List❱s cannot be converted by themselves. Consider nesting the value in a      \n\
        \record with arbitrary fields.                                                   \n\
        \                                                                                \n\
        \For example, from:                                                              \n\
        \    ┌────┐                                                                      \n\
        \    | 42 |                                                                      \n\
        \    └────┘                                                                      \n\
        \into                                                                            \n\
        \    ┌────────────────────────┐                                                  \n\
        \    | { meaningOfLife = 42 } |                                                  \n\
        \    └────────────────────────┘                                                  \n\
        \                                                                                \n\
        \" <> insert e

    show (HeterogeneousArray e) =
        _ERROR <> ": Heterogeneous arrays are not currently supported                    \n\
        \                                                                                \n\
        \Explanation: The tomland library cannot handle arrays with elements of          \n\
        \ different types. You can check the status of this feature at:                  \n\
        \   https://github.com/kowainik/tomland/issues/373                               \n\
        \                                                                                \n\
        \For example:                                                                    \n\
        \    ┌────────────────────────────────────┐                                      \n\
        \    | let X = < A : Natural | B : Bool > |                                      \n\
        \    | in { x = [ X.A 10, X.B false ] }   |                                      \n\
        \    └────────────────────────────────────┘                                      \n\
        \                                                                                \n\
        \" <> insert e

instance Exception CompileError


_ERROR :: String
_ERROR = Text.unpack $ Dhall.Util._ERROR

insert :: Pretty a => a -> String
insert = Text.unpack . Pretty.renderStrict . Dhall.Pretty.layout . Dhall.Util.insert

{-| Converts a Dhall expression into a @tomland@ TOML expression

>>> :set -XOverloadedStrings
>>> :set -XOverloadedLists
>>> import Dhall.Core
>>> import Toml.Type.Printer
>>> f = makeRecordField
>>> let toml = dhallToToml $ RecordLit [("foo", f $ NaturalLit 1), ("bar", f $ TextLit "ABC")]
>>> toml == Right (TOML {tomlPairs = HashMap.fromList [("foo",AnyValue (Value.Integer 1)),("bar",AnyValue (Value.Text "ABC"))], tomlTables = HashMap.fromList [], tomlTableArrays = HashMap.fromList []})
True
>>> fmap Toml.Type.Printer.pretty toml
Right "bar = \"ABC\"\nfoo = 1\n"
-}
dhallToToml :: Expr s Void -> Either CompileError TOML
dhallToToml expression = do
    record <- assertRecordLit (Core.normalize expression)
    toTomlTable record

-- empty union alternative like < A | B >.A
pattern UnionEmpty :: Text -> Expr s a
pattern UnionEmpty x <- Core.Field (Core.Union _) (Core.FieldSelection _ x _)
-- union alternative with type like < A : Natural | B>.A 1
pattern UnionApp :: Expr s a -> Expr s a
pattern UnionApp x <- Core.App (Core.Field (Core.Union _) _) x

assertRecordLit
    :: Expr Void Void
    -> Either CompileError (Map Text (Core.RecordField Void Void))
assertRecordLit (Core.RecordLit r) = Right r
assertRecordLit (UnionApp x)       = assertRecordLit x
assertRecordLit e                  = Left $ NotARecord e

toTomlTable :: Map Text (Core.RecordField Void Void) -> Either CompileError TOML
toTomlTable r = foldM (toTomlRecordFold []) (mempty :: TOML) (Map.toList r)

toTomlRecordFold
    :: [Piece]
    -> TOML
    -> (Text, Core.RecordField Void Void)
    -> Either CompileError TOML
toTomlRecordFold curKey toml (key, val) =
    toToml toml (Piece key :| curKey) (Core.recordFieldValue val)

toToml :: TOML -> NonEmpty Piece -> Expr Void Void -> Either CompileError TOML
toToml toml pieces expr  = case expr of
    Core.BoolLit a ->
        insertPrim (Value.Bool a)

    Core.NaturalLit a ->
        insertPrim (Value.Integer (toInteger a))

    Core.IntegerLit a ->
        insertPrim (Value.Integer a)

    Core.DoubleLit (DhallDouble a) ->
        insertPrim (Value.Double a)

    Core.TextLit (Core.Chunks [] a) ->
        insertPrim (Value.Text a)

    UnionEmpty a ->
        insertPrim (Value.Text a)

    UnionApp a ->
        toToml toml pieces a

    Core.Some a ->
        toToml toml pieces a

    Core.App Core.None _ ->
        return toml

    Core.ListLit _ a -> case toList a of
        -- TODO: unions need to be handled here as well, it's a bit tricky
        -- because they also have to be probed for being a "simple"
        -- array of table
        union@(UnionApp (Core.RecordLit _)) : unions -> do
            insertTables (union :| unions)

        record@(Core.RecordLit _) : records -> do
            insertTables (record :| records)

        -- inline array
        expressions -> do
            anyValues <- mapM toAnyValue expressions

            case AnyValue.toMArray anyValues of
                Left _ -> Left (HeterogeneousArray expr)
                Right array -> insertPrim array

    Core.RecordLit r -> do
        let (inline, nested) =
                Map.partition (isInline . Core.recordFieldValue) r

        -- the order here is important, at least for testing, because the
        -- PrefixMap inside TOML is dependent on insert order
        let pairs = Map.toList inline <> Map.toList nested

        if null inline
        -- if the table doesn't have inline elements, don't register the table,
        -- only its non-inlined children. Ex:
        -- [a] # bad
        --   [b]
        --     c = 1
        -- [a.b] # good
        --   c = 1
        then do
            foldM (toTomlRecordFold (toList pieces)) toml pairs
        else do
            newPairs <- foldM (toTomlRecordFold []) mempty pairs
            return (TOML.insertTable key newPairs toml)
    _ ->
        Left (Unsupported expr)
  where
    key :: Key
    key = Key (NonEmpty.reverse pieces)

    insertPrim :: Value.Value a -> Either CompileError TOML
    insertPrim val = return (TOML.insertKeyVal key val toml)

    insertTables :: NonEmpty (Expr Void Void) -> Either CompileError TOML
    insertTables expressions = do
        tables <- case mapM assertRecordLit expressions of
            Right x -> mapM toTomlTable x
            Left (NotARecord e) -> Left (HeterogeneousArray e)
            Left x -> Left x
        return (TOML.insertTableArrays key tables toml)

    -- checks if the value should be represented as an inline key/value pair.
    -- Elements that are inlined are those that do not have a [header] or
    -- [[header]]. One edge case is tables within multiple arrays, though not
    -- currently supported by tomland, can only be represented as inline tables.
    isInline v = case v of
        Core.BoolLit _    -> True
        Core.IntegerLit _ -> True
        Core.NaturalLit _ -> True
        Core.DoubleLit _  -> True
        Core.TextLit _    -> True
        Core.ListLit _ s  -> case Seq.lookup 0 s of
            Nothing                  -> True
            Just (Core.BoolLit _)    -> True
            Just (Core.NaturalLit _) -> True
            Just (Core.DoubleLit _)  -> True
            Just (Core.TextLit _)    -> True
            Just (Core.ListLit _ _)  -> True
            _                        -> False
        _ -> False

    -- toAnyValue is a helper function for making lists so it returns a list
    -- specific error, in particular tomland's inability to represent tables in
    -- multi-dimensional arrays
    toAnyValue :: Expr Void Void -> Either CompileError AnyValue
    toAnyValue expression = case expression of
        Core.BoolLit x ->
            Right (AnyValue (Value.Bool x))
        Core.IntegerLit x ->
            Right (AnyValue (Value.Integer x))
        Core.NaturalLit x ->
            Right (AnyValue (Value.Integer (toInteger x)))
        Core.DoubleLit (DhallDouble x) ->
            Right (AnyValue (Value.Double x))
        Core.TextLit (Core.Chunks [] x) ->
            Right (AnyValue (Value.Text x))
        UnionEmpty x ->
            Right (AnyValue (Value.Text x))
        UnionApp x ->
            toAnyValue x
        Core.ListLit _ x -> do
            anyList <- mapM toAnyValue (toList x)
            case AnyValue.toMArray anyList of
                Right x' -> Right (AnyValue x')
                Left _   -> Left (HeterogeneousArray expr)
        Core.RecordLit _ ->
            Left (UnsupportedArray expression)
        _ ->
            Left (Unsupported expression)

data Options = Options
    { input :: Maybe FilePath
    , output :: Maybe FilePath
    }

parserInfo :: Options.ParserInfo Options
parserInfo = Options.info
    (Options.helper <*> versionOption <*> optionsParser)
    (Options.fullDesc <> Options.progDesc "Convert Dhall to TOML")
  where
    versionOption =
        Options.infoOption (showVersion Meta.version)
            (Options.long "version" <> Options.help "Display version")

    optionsParser = do
        input <- (Options.optional . Options.strOption)
            (  Options.long "file"
            <> Options.help "Read Dhall from file instead of standard input"
            <> Options.metavar "FILE"
            <> Options.action "file"
            )

        output <- (Options.optional . Options.strOption)
            (  Options.long "output"
            <> Options.help "Write TOML to a file instead of standard output"
            <> Options.metavar "FILE"
            <> Options.action "file"
            )

        pure Options{..}

{-| Runs the @dhall-to-toml@ command
-}
dhallToTomlMain :: IO ()
dhallToTomlMain = do
    Options{..} <- Options.execParser parserInfo

    resolvedExpression <- maybe inputToDhall fileToDhall input

    toml <- Core.throws (dhallToToml resolvedExpression)

    let text = Printer.pretty toml

    case output of
        Just file -> Text.IO.writeFile file text
        Nothing   -> Text.IO.putStrLn text
