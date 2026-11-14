# Shared Dhall Core: a code-centered guide

Status: this document explains the design on `feature/shared-dhall-core`.
It describes what the code does today, including the parts that are
experimental or disabled by default.

The shorter decision records are:

- [001-shared-core.md](001-shared-core.md): why the shared representation was chosen
- [002-test-matrix.md](002-test-matrix.md): behavior that the tests compare
- [003-dag-cbor.md](003-dag-cbor.md): the experimental binary format
- [004-format-migration.md](004-format-migration.md): steps required before that
  binary format can become the default

## 1. The problem, shown in code

Consider this Haskell value:

```haskell
one :: Expr Void Void
one = NaturalLit 1

expression :: Expr Void Void
expression = NaturalPlus one one
```

`expression` has three occurrences:

```text
NaturalPlus
├── NaturalLit 1
└── NaturalLit 1
```

The two children have exactly the same contents. In a large Dhall normal form,
the repeated part may be a record or union with thousands of fields rather
than the small `NaturalLit 1` above.

The old representation describes this shape as a recursive tree. A program
walking that tree visits both copies. Rebuilding a large repeated value can
also allocate both copies.

The new representation stores each distinct piece once:

```text
0: NaturalLit 1
1: NaturalPlus 0 0
root: 1
```

Here `0` and `1` are positions in an indexed collection. Both children of the
addition point to position `0`.

This shape is called a *directed acyclic graph*, usually abbreviated to
**DAG**:

- **graph**: entries may point to other entries;
- **directed**: a parent points to its children;
- **acyclic**: following child references can never return to the starting
  entry.

The implementation calls this representation `CoreGraph`.

## 2. The main types

The essential definitions are in `Dhall.Core.DAG.Node` and
`Dhall.Core.DAG`. The following is a shortened version of the real code:

```haskell
newtype NodeId = NodeId { unNodeId :: Word32 }

data CoreNode a
    = CVar Var
    | CApp NodeId NodeId
    | CNaturalLit Natural
    | CNaturalPlus NodeId NodeId
    | CLam Text NodeId NodeId
    -- one case for each kind of denoted Dhall expression

data CoreGraph a = CoreGraph
    { graphRoot  :: NodeId
    , graphNodes :: Vector (CoreNode a)
    }
```

A `NodeId` is simply an index into `graphNodes`. It is not a hash and it is not
a permanent identifier. `Word32` is Haskell's type for an unsigned 32-bit
integer.

The type parameter `a` is the value carried by Dhall's `Embed` case. The type
parameter `s` used by `Expr s a` is source information, such as a location in
the input file. `Void` means that the corresponding kind of value is absent.

For the earlier addition example, the value is conceptually:

```haskell
CoreGraph
    { graphRoot = NodeId 1
    , graphNodes =
        [ CNaturalLit 1
        , CNaturalPlus (NodeId 0) (NodeId 0)
        ]
    }
```

The actual `CoreGraph` constructor is kept private. Callers use functions such
as `fromExpr`, `lookupNode`, `rootId`, `foldUnique`, and `toSharedExpr`.

### Why `NodeId` cannot be copied between graphs

`NodeId 0` only means “the first entry in this graph.” In another graph the
first entry can contain something unrelated.

Therefore, combining two graphs does not copy their numbers. `internGraph`
reads each reachable node from the source graph, copies its contents into the
destination builder, and receives new numbers belonging to the destination.

## 3. Building a graph, one node at a time

Graph construction uses a `Builder`:

```haskell
data BuilderState a = BuilderState
    { bsNodes   :: Seq (CoreNode a)
    , bsTable   :: HashMap (CoreNode a) Word32
    , bsSharing :: Bool
    }
```

`bsNodes` is the growing sequence of nodes. `bsTable` answers the question:
“Have we already stored a node with these contents?”

The operation that adds a node is named `intern`:

```haskell
intern :: Eq a => Builder s a -> CoreNode a -> ST s NodeId
intern builder node =
    case HashMap.lookup node existingNodes of
        Just oldIndex ->
            return (NodeId oldIndex)

        Nothing -> do
            let newIndex = length nodes
            append node
            remember node newIndex
            return (NodeId newIndex)
```

This sample omits bookkeeping, but shows the decision:

1. Build the children first.
2. Construct a `CoreNode` containing the children's `NodeId`s.
3. Look for an equal `CoreNode`.
4. Reuse its index if it exists.
5. Otherwise append the new node and return its new index.

“Hash-consing” is the traditional name for this lookup-and-reuse technique.
In this document it is simply called **structural sharing**.

The hash used by `HashMap` only makes lookup faster. Correctness does not
depend on two hashes being different: `HashMap` also compares the complete
`CoreNode` values for equality.

After construction, `freeze` converts the growing sequence to an immutable
`Vector` and discards the lookup table:

```haskell
freeze :: Builder s a -> NodeId -> ST s (CoreGraph a)
```

The word `ST` in these signatures means that Haskell permits local mutation
while building the value, but prevents that mutable state from escaping.
Users of a completed `CoreGraph` see an immutable value.

### What counts as “the same node”

The builder shares exact *denoted* syntax. `denote` removes source locations
(`Note` and `Src`) and printing preferences that do not affect meaning.

The builder does not silently rename bound variables. For example, these
lambdas are equivalent after renaming but are not the same exact node:

```dhall
λ(x : Natural) → x
λ(y : Natural) → y
```

`alphaNormalizeGraph` performs that renaming explicitly when it is wanted.

## 4. Converting between `Expr` and `CoreGraph`

The public `Expr` type remains unchanged. Parsing, formatting, source
locations, error messages, Template Haskell, and existing client code still
use it.

To enter the shared representation:

```haskell
fromExpr :: Eq a => Expr s a -> CoreGraph a
```

To return to the existing API:

```haskell
toSharedExpr :: CoreGraph a -> Expr Void a
```

`toSharedExpr` creates one `Expr` value for every graph node:

```haskell
decoded = Vector.generate nodeCount $ \index ->
    decode (graphNodes Vector.! index)

at (NodeId index) = decoded Vector.! fromIntegral index
```

When two parent fields contain the same `NodeId`, `at` returns the same
already-created Haskell value. The resulting `Expr` therefore shares memory
even though its type still looks like a recursive tree.

There is an important limit to this compatibility:

```haskell
normalize expression
```

returns an `Expr` that shares memory, but a generic recursive function can
still visit a shared child more than once. Code processing very large normal
forms should use `CoreGraph` functions such as `foldUnique` instead.

## 5. Normalization, step by step

Dhall normalization evaluates an expression and then converts the evaluated
value back to syntax. The implementation calls these two operations `eval`
and `quote`.

The new graph-returning path is:

```haskell
normalizeGraph :: Eq a => Expr s a -> CoreGraph a
normalizeGraph = normalizeGraphWith SharingOn

normalizeGraphWith mode expression =
    withEvalRun defaultEvalConfig $ runST $ do
        builder <- newBuilder mode
        let value = evalImpl Empty (denote expression)
        root <- quoteGraph builder EmptyNames value
        freeze builder root
```

The steps are:

1. `denote expression` removes source-only information.
2. `evalImpl` computes a `Val`, the evaluator's internal form of a Dhall value.
3. `quoteGraph` converts that value directly into shared `CoreNode`s.
4. `freeze` returns the completed `CoreGraph`.

The existing API still works:

```haskell
normalize :: Eq a => Expr s a -> Expr t a
```

It follows the same evaluate-and-quote design and returns a physically shared
`Expr`.

For testing, `normalizeGraphWith SharingOff` always appends nodes instead of
reusing equal ones. The tests compare its result with `SharingOn`. Turning
sharing on must change memory use only, never Dhall behavior or output.

## 6. Evaluated values and their identifiers

The evaluator uses `Val` while computing. A shortened version of the real type
is:

```haskell
data Val a = Val
    { valId     :: ValId
    , valClosed :: Bool
    , valF      :: ValF a
    }
```

`valF` contains the actual value, for example a natural number, list, record,
function, or unevaluated variable.

`ValId` is a newly allocated number attached to each `Val`. It has two jobs:

1. Comparing a value with itself can return `True` immediately.
2. Optional result caches can refer to a value without deeply comparing it.

`valClosed` is `True` when the value contains no reference to a variable
outside itself. Caching a closed value is simpler because its meaning cannot
change when it appears under a different set of variable bindings.

The identifiers are internal. The `Show` instance deliberately ignores them:

```haskell
instance (...) => Show (Val a) where
    showsPrec n (Val _ _ contents) = showsPrec n contents
```

They are also absent from Dhall text, error messages, CBOR, and semantic
hashes.

### Functions remember the variables available when they were created

An evaluated Dhall function contains a `Closure`:

```haskell
data Closure a = Closure
    { cloName   :: Text
    , cloEnv    :: Environment a
    , cloBody   :: Expr Void a
    , cloBodyId :: NodeId
    }
```

A closure means: “evaluate this body later, using the variable values that
were in scope when the function was created.” `cloBodyId` gives the body
a compact identity for the optional result cache.

Each `Environment` update also receives an `EnvId`. Thus two evaluations of
the same open body can be distinguished when their variables have different
values.

## 7. Optional result caches

A result cache remembers the answer to a previous computation. This is also
called memoization. Four caches exist in `Dhall.Eval.Run`:

```haskell
-- Applying a function:
EvalKey bodyNode environment binder argumentValue

-- Converting a value back to syntax:
ValId -> NodeId

-- Checking whether two closed values are equal:
(ValId, ValId) -> Bool

-- Inferring a type:
InferKey expressionNode valueEnvironment typeEnvironment
```

The complete input must be part of the key. For example, caching function
application only by `bodyNode` would be wrong: the body may read a variable
whose value differs between calls.

Entries are bounded by `cacheCap`, so a long computation cannot keep every
intermediate value alive indefinitely.

### Current default

These speed caches are currently **off by default**.

```haskell
defaultEvalConfig = EvalConfig
    { enableCache     = False
    , cacheCap        = 10000
    , internSharing   = SharingOn
    , assignIds       = True
    , internOnClosure = True
    }

cacheOnConfig = defaultEvalConfig { enableCache = True }
```

The reason is concrete: when the caches are enabled during threaded Template
Haskell compilation (`Dhall.Test.TH`), GHC blackholes even after insert paths
force values before updating the shared `IORef`. Until a design cannot
re-enter the same shared state from TH, the default stays off. Use
`cacheOnConfig` for runtime-only measurement.

`assignIds` and `internOnClosure` are diagnostic knobs: set them to `False` to
measure the cost of per-value identity allocation and closure-body interning
without changing the public API.

Structural sharing in `CoreGraph` remains on by default and does not depend on
these caches.

### ValId allocation bug (fixed)

An early implementation allocated ids with `freshValId ()` while rewriting the
whole `EvalRun` record under `atomicModifyIORef'`. GHC floated that into a
CAF, so every `Val` shared one id. `Natural/fold`'s shortcut then treated
`succ acc == acc` on the first step and returned the seed (e.g.
`Natural/fold 10 Natural (+1) 0` normalized to `0`). Ids now live in dedicated
`IORef` counters, `mkVal` is `NOINLINE`, and the fold shortcut uses structural
`convBody` rather than `convImpl`'s identity fast path.

## 8. Equality of evaluated values

The evaluator's equality function starts with the cheapest safe check:

```haskell
convImpl environment left right
    | valId left == valId right =
        True

    | valClosed left && valClosed right
    , Just answer <- lookupConvMemo (valId left) (valId right) =
        answer

    | otherwise =
        convBody environment left right
```

If the identifiers match, both arguments are the very same evaluated value.
Otherwise `convBody` compares their contents. When caches are enabled, the
answer for two closed values can be reused.

## 9. Typechecking and variable context

Typechecking keeps two related collections:

```haskell
data Ctx a = Ctx
    { values :: Environment a
    , types  :: Types a
    }

data Types a
    = TypesEmpty
    | TypesBind EnvId (Types a) Text (Val a)
```

`values` says what a variable evaluates to. `types` says what type the
variable has. Each new type binding receives an `EnvId`.

Before inferring an expression's type, the typechecker constructs this key:

```haskell
let node = internBody (denote expression)
    key  = InferKey
        (unNodeId node)
        (unEnvId (envId values))
        (unEnvId (typesId types))
```

Why are all three numbers needed? Consider:

```dhall
λ(x : Natural) → x
λ(x : Bool)    → x
```

The body syntax is `x` in both places, so it may have the same `NodeId`.
However, its inferred type is `Natural` in the first function and `Bool` in
the second. The two type environments have different identifiers, so their
cache keys differ.

Errors still come from the original `Expr s a`, including its `Note` and
`Src` values. Sharing denoted expressions therefore does not replace a useful
source location with the location of some other occurrence.

## 10. Shadowed variable names

The same name can refer to different variables:

```dhall
let x = 1
let first = x
let x = 2
in  [ first, x ]
```

The two occurrences printed as `x` do not have the same meaning. The first
refers to the outer binding and the second to the inner binding.

The implementation handles this separately at each stage:

1. `CoreGraph` may share identical open syntax. This is safe because it stores
   syntax, not the result of evaluating that syntax.
2. Evaluation cache keys include `EnvId`, so the same syntax with different
   variable values does not reuse the wrong result.
3. Type cache keys include the value and type environments.
4. Common-subexpression extraction records which binding each free variable
   refers to before introducing a `let`.

## 11. Compact Dhall text with `cse`

`Dhall.Core.CSE` is an optional transformation for readable output. “CSE”
means *common-subexpression elimination*: repeated text is named once with a
`let`.

For example, an expression shaped like:

```dhall
[ largeExpression, largeExpression ]
```

can be printed as:

```dhall
let shared0 = largeExpression
in  [ shared0, shared0 ]
```

This is a Dhall program with the same meaning, but it is not the normal form:
normalizing the `let` expands its uses again. Consequently, `cse` is never
used to calculate semantic hashes.

For an open expression such as `f x`, equality of `NodeId` alone is not
enough. The CSE implementation also records which surrounding variable each
free name refers to, preventing an expression from being moved across a
different or shadowing binder.

## 12. Binary encoding

CBOR is the binary data format used to calculate Dhall semantic hashes. The
current Dhall standard encodes a recursive expression tree, so repeated
subexpressions produce repeated bytes.

`Dhall.Core.DAG.CBOR` prototypes a shared binary form:

```text
["Dhall-DAG", 1, nodes, root]
```

For the addition example, its logical shape is:

```text
nodes[0] = NaturalLit 1
nodes[1] = NaturalPlus (reference 0) (reference 0)
root     = 1
```

Before encoding, `canonicalize` performs these steps:

1. Rebuild the graph with structural sharing enabled.
2. Visit children before their parent.
3. Give nodes new consecutive numbers in that visit order.
4. Encode each node once.

This makes the bytes independent of the order in which the original graph was
built. During decoding, a child may refer only to an earlier node. This rule
prevents cycles. The decoder also rejects an empty node list, an unsupported
version, trailing bytes, an invalid root, and more than one million nodes.

### This format is not the default

The public semantic hash remains:

```haskell
hashGraphLegacy =
    sha256Hash
        . Lazy.toStrict
        . Dhall.Binary.encodeExpression
        . toSharedExpr
```

In other words, it still hashes the standard tree encoding. Changing the
default binary form would change every frozen `sha256:` import. ADR 004 lists
the coordinated standard update, new cache directory, refreezing, and
cross-implementation testing required before such a change.

## 13. Template Haskell

“TH” in the code and tests means **Template Haskell**. `Dhall.TH` can run
Dhall during Haskell compilation and generate Haskell declarations:

```haskell
Dhall.TH.makeHaskellTypes
    [ MultipleConstructors "Department" "./tests/th/Department.dhall"
    , SingleConstructor
        "Employee"
        "MakeEmployee"
        "./tests/th/Employee.dhall"
    ]
```

GHC executes these declarations while compiling `Dhall.Test.TH`. They parse,
resolve, typecheck, and normalize the referenced Dhall files. This test is
important because it exercises the evaluator from inside a threaded GHC
process; that is how the unsafe default cache behavior was found.

Template Haskell continues to consume ordinary `Expr` values. It is one
reason the public syntax type was not replaced by `CoreGraph`. Speed caches
remain off by default because enabling them still blackholes threaded TH
compilation of `Dhall.Test.TH`.

## 14. Public entry points

Code that needs an ordinary expression can keep using:

```haskell
normalize :: Eq a => Expr s a -> Expr t a
```

Code that wants to process a large result without repeatedly walking shared
children can use:

```haskell
normalizeGraph      :: Eq a => Expr s a -> CoreGraph a
fromExpr            :: Eq a => Expr s a -> CoreGraph a
toSharedExpr        :: CoreGraph a -> Expr Void a
graphMetrics        :: CoreGraph a -> GraphMetrics
alphaNormalizeGraph :: Eq a => CoreGraph a -> CoreGraph a
prettyGraph         :: Pretty a => CoreGraph a -> Text
hashExpressionGraph :: CoreGraph Void -> SHA256Digest
```

The experimental binary functions are:

```haskell
encodeGraph :: CoreGraph Void -> ByteString
decodeGraph :: ByteString -> Either String (CoreGraph Void)
```

## 15. Where to read the implementation

Read these files in this order:

1. `Dhall/Core/DAG/Node.hs` — `NodeId` and the node cases.
2. `Dhall/Core/DAG.hs` — construction, lookup, conversion, and graph walks.
3. `Dhall/Eval.hs` — evaluated values and direct graph output from `quoteGraph`.
4. `Dhall/Eval/Run.hs` — identifiers, optional caches, and statistics.
5. `Dhall/TypeCheck.hs` — context identifiers and type inference.
6. `Dhall/Core/CSE.hs` — optional `let` extraction for compact text.
7. `Dhall/Core/DAG/CBOR.hs` — the experimental binary encoder and decoder.
8. `Dhall/Test/DAG.hs` — small executable examples of the required behavior.

## 16. Behaviors checked by tests

The tests check, among other things:

- converting `Expr -> CoreGraph -> Expr` preserves denoted syntax;
- sharing on and sharing off produce the same expression;
- repeated children use fewer stored nodes when sharing is on;
- graph-based shift, substitution, and alpha-normalization match the existing
  expression operations;
- graph normalization matches `normalize`;
- `ValId` and `EnvId` do not appear in output;
- disabling caches performs no cache insertions;
- typechecking repeated expressions returns the expected type;
- experimental binary bytes do not depend on sharing mode or construction
  order;
- decoding experimental bytes reconstructs the same expression;
- the legacy graph hash equals the existing expression hash.

These comparisons let the implementation change its storage without silently
changing Dhall semantics, text, or standard hashes.

## 17. Shared imports on current main

Import resolution keeps main's twin expression. Each use of a loaded import
is an `Embed` of an `ImportRef`, and `normalizeLoaded` evaluates that twin
in an `Environment` whose bottom frame is `imports`. The frame resolves
every reference to one thunk, so a file imported twice is evaluated once.

`quote` / `quoteGraph` still intern the resulting normal form into a
`CoreGraph`. `dhall hash` uses `normalizeLoaded` (or a cached integrity
normal form) and then `alphaNormalize`. It does not re-walk the fully
inlined tree through `normalizeGraph`.

Typechecking a resolved file uses `typeWithImports`: child imports stay
embedded, and their types come from `_importTypes`. The value environment
is rebuilt with `imports` at the bottom so conversion can force a child
only when it needs the value.

`quoteBounded` is the `--max-output-size` path. It emits predefined
functions as ordinary variables (`Natural/fold`, `List/head`, and the
rest), matching `quoteGraph`.

Default semantic hashes stay on legacy tree CBOR. `encodeGraph` is
experimental and is not what `hashExpression` writes. Speed caches stay
off unless a caller passes `cacheOnConfig`.

