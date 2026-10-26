# How Dhall loads imports

This describes what `dhall/src/Dhall/Import.hs` does today: how a file that
mentions other files is turned into a value, what is remembered for the rest
of the run, what is written to disk, and which commands use which result.

Import modes are `Code`, `as Text`, `as Bytes`, and `as Location`.

---

## 1. Two results of one load

Loading a program produces an expression with no imports left in it. There are
two ways to hold that expression.

**The inlined expression** has every import replaced by that import's full
value, copied into place. This is what `dhall resolve` prints. It can be very
large when the same child appears under many parents, because each parent
holds its own copy of that child.

**The shared tree** has the same shape, except each import is a small number.
The number points at one body for that import, kept on the side. Every use of
the same import is the same number, so later simplification of that import
happens once. The code calls this second tree the **twin**. Its import nodes have
type `ImportRef`.

`loadWithShared` builds both. `loadWith` builds only the inlined expression.
Nested loads keep whichever choice the outermost load made.

The ordinary `dhall` command, and the library helper
`resolveSharedWithSettings`, use `loadWithShared`. Callers that only need the
inlined expression, or that must apply a custom simplifier, use `loadWith`.
Section 12 lists them.

A custom simplifier is a Haskell function supplied by the caller. The shared
evaluator does not call it. A caller that sets one simplifies the inlined
expression instead.

---

## 2. What one run remembers

All of this lives on `Status` (`Dhall.Import.Types`). The public import API
hides the fields. The names below are the ones in the source.

### Results of imports

- `_cache` maps each import's identity to its finished value. The identity
includes the path or URL, the mode (`Code`, `Text`, and so on), and the
integrity hash when the import has one. A second request for the same
identity in the same run returns this entry and does not read the file
again.
- `_importTypes` maps that same identity to the type that was inferred for
the file. A parent can reuse a child's type instead of walking the child's
copied text.
- `_importRefs` maps an identity to the number used for it in the shared tree.
The number is chosen once.
- `_importBodies` maps that number to the body the shared evaluator will
simplify. For an ordinary code file the body is that file's own shared
tree, still naming its children by number. For a hash-protected file whose
hash was checked, the body is replaced by the fully simplified value.
`_keepImportBodies` is false only for a private load that will neither
return `Status` nor simplify the shared tree. The body map then stays empty.
- `_importHoles` maps an identity that failed to a placeholder, when the
caller asked to keep going after errors. Placeholders are not stored in
`_cache`.

### Disk-cache helpers

- `_merkleHashCache` stores, for each import, the hash used when a parent
computes its own disk-cache name. A frozen import contributes its integrity
hash. An ordinary import contributes the hash computed for its own cache
entry. An import that was never loaded, such as the unused side of `?`,
contributes a hash of its written form.
- `_merkleContextFingerprint` and `_merkleSubstitutionsFingerprint` remember
the hashes of the starting context and of the substitution map, so those
are not re-encoded for every file.
- `_semanticCacheMode` is `UseSemanticCache` or `IgnoreSemanticCache`.
Ignoring the cache skips the on-disk entries for hash-protected imports.
The ordinary-code disk cache and the in-memory maps above are still used.

### Other settings that change a load

- `_stack` is the chain of imports that led here. It detects cycles and is
included in error messages.
- `_graph` is the list of parent-to-child edges. `dhall resolve --dot` prints
it.
- `_normalizer` is the optional custom simplifier.
- `_startingContext` is extra names the caller already knows, used while
type-checking.
- `_substitutions` is a map of names the Haskell API asked to replace.
`_resolvedSubstitutions` is that map prepared for use, computed at most
once per run.
- `_importErrorMode` is `FailFast` or `CollectErrors`. The command-line tool
collects errors so it can report every missing import. `?` still stops at
the first failure of one side, so the other side can run.
- `_verifySemanticHash` is false in the language server during an ordinary
load. The file is still type-checked. Checking the integrity hash, which
also fully simplifies the file, waits until the user asks for a hash.
- `_importSources` keeps the text that was fetched and where it came from,
for diagnostics. A hit in the hash-protected disk cache has no original
text. `_keepImportSources` turns this off when nobody will read it.

There is no separate cache of parsed text. The first time an identity is
loaded, the file is read and parsed. The finished value in `_cache` is what
a later request reuses.

---

## 3. Walking a file

`resolveImports` walks the parsed expression.

For an import node it:

1. Builds the child's identity from the parent (`chainImport`). A relative

   path is resolved against the file that mentioned it. The identity is
    canonical, so two spellings of the same file share one entry.
2. Rejects a remote, environment, or `missing` parent that tries to import a

   local file. A local parent may import either.
3. Rejects a child that is already on the stack. That is a cycle.
4. Records the edge from parent to child.
5. Loads the child (`loadImport`) and puts the child on the stack while that

   load runs.

If the shared tree is being built, the import node becomes the child's
number. Otherwise the inlined expression receives a copy of the child's
finished value, and the "shared" result is that same copy under another type.
The two are one object in that case.

For `left ? right`, the left side is tried first. A missing import on the
left tries the right side. A type error, a cycle, a hash mismatch, or a parse
error is not treated as "missing": it fails the whole alternative. If the
left side was protected by an integrity hash and the right side succeeds with
the same hash, the result is saved in the hash-protected disk cache. Section
10 describes that fill.

Every other form is walked by visiting its parts.

`loadImport` is the per-import entry:

1. If this identity already failed and was replaced by a placeholder, return

   that placeholder.
2. If `_cache` has a value, return it. When the shared tree is being built,

   make sure the identity has a number. The body was stored on the first
    load.
3. Otherwise load from disk caches or from the file, store the value in

   `_cache`, and, when the shared tree is being built, store a body for its
    number if none was stored already.

---

## 4. Hash-protected imports and ordinary ones

`loadImportWithSemanticCache` splits on the import.

An import with no integrity hash, and every `as Location` import, goes to
`loadImportWithSemisemanticCache`. `as Location` is not looked up in the
hash-protected disk cache, even when someone wrote a hash next to it.

An import with an integrity hash is looked up in the `dhall/` directory of
the cache, under a file named `1220` plus that hash.

- If the file is there and its bytes hash to the expected value, those bytes
are decoded and returned as an already simplified value. The original
source is not fetched.
- If the file is there but the bytes do not match, a warning is printed and
the import is loaded from source, as on a miss.
- If the cache is disabled, or the file is absent, the import is loaded from
source. It is type-checked. When hash checks are on, it is then fully
simplified, the simplified form is hashed, and the hash must match. A match
is written back to `dhall/`. A mismatch is an error. The shared body, if
one is being kept, is replaced by that simplified form, so later evaluation
does not simplify the unsimplified body that was recorded while reading the
file.

`as Text` and `as Bytes` read the file as raw text or raw bytes and wrap that
in a literal. They are already simplified. They are not written to either
disk cache. Their hash is remembered only so a parent can name its own cache
entry.

`as Location` builds a small union value naming the kind of import and, where
it applies, the path or URL. It is already simplified and is not cached on
disk.

---

## 5. The disk cache for ordinary code files

Unhashed `Code` imports use a second directory, `dhall-haskell-v2/`. An older
directory, `dhall-haskell/`, used a different key and stored a full simplified
expression. The two must not be mixed. Files in both directories are named
`1220` plus a hash.

The key for an ordinary code file is a fingerprint of:

- the file's own syntax, with each import replaced by that import's hash
- the starting context
- the substitution map

It is not a hash of the fully copied expression. Changing a child changes the
child's hash, which changes the parent's key. `computeMerkleSemisemanticHash`
builds it.

The stored payload is one of three shapes, told apart by a leading byte:

| Byte | What is stored | What a later run does with it |
| --- | --- | --- |
| 0 | An old, small, fully simplified expression | Decode it and use it as already simplified. Ignored when a custom simplifier is set, because that old value was produced by the built-in simplifier. |
| 1 | Nothing but "this file type-checks" | Compute the type from the children, write the type back when it is available, and build the value from the children. |
| 3 | The inferred type | Restore that type and build the value from the children. The value itself is not what was stored. |

New writes use byte 3 when the type was inferred, and byte 1 when it was not.
Byte 0 is only read, so that caches written by older versions still work.

The load of an ordinary code file, in `loadImportWithSemisemanticCache`, is:

1. Read and parse the file.
2. Resolve the imports inside it. Children are loaded, type-checked, and

   cached before the parent is.
3. If the caller is collecting errors and this file recorded a new failure,

   or it pulled in a placeholder from an earlier failure, do not type-check
    or cache it. Replace it with a placeholder. The errors are already listed.
4. Compute the cache key and look in `dhall-haskell-v2/`.
5. On a byte-3 hit, restore the type and build the value (section 7). On a

   byte-1 hit, or a byte-3 entry that does not decode, infer the type from
    the children and write a fresh entry. On a byte-0 hit, with no custom
    simplifier, return the decoded value.
6. On a miss, a corrupt entry, or a byte-0 entry under a custom simplifier:
   - If the file is only a single import, reuse that child's type. The child
    was loaded in step 2.
   - Otherwise type-check this file from its children's types (section 8).
   If that cannot be done, type-check the inlined expression. A type error
   is reported from that inlined expression, so the message points at real
   code.
   - Store the type and write the disk entry.
   - Build the value and return it marked type-checked, not yet simplified.

Simplification of an ordinary code file waits until something needs the
simplified form: an integrity check, a `?` cache fill, or the caller's final
evaluation.

---

## 6. How a file's value is built

`sharedImportValue` builds the value stored in `_cache`. Both paths avoid
copying every child at every level.

**When the shared tree is being built**

1. Drop source notes from this file's shared tree first, then apply

   substitutions. Dropping notes first matters: doing it afterwards would
    copy every substitution into the copy.
2. Save that tree as this import's body.
3. Walk the shared tree and the inlined expression together. They have the

   same shape. At each import number, record the inlined child's value.
4. Walk the saved body and replace each number with that child's value.

   Parts of the file that contain no imports are reused as they are, rather
    than copied. The replacement is strict, so each number holds only its own
    child.

The body that evaluation uses is the tree from step 2, where children are
still numbers. The value in `_cache` is the tree from step 4, where children
have been filled in. Parents that need a child's value share those filled-in
children by pointer.

**When only the inlined expression is being built**

1. Apply substitutions to the parsed file.
2. Replace each import in that file with the child's finished value from

   `_cache`.
3. If any child is missing from `_cache`, that replacement fails. The unused

   side of `?` is a common reason. The fallback is to drop notes from the
    inlined expression, which does copy the children that were already inlined.

A hashed import that then passes its integrity check overwrites the shared
body with the fully simplified value (`replaceImportBody`).

---

## 7. How a file is type-checked

`typecheckWithAlreadyCheckedImports` type-checks one file using the types of
the imports it mentions.

It first picks the side of each `?` that actually loaded
(`chooseResolvedArm`). It then asks whether every import in that choice is
already in `_cache`. If any is missing, it gives up and the caller type-checks
the inlined expression instead.

When every child is present, each child's type comes from `_importTypes` when
it was stored, and otherwise from type-checking that child's finished value.
The check itself is `typeWithImports`: an import node is given the type and
the value already known for that import, and the rest of the file is checked
around those. The inferred type is stored in `_importTypes`.

`typecheckLoaded` is the same check for the root expression the user wrote.
If the child-based check cannot run, it type-checks the inlined expression
with `typeWith`.

A file whose whole text is one import does not need a fresh check. Its type
is the child's type (`storeChildImportType`).

This is why a large import tree can be type-checked quickly, while
type-checking the same program after every import has been copied into one
expression remains slow. The copied expression repeats each child once per
parent, and the type checker walks every copy.

---

## 8. When a file is fully simplified

An ordinary code import is stored as type-checked only. A hash-protected code
import, once its hash is checked, is stored as already simplified. `as Text`,
`as Bytes`, and `as Location` are already simplified when they are built.

`ensureNormalized` forces a type-checked value to a simplified one with
`normalizeWith`, including a custom simplifier when one is set, and replaces
the `_cache` entry.

`normalizeLoaded` simplifies a shared tree. Each body is simplified the first
time the result uses it. A later use shares that result. An import the result
never uses is not simplified. This evaluator does not call a custom
simplifier. An ill-typed starting context is ignored here; type-checking
reports that error.

`normalizeLoadedCapped` is the same, but it can stop once the simplified text
would exceed a size limit. The `dhall` command uses that for
`--max-output-size`.

`alphaBetaNormalForIntegrityHash` produces the fully simplified form whose
hash is the integrity hash:

- An already simplified import is only renamed to canonical bound names.
- With no custom simplifier, the shared body is simplified with
`normalizeLoaded` when one was kept. Otherwise the inlined value is
simplified.
- With a custom simplifier, the inlined value is simplified with that
simplifier.

The bytes that are hashed are that simplified expression, encoded, after
bound names are rewritten to a canonical form.

---

## 9. Alternatives written with `?`

The Prelude often writes `missing sha256:… ? ./some/local/file.dhall`. The
left side is tried first. If that hash is already in the `dhall/` cache, the
local file is never read. If the left side is missing, the right side is
loaded.

The right side is not itself hash-protected, so a different value is not an
error. If its simplified form happens to have the same hash, that form is
written to `dhall/` under that hash. The next run can take the left side.
The value returned to the caller is the simplified form, the same thing a
cache hit would have returned.

The language server turns hash checks off for ordinary loads, and this fill
is skipped there as well. The `dhall` executable still fills the cache.

If the successful side contains a placeholder from a collected error, nothing
is written.

A fill simplifies the inlined expression with `normalizeWith`, so a custom
simplifier is included. The shared tree is not used for this particular
simplification.

---

## 10. When an import fails

`FailFast`, used by `Dhall.Import.load`, throws on the first failure.

`CollectErrors`, used by the `dhall` command, records the failure and keeps
resolving later imports. The failed import becomes a hole (`ImportHole`) in
the inlined expression. The hole's number, the source span, and the errors
are stored so they can be printed together. A file that contains such a hole
is not type-checked and is not written to the ordinary-code disk cache.

Inside `?`, collection is suspended. The failure has to throw so the other
side can run. If both sides fail with a missing import, the two errors are
reported together and the source span covers both sides.

---

## 11. Which callers use which result

### Shared tree, then simplify that tree

These type-check with `typecheckLoaded` and simplify with `normalizeLoaded`.
They do not set a custom simplifier, or they only take this path when none
is set.

- The default `dhall` command, `dhall type`, `dhall hash`, `dhall text`, and
`dhall directory-tree`. `dhall hash` of a single import uses
`alphaBetaNormalForIntegrityHash` so the hash matches the integrity-check
rule. `dhall type` infers a type and does not need the simplified value.
- Library `input`, `inputExpr`, `interpretExpr`, and `fromExpr`, through
`typecheckLoaded` and, when a value is needed, `normalizeLoaded`. The
library also replaces names from its substitution map in the root
expression before the walk. Each imported file applies that same map when
its value is built. A custom simplifier switches the final simplification
back to the inlined expression.
- `dhall freeze`, when no custom simplifier is set. The import is loaded with
`loadWithShared`, type-checked, and hashed from its shared body. With a
custom simplifier, freeze uses `loadWith`, type-checks the inlined
expression, and simplifies that. The command's default only freezes remote
imports. `--all` includes local ones.
- Template Haskell `staticDhallExpression`.
- Schema rewriting (`Dhall.Schemas.rewriteWithSchemas`).
- `dhall-to-csv`.
- The Dhall and Hash modes of `dhall-try`. Its Type, JSON, and YAML modes
stop after type-checking the inlined expression.
- The language server, when no custom simplifier is set. It also keeps the
shared body in its cache so a later hash request can simplify that body.
With a custom simplifier it uses `loadWith` and does not keep a body.
Ordinary loads do not check integrity hashes.

`loadRelativeToTypecheckAndNormalize` is the helper that loads the shared
tree, type-checks, and simplifies, for callers that have no custom
simplifier.

### Inlined expression, type-checked from children, not simplified here

`loadRelativeToAndTypecheck` loads with `loadWith` and type-checks with
`typecheckLoaded`. The caller then translates the inlined expression.

- `dhall-to-json` and `dhall-to-yaml`
- `dhall-to-toml`
- `dhall-to-nix` and `dhall-to-bash`

Switching these to the shared tree and then throwing the tree away would keep
both copies for no later use.

### Inlined expression for a different reason

- `dhall resolve` with no extra mode loads the shared tree, because it goes
through the same helper as the default command, and then prints the inlined
expression. Unhashed code files are still unsimplified, so the printed
program can contain `let`.
- `dhall resolve --dot` and `dhall resolve --transitive-dependencies` use
`loadWith`. They need the graph or the list of identities, not a simplified
value.
- `dhall resolve --immediate-dependencies` prints the imports written in the
file. It does not load them.
- `dhall normalize` rejects imports. It type-checks and simplifies the
expression as it was written.
- The REPL loads with `loadWith` and simplifies the inlined expression. It
also remembers earlier bindings in a context the shared evaluator does not
see, which is why it stays on this path.

---

## 12. A short example

Three files, empty disk caches, one run of `dhall`.

`dep.dhall`:

```dhall
True
```

`lib.dhall`:

```dhall
let x = ./dep.dhall sha256:abc… in x
```

`main.dhall`:

```dhall
{ a = ./lib.dhall, b = ./lib.dhall }
```

`main.dhall` is loaded with the shared tree on.

`lib.dhall`**.** Its identity misses `_cache`. It has no integrity hash, so
it is an ordinary code file. The file is read and parsed. Resolving it
reaches `dep.dhall`.

`dep.dhall`**.** Its identity includes the hash. The `dhall/` cache misses.
The file is read. It has no imports. It is type-checked, fully simplified
because the hash must be checked, and the simplified form is hashed. On a
match, the bytes are written to `dhall/1220…`. The value is stored in
`_cache` as already simplified, and the shared body for `dep.dhall` is that
simplified `True`.

**Back in** `lib.dhall`**.** Both children of the file are done (`dep.dhall` is
the only one). The ordinary-code cache misses. The file is type-checked from
`dep.dhall`'s type. The inferred type is stored in `_importTypes` and written
to `dhall-haskell-v2/` with a leading byte 3. The value stored in `_cache`
has `dep.dhall` filled in by pointer to `dep.dhall`'s value. The shared body
still names `dep.dhall` by its number. The value is marked type-checked, not
simplified.

`main.dhall`**.** Both fields name the same identity, so the second
`./lib.dhall` is the `_cache` hit. The shared tree of `main.dhall` contains
the same number twice. Simplifying `main.dhall` simplifies `lib.dhall` once,
the first time it is needed. The second field reuses that result.

A second run, with the disk caches still in place, does not re-type-check
`lib.dhall`. The byte-3 entry restores its type, and the value is built from
`dep.dhall` again. `dep.dhall` itself is the decoded `dhall/` entry, and its
source is not read.

---

## 13. What the benchmarks are comparing

The evaluation bench and the command-line bench in
`dhall/benchmark/evaluation/` measure different things. The large import tree
used for the sharing work is `large7`, entered at `big.dhall`.

- `large7.type_inlined` type-checks the fully copied expression. It stays
slow, on the order of ten seconds, because each copied child is walked.
- `large7.type_compositional` type-checks from children the way
`typecheckLoaded` does. It is a fraction of a millisecond on the same tree.
- `large7.nf_shared` simplifies the shared tree. `large7.nf_inlined`
simplifies the copied expression. Filling children in by pointer means the
copied expression no longer rebuilds a fresh copy of each child while it is
simplified, so the inlined simplification is also fast. The type-checker
does not get that shortcut.
- The command-line rows `large7/type`, `large7/default`, and `large7/hash`
are `dhall type`, plain `dhall`, and `dhall hash` on `big.dhall`. They take
the shared path. On one machine they were about a tenth of a second, cold
and warm. Each fixture gets its own empty cache directory, so one fixture
does not warm the next, and neither uses `~/.cache/dhall`.

`dhall freeze --all` on `big.dhall` is the command that used to simplify the
copied tree while hashing. With the shared body it is a fraction of a second
and allocates on the order of a gigabyte. The same command on a build that
simplifies the copied tree took about ten seconds and allocated tens of
gigabytes. Peak live memory is higher on the shared path, because the shared
bodies and the inlined expression are kept together. The older path allocated
a great deal of short-lived data and kept a smaller live set.

These timings are one machine's measurements of this tree. They are a picture
of the difference, not a limit the program promises to meet.
