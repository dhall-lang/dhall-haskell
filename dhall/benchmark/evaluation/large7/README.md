# large7 — duplicated unhashed imports

Both entries import the same Prelude and LatinChar modules from many sites.
Resolving those imports inlines a copy at every site, so the import graph
becomes a tree a few thousand times larger.

- `big.dhall` imports `Name/package.dhall`. This is the whole Name package
  (70 files) and the entry `run.sh` and the tasty harness time.
- `small.dhall` imports `Name/words.dhall` only. It is the same graph with the
  `order`, `equality`, `fromLatinCharsAndNumber`, and `toTextIn*` files left
  out. On an optimized build it is about a third of the `big.dhall` time.
  `run.sh` has those three commands commented out. The tasty harness reads
  `large7MainPath` in `Bench/ImportTrees.hs`.

Prelude files under `dhall-lang/Prelude/` are unhashed copies. The upstream
`dhall-lang` files still carry `sha256:` checks, and those checks collapse
this tree. `prelude.dhall` is a small record of the functions this tree uses.
The lude and typeclasses files are real copies, with integrity checks removed.

`dhall type` typechecks the parsed file, using the type stored for each
import. `dhall hash`, `dhall text`, `dhall directory-tree`, and the default
`dhall` command normalize through the shared import twin. `type_inlined`
still typechecks the inlined tree, so that bench stays slow on purpose.

## Commands

```sh
cd dhall && stack bench evaluation --ba '--pattern large7'
DHALL="$(stack exec -- which dhall)" ./benchmark/evaluation/large7/run.sh
```

`run.sh` times three commands, and kills each one after 180s:

- `dhall type --quiet --file big.dhall`
- `dhall --file big.dhall`
- `dhall hash --file big.dhall`

## Harness

Prep parses and resolves once. It does not typecheck the resolved tree.
The timed benches are:

| Bench | What it measures |
|---|---|
| `large7.type_inlined` | `typeOf` of the inlined tree |
| `large7.type_compositional` | typecheck with each import bound from the cache |
| `large7.nf_inlined` | `normalize` of the inlined tree |
| `large7.nf_shared` | `normalizeLoaded` of the shared twin (default evaluator) |

`type_inlined` is the slow path. A run that is still going after 3 minutes
should be killed.

## Results

Optimized `-O2` build. `big.dhall`. No command below was killed at 180s.
The one killed run in this work was an accidental full evaluation suite,
which was stopped before it produced a number.

| Commit | Fixture | Command | Seconds |
|---|---|---|---|
| fixture, before sharing | big.dhall | `dhall type` cold | 32.78 |
| fixture, before sharing | big.dhall | `dhall` | 33.12 |
| fixture, before sharing | big.dhall | `dhall hash` | 16.41 |
| fixture, before sharing | bench | `type_inlined` | 11.02 |
| compositional typecheck | big.dhall | `dhall type` cold | 18.23 |
| compositional typecheck | big.dhall | `dhall` cold | 17.45 |
| compositional typecheck | big.dhall | `dhall` warm | 34.05 |
| compositional typecheck | big.dhall | `dhall hash` | 16.80 |
| compositional typecheck | bench | `type_compositional` | 11.14 |
| compositional typecheck | large3 `get_config.dhall` | `dhall` | 7.08 |
| compositional typecheck | large5 `pipeline-code.dhall` | `dhall` | 4.64 |
| compositional typecheck | k8s `(./file3.dhall).mkPod` | `dhall` | 3.26 |
| compositional typecheck | k8s `(./file4.dhall).mkPod` | `dhall` | 0.73 |
| types stored on cache hits | big.dhall | `dhall type` cold | 17.40 |
| types stored on cache hits | big.dhall | `dhall type` warm | 16.46 |
| types stored on cache hits | big.dhall | `dhall` warm | 16.50 |
| types stored on cache hits | big.dhall | `dhall hash` | 16.59 |
| types stored on cache hits | bench | `type_compositional` | 0.000172 |
| types stored on cache hits | bench | `type_inlined` | 11.14 |
| types stored on cache hits | bench | `nf_shared` | 0.00213 |
| hole counted, not walked | big.dhall | `dhall type` cold | 0.092 |
| hole counted, not walked | big.dhall | `dhall type` warm | 0.077 |
| hole counted, not walked | big.dhall | `dhall` warm | 0.151 |
| hole counted, not walked | big.dhall | `dhall hash` warm | 0.086 |
| type written to the cache | big.dhall | `dhall type` cold | 0.884 |
| type written to the cache | big.dhall | `dhall type` warm | 0.069 |
| type written to the cache | big.dhall | `dhall` warm | 0.164 |
| shared twin for hash and text | big.dhall | `dhall hash` warm | 0.085 |
| shared twin for hash and text | large5 `pipeline-code.dhall` | `dhall hash` | 0.426 |
| denote before substitute | big.dhall | `dhall type` warm | 0.079 |
| denote before substitute | big.dhall | resident set | 36 MB, same as before |
| final tree | big.dhall | `dhall type` cold | 0.877 |
| final tree | big.dhall | `dhall type` warm | 0.067 |
| final tree | big.dhall | `dhall` | 0.152 |
| final tree | big.dhall | `dhall hash` | 0.088 |
| final tree | bench | resolve | 0.070 |
| final tree | bench | `type_compositional` | 0.000285 |
| final tree | bench | `nf_shared` | 0.00208 |
| final tree | bench | `type_inlined` | 11.10 |
| final tree | bench | `nf_inlined` | 0.00301 |
| final tree | large3 `get_config.dhall` | `dhall` | 4.65 |
| final tree | large5 `pipeline-code.dhall` | `dhall` | 4.47 |
| final tree | large5 `pipeline-code.dhall` | `dhall hash` | 0.366 |
| final tree | k8s `(./file3.dhall).mkPod` | `dhall` | 1.66 |
| final tree | k8s `(./file4.dhall).mkPod` | `dhall` | 0.237 |
| final tree | large1 `main.dhall` | `dhall` | 2.77 |
| final tree | large4 `generate-example.dhall` | `dhall` | 0.608 |
| final tree | prelude_import `prelude-code.dhall` | `dhall` | 0.388 |
| final tree | large6 parse | `dhall` | 0.618 |
| final tree | large6 eval | `dhall` | 0.504 |
| final tree | large6 typecheck | `dhall` | 0.023 |
| final tree | large6 normalize | `dhall` | 0.610 |
| final tree | large6 multi | `dhall` | 0.498 |
| final tree | large6 walk | `dhall` | 0.309 |

The commits that changed the result are compositional typechecking, storing
the type on a cache hit, and counting import holes instead of walking the
inlined tree. Hash and text already finished in under a tenth of a second
once that walk was gone; pointing them at the shared twin kept the same hash.
Writing the type into the cache did not make a warm `dhall type` faster than
recomputing it in memory, and the cold run was slower. Denoting a file before
substituting its children did not change time or resident set. Those two
commits are still on this branch. The language server was built with the
same typecheck and normalize helpers. `big.dhall` hashes to
`sha256:8be4a48346a4754df6e86f6289a0487d2a64019bd5d5aa4b10eb8ae442694197`.

