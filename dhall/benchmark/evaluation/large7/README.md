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

`run.sh` times `dhall type`, a default `dhall` evaluation, and `dhall hash`.
The tasty benches split the same work: two of them use the inlined tree, and
two use the shared import.

## Commands

```sh
cd dhall && stack bench evaluation --ba '--pattern large7'
DHALL="$(stack exec -- which dhall)" ./benchmark/evaluation/large7/run.sh
```

`run.sh` times three commands:

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

`type_inlined` and `nf_inlined` are the fully inlined tree. `type_compositional`
and `nf_shared` are the shared import.

