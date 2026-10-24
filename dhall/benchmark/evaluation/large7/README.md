# large7 — duplicated unhashed imports

`small.dhall` imports `Name/words.dhall`. That file and its dependencies import
the same Prelude and LatinChar modules from many sites. Resolving those imports
inlines a copy at every site, so the import graph (62 files) becomes a tree a
few thousand times larger.

Prelude files under `dhall-lang/Prelude/` are symlinks to the `dhall-lang`
submodule. `prelude.dhall` is a small record of the functions this tree uses.
The lude and typeclasses files are real copies, with integrity checks removed.

`dhall type` and `dhall hash` typecheck or normalize that inlined tree. The
default `dhall` command typechecks the inlined tree, then normalizes through
the shared import twin.

## Commands

```sh
cd dhall && stack bench evaluation --ba '--pattern large7'
DHALL="$(stack exec -- which dhall)" ./benchmark/evaluation/large7/run.sh
```

`run.sh` times three commands, and kills each one after 180s:

- `dhall type --quiet --file small.dhall`
- `dhall --file small.dhall`
- `dhall hash --file small.dhall`

## Harness

Prep parses and resolves once. It does not typecheck the resolved tree.
The timed benches are:

| Bench | What it measures |
|---|---|
| `large7.type_inlined` | `typeOf` of the inlined tree (`dhall type` today) |
| `large7.type_compositional` | typecheck with each import bound from the cache |
| `large7.nf_inlined` | `normalize` of the inlined tree (`dhall hash` of this file today) |
| `large7.nf_shared` | `normalizeLoaded` of the shared twin (default evaluator) |

`type_inlined` and `nf_inlined` are the slow path. A run that is still going
after 3 minutes should be killed.
