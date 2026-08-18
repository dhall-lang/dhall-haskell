# many_files — as-code substitution regression probe

This fixture exists because `substitutions.as_code` (one file, 8000 nested
lets, closed substitution values, disjoint binders) **cannot see** the
customer as-code slowdown.

Do **not** replace this with a single large file. as-code calls
`substitute` once per imported module; one file would only call it a
couple of times (the nested-let group already measures that).

The 200 modules are **generated at bench prep** into a temp directory
(`writeManyFilesFixture` in `Main.hs`), not committed. Generation is
outside the timed `resolve_cold_cache_on` samples. The tree is deleted
when the process exits.

Customer shape, from the `ResolvedSubstitutions` profiles:

- hundreds of imported files, each calling `substitute` on the Code
  semi-semantic-miss path (~592 calls vs 2 for as-source)
- ~200 Haskell-API keys
- substitution values with free names that collide with local binders
  (`a`, `x`, …), so `shiftSubstitutions` cannot return the same map pointer

## Generated tree

| File | Role |
|------|------|
| `mods/m000.dhall` … `m199.dhall` | `let a = i let x = 1 in a + x` |
| `package.dhall` | `[ ./mods/m000.dhall, … ]` |
| `pipeline-code.dhall` | `./package.dhall` |
| `pipeline-source.dhall` | `./package.dhall as Source` |

The harness installs 200 `UserType000`–`UserType199` substitutions.
Values are large (~64-field records). Every 10th has a free `a`; the rest
are closed. That is what plan (1) (shift only mentioning values) and plan
(2) (memoize the first root `let a` shift) need in order to show up.
Tiny 5-field records were cheaper than tasty-bench noise, so those
optimizations did not move this group.

## Harness

Mode B, `Dhall.resolveWithSettings`, fresh `XDG_CACHE_HOME` per sample.

| Group | Benchmark |
|-------|-----------|
| `substitutions.many_files.as_code` | `resolve_cold_cache_on` |
| `substitutions.many_files.as_source` | `resolve_cold_cache_on` |

```sh
stack bench evaluation --ba '--pattern substitutions.many_files'
```

CLI (no Haskell substitutions; full `dhall --file`): from
`substitutions/`, `./run.sh` generates this tree into a temp directory
and times Code and `as Source` with a fresh `XDG_CACHE_HOME` per run.

## What to read

- **`as_code`**: one `substitute` per imported module (plus the package
  list). Before caching `ResolvedSubstitutions` on `Status`, each call
  re-ran `resolveSubstitutions` / `freeVarNames`. That is the 18s → 30s
  customer regression. After the cache, the map is resolved once per run.
- **`as_source`**: substitutions run at root `finalizeSourceImport` (and
  the entry expression). Unhashed children still finalize per file on this
  synthetic tree; hashed customer libraries skip most of those calls via
  denoted-AST reuse. Treat as-source numbers here as a lower bound on the
  gap, not a reproduction of the customer as-source win.

Do not compare to the nested-let `substitutions.as_code` group: that one
is an identity-fast-path microbench, not a many-import regression probe.
