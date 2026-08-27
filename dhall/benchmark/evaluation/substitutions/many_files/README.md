# many_files — as-code substitution regression probe

This fixture exists because `substitutions.as_code` (one file, 8000 nested
lets, closed substitution values, disjoint binders) **cannot see** the
customer as-code slowdown.

Do **not** replace this with a single large file. as-code calls
`substitute` once per imported module; one file would only call it a
couple of times (the nested-let group already measures that).

The 200 modules are **generated at bench prep** into a temp directory
(`writeManyFilesFixture` in `Bench.Substitutions`), not committed. Generation is
outside the timed `resolve_cold_cache_on` samples. The tree is deleted
when the process exits.

Customer shape, from the `ResolvedSubstitutions` profiles:

- hundreds of imported files, each calling `substitute` on the
  semi-semantic-miss path (~592 calls)
- ~200 Haskell-API keys
- substitution values with free names that collide with local binders
  (`a`, `x`, …), so `shiftSubstitutions` cannot return the same map pointer

## Generated tree

| File | Role |
|------|------|
| `mods/m000.dhall` … `m199.dhall` | `let a = i let x = 1 in a + x` |
| `package.dhall` | `[ ./mods/m000.dhall, … ]` |
| `pipeline-code.dhall` | `./package.dhall` |

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
| `substitutions.many_files` | `resolve_cold_cache_on` |

```sh
stack bench evaluation --ba '--pattern substitutions.many_files'
```

CLI (no Haskell substitutions; full `dhall --file`): from
`substitutions/`, `./run.sh` generates this tree into a temp directory
and times the benchmark with a fresh `XDG_CACHE_HOME` per run.
