# large6 — isolated slow child imports

This benchmark isolates the cost of **hash-protected slow child imports**
under plain `Code` versus `as Source`.

See also `../README.md` for the three harness measurement modes (A/B/C).

## Artificial burdens

Most variants inject frozen child(ren) from `slow/`. Burdens are about
**0.5 seconds per activation** on a MacBook Pro M1 (roughly linear when scaled),
except `multi` which spreads ~0.5 s across eight children.

| Variant | Child file | What gets slow |
|---------|------------|----------------|
| `parse` | `./slow/parse.dhall` (generated) | **Parsing** ~1.3M lines of nested `{--}` comments |
| `eval` | `./slow/eval.dhall` | **Evaluation** of heavy `Natural/fold` when applied |
| `typecheck` | `./slow/typecheck.dhall` | **Type-checking** `assert` that evaluates frozen eval import |
| `normalize` | `./slow/normalize.dhall` | **Beta-normalization** of top-level `Natural/fold` during Code import load |
| `multi` | `./slow/multi/{1..8}.dhall` | **Eight** frozen children, ~60 ms normalize each |
| `walk` | `./slow/walk.dhall` | **Structural walk** of ~30k-element import-free `List Natural` |

Regenerate `walk.dhall` with `python3 slow/generate-walk.py`.
`slow/parse.dhall` is generated during evaluation-benchmark setup, or with
`python3 slow/generate-parse.py`. It is not checked in.

Packages (`package-long-*.dhall`) import `sha256:`-protected children from
`slow/`. Pipelines:

- `pipeline-code-long-*.dhall` — `./package-long-*.dhall` (`Code`)
- `pipeline-source-long-*.dhall` — `./package-long-*.dhall as Source`

## Full benchmark matrix

Benchmark group names: `large6.slow_<burden>.as_<mode>`.

| Group | Harness mode | Benchmarks | Where to read slow cost |
|-------|--------------|------------|-------------------------|
| `slow_parse.as_code` | A (phase) | resolve, typecheck, evaluation | **resolve** ~580 ms |
| `slow_parse.as_source` | A | resolve, typecheck, evaluation | **resolve** ~580 ms |
| `slow_eval.as_code` | **B (cold)** | `resolve_cold_cache_on` only | **~425 ms** cold resolve |
| `slow_eval.as_source` | A + C | resolve, typecheck, evaluation | **evaluation** ~220 ms |
| `slow_typecheck.as_code` | **B** | `resolve_cold_cache_on` only | **~425 ms** |
| `slow_typecheck.as_source` | A | resolve, typecheck, evaluation | **resolve + typecheck** ~225 ms each |
| `slow_normalize.as_code` | **B** | `resolve_cold_cache_on` only | **~530 ms** |
| `slow_normalize.as_source` | A + C | resolve, typecheck, evaluation | **evaluation** ~530 ms; resolve often ~1 ms (semisemantic) |
| `slow_multi.as_code` | **B** | `resolve_cold_cache_on` only | **~440 ms** |
| `slow_multi.as_source` | A + C | resolve, typecheck, evaluation | **evaluation** ~440 ms; resolve often ~8 ms |
| `slow_walk.as_code` | A | resolve, typecheck, evaluation | **resolve** ~530 ms |
| `slow_walk.as_source` | A | resolve, typecheck, evaluation | **resolve** ~535 ms (parity after denoted reuse) |

**Mode A** = prep with cache on; timed `resolve` uses semantic cache off,
semisemantic still on. **Mode B** = parse-only prep; fresh `XDG_CACHE_HOME` per
`resolve_cold_cache_on` sample.

Code variants `eval`, `typecheck`, `normalize`, and `multi` use Mode B because
Mode A prep warmed semisemantic and made all three phases look ~μs.

## Expected behaviour

- **Plain `Code` (Mode B rows)**: parse/typecheck/normalize burden on cold
  resolve; eval fixture’s cost is inside the resolved import graph.
- **`as Source` (Mode A rows)**: hashed children still pay Code hash-check on
  resolve; normalize/eval cost often shifts to **evaluation** when the Source
  product keeps an unnormalized fold.
- **`slow_walk`**: probes second Source structural walk after Code load; should
  be near-parity between modes after denoted-AST reuse.

## Reference timings (full suite, opportunistic cache on)

Approximate numbers from a recent M1 run:

| Variant | `as_code` | `as_source` |
|---------|-----------|-------------|
| `parse` resolve | ~584 ms | ~586 ms |
| `eval` | cold **425 ms** | eval **222 ms** |
| `typecheck` | cold **425 ms** | resolve+tc **225 ms** |
| `normalize` | cold **534 ms** | eval **536 ms** |
| `multi` | cold **437 ms** | eval **436 ms** |
| `walk` resolve | ~532 ms | ~535 ms |

Cold `dhall hash` with empty cache (alternative to Mode B):

| Variant | `as_code` | `as_source` |
|---------|-----------|-------------|
| `normalize` | ~0.55 s | ~1.07 s |
| `multi` | ~0.45 s | ~0.89 s |
| `walk` | ~0.56 s | ~0.56 s |

## Commands

```sh
cd dhall && stack bench evaluation --ba '--pattern large6'
stack bench evaluation --ba '--pattern slow_normalize'
stack bench evaluation --ba '--pattern resolve_cold_cache_on'
stack exec -- dhall resolve --file ./benchmark/evaluation/large6/pipeline-code-long-parse.dhall
```
