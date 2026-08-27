# large6 — isolated slow child imports

This benchmark isolates the cost of **hash-protected slow child imports**.

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
`slow/parse.dhall` is always regenerated during evaluation-benchmark setup, or
with `python3 slow/generate-parse.py`. It is not checked in.

Packages (`package-long-*.dhall`) import `sha256:`-protected children from
`slow/`. Pipelines:

- `pipeline-code-long-*.dhall` — `./package-long-*.dhall`

## Full benchmark matrix

Benchmark group names: `large6.slow_<burden>`.

| Group | Harness mode | Benchmarks | Where to read slow cost |
|-------|--------------|------------|-------------------------|
| `slow_parse` | A (phase) | resolve, typecheck, evaluation | **resolve** ~580 ms |
| `slow_eval` | **B (cold)** | `resolve_cold_cache_on` only | **~425 ms** cold resolve |
| `slow_typecheck` | **B** | `resolve_cold_cache_on` only | **~425 ms** |
| `slow_normalize` | **B** | `resolve_cold_cache_on` only | **~530 ms** |
| `slow_multi` | **B** | `resolve_cold_cache_on` only | **~440 ms** |
| `slow_walk` | A | resolve, typecheck, evaluation | **resolve** ~530 ms |

**Mode A** = prep with cache on; timed `resolve` uses semantic cache off,
semisemantic still on. **Mode B** = parse-only prep; fresh `XDG_CACHE_HOME` per
`resolve_cold_cache_on` sample.

Code variants `eval`, `typecheck`, `normalize`, and `multi` use Mode B because
Mode A prep warmed semisemantic and made all three phases look ~μs.

## Expected behaviour

- **(Mode B rows)**: parse/typecheck/normalize burden on cold
  resolve; eval fixture’s cost is inside the resolved import graph.
- **`slow_walk`**: probes second Source structural walk after Code load; should
  be near-parity between modes after denoted-AST reuse.


## Commands

```sh
cd dhall && stack bench evaluation --ba '--pattern large6'
stack bench evaluation --ba '--pattern slow_normalize'
stack bench evaluation --ba '--pattern resolve_cold_cache_on'
stack exec -- dhall resolve --file ./benchmark/evaluation/large6/pipeline-code-long-parse.dhall
```
