This benchmark isolates the cost of a **single hash-protected slow child import**
under plain `Code` versus `as Source`.

Each variant injects one frozen child from `slow/`. The artificial burden is
about **0.5 seconds per activation** on a MacBook Pro M1 (roughly linear when
scaled up or down).

| Variant | Where the burden lives | What gets slow |
|---------|------------------------|----------------|
| `parse` | `./slow/parse.dhall` | **Parsing** ~1.3M lines of nested `{--}` comments (~0.5s cold) |
| `eval` | `./slow/eval.dhall` | **Evaluation** of a heavy `Natural/fold` when the function is applied (~0.5s for `f 1`) |
| `typecheck` | `./slow/typecheck.dhall` | **Type-checking** an `assert` that must evaluate the frozen eval import once (~0.5s) |

Packages (`package-long-*.dhall`) each import exactly one `sha256:`-protected
child from `slow/`. Pipelines compare the same package under two import modes:

- `pipeline-code-long-*.dhall` — `./package-long-*.dhall` (`Code`)
- `pipeline-source-long-*.dhall` — `./package-long-*.dhall as Source`

Expected behaviour (see `stack bench evaluation --ba '--pattern large6'`):

Benchmark groups are named `large6.slow_<burden>.as_<mode>`, for example
`large6.slow_parse.as_source` (frozen parse-heavy child, package imported
`as Source`) versus `large6.slow_parse.as_code` (same child, plain `Code`).

- **Plain `Code` (`as_code`)**: parse/typecheck burden shows on **resolve**; eval burden on **evaluation** (after import-time normalization).
- **`as Source` (`as_source`)**: parse- and typecheck-heavy children cost much more on **resolve** (multiple validation/finalization passes); eval-heavy cost shifts mainly to **evaluation**, not resolve.

Suggested commands:

```sh
cd dhall && stack bench evaluation --ba '--pattern large6'
cd dhall && stack bench evaluation --ba '--pattern slow_parse.as_source'
stack exec -- dhall resolve --file ./dhall/benchmark/evaluation/large6/pipeline-code-long-parse.dhall
stack exec -- dhall resolve --file ./dhall/benchmark/evaluation/large6/pipeline-source-long-parse.dhall
```
