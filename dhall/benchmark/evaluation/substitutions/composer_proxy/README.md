# composer_proxy — synthetic substitution-heavy end-to-end bench

Generated fixture for a substitution-heavy import load: many modules whose
**bodies are wide record ASTs** (dozens of fields), a large Haskell-API
substitution map, then cold resolve → typecheck → normalize via
`Dhall.resolveWithSettings`.

“Wide” here means **many `Dhall.Map` nodes in the expression tree**, not long
source text or a heavy beta-reduction. Each module is short to parse; cost
shows up when walking/substituting/typechecking/normalizing the large record
type and value. The normal form is correspondingly large in **node count**
(same shape as the value), not because of deep computation.

| Signal | Choice |
|--------|--------|
| Many Code imports | 400 generated modules |
| Large `InputSettings` substitution map | 400 closed `UserType*` record **types** (64 fields each) |
| Wide AST (Map / denote / typecheck / NF size) | 64-field Natural **records** under `λ(_ : UserType000) → …` |
| Library substitution path | `Dhall.resolveWithSettings` |
| Checked normal form | Cold **resolve → typeOf → normalize** per sample |
| Code vs Source | `pipeline-code.dhall` / `pipeline-source.dhall` |

## Relation to other substitution probes

- Prefer `substitutions.many_files` for resolve-only cost of many **small**
  imports (`let a = i let x = 1 in a + x`) with a colliding substitution map.
- Prefer this group when the interesting cost is the combined
  import + typecheck + normalize path on **wide-record** ASTs.

## Harness

Generated at prep into a temp directory (not committed). Mode **D**:

| Step | Behavior |
|------|----------|
| Prep | Generate tree; parse pipeline only |
| `end_to_end_cold` | Fresh `XDG_CACHE_HOME`; `resolveWithSettings` → `typeOf` → `normalize` |

| Group | Benchmark |
|-------|-----------|
| `substitutions.composer_proxy.as_code` | `end_to_end_cold` |
| `substitutions.composer_proxy.as_source` | `end_to_end_cold` |

```sh
stack bench evaluation --ba '--pattern substitutions.composer_proxy'
```
