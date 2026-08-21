This example is taken from github:uwedeportivo/ds-dhall

Run `dhall --file generate-example.dhall`. This is a medium customization tree
over a large generated record/schema. The Code-mode fixture does **not** use
`as Source`; resolving it can OOM because `apply-*` combinators are normalized
under binders. That is an intentional real-world failing benchmark until
import-resolution inefficiencies are fixed.

An `as Source` variant (`generate-example-source.dhall`, group `large4.source`)
completes in a few seconds.

## Benchmark groups

| Group | File | Notes |
|-------|------|-------|
| `large4` | `generate-example.dhall` | Code; may OOM (~100 GB NF during resolve) |
| `large4.source` | `generate-example-source.dhall` | Internal `as Source` on `apply-all.dhall` |

See `../README.md` for phase-benchmark interpretation (prep warms disk caches;
timed resolve uses the semantic cache off).

Subdirectory `base/` holds the large generated record/schema used by the
customization combinators.
