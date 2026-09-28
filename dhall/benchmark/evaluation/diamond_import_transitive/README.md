# diamond_import_transitive: unhashed shared import, reached transitively

Like `../diamond_import`, but the sites reach the shared unhashed
import indirectly (`main.dhall` --> `site1..6.dhall` --> `prelude.dhall`).
`prelude.dhall` also mixes cheap helper functions in with the expensive field, and each site projects out and uses one piece of it (`Prelude.double Prelude.expensive`), rather than
re-exporting the whole import untouched. This is closer to how application code
actually uses a shared prelude.

Run with `cd dhall && stack bench evaluation --ba '--pattern diamond_import_transitive'` and compare the `diamond_import_transitive.end_to_end_cold` row to `diamond_import`'s.
