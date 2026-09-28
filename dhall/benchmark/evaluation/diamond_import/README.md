# diamond_import: unhashed shared import, multiple sites

A shared unhashed import used at six sites should normalize once and reuse the result, not re-normalize per site. Run with `cd dhall && stack bench evaluation --ba '--pattern diamond_import'` and check the `diamond_import.end_to_end_cold` row.
