-- Distinct frozen child #1 for the multi-child large6 variant.
-- Medium normalize burden (~50ms) so eight children amplify Code-mode
-- hash-check + Source expand work under an `as Source` root.
let tag = 1

let n = Natural/fold 1000000 Natural (λ(x : Natural) → x + 1) 0

in  { tag, n }
