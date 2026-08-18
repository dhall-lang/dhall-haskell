-- Artificial normalization burden: ~0.5 seconds during Code-mode import loading.
-- Unlike `eval.dhall` (a lambda that is already normal), this is a top-level
-- `Natural/fold` that must beta-reduce during resolve/hash-check of a Code import.
-- The normal form is the Natural literal `2400000`.
let a = 0

let f = λ(x : Natural) → x + 1

let factor = 10000000

in  Natural/fold factor Natural f a
