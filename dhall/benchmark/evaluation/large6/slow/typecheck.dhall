-- Artificial type-check burden: ~0.5 seconds while type-checking this file.
-- The cost is not in parsing this small file; the `assert` forces evaluation of
-- the frozen `./eval.dhall` import once during type-checking.
-- typechecking this file evaluates long-eval once (~0.5 seconds)
let long-eval =
      ./eval.dhall
        sha256:860dc8715d1ea8e6aa086947916560dc5a4daed89c573734c17f51c6338455e2

let _ = assert : long-eval 1 === 0

in  True
