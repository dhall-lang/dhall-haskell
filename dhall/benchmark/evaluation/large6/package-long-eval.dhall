-- Single hash-protected child with eval-heavy burden (~0.5s when `offset 1` runs).
let offset =
      ./slow/eval.dhall
        sha256:860dc8715d1ea8e6aa086947916560dc5a4daed89c573734c17f51c6338455e2

in  { result = 1 + offset 1 }
