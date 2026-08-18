-- Single hash-protected child with typecheck-heavy burden (~0.5s to type-check `slow/typecheck.dhall`).
let slow =
      ./slow/typecheck.dhall
        sha256:27abdeddfe8503496adeb623466caa47da5f63abd2bc6fa19f6cfcb73ecfed70

in  { result = slow }
