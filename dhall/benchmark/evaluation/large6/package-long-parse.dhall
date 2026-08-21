-- Single hash-protected child with parse-heavy burden (~0.5s to parse generated `slow/parse.dhall`).
let slow =
      ./slow/parse.dhall
        sha256:27abdeddfe8503496adeb623466caa47da5f63abd2bc6fa19f6cfcb73ecfed70

in  { result = slow }
