-- Single hash-protected child with normalize-heavy burden
-- (~0.5s to beta-normalize `slow/normalize.dhall` during Code-mode load).
let slow =
      ./slow/normalize.dhall
        sha256:d71e12343d6795905ceb32454fc6171b49295088e36ecfafe71489dce1f6fe30

in  { result = slow }
