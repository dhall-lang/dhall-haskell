-- Single hash-protected import-free child with a large AST (~0.5s per
-- structural traverse/denote). Probes the second Source walk after Code load.
let slow =
      ./slow/walk.dhall
        sha256:e93358155302e83205fedb47ae65c9e3b4b7c3026ca6a18573111a92edc8611f

in  { result = slow }
