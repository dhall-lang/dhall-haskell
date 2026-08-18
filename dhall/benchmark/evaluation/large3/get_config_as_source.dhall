-- Normal form size is about 60 KB.
let p = ./package.dhall as Source

in  p.Configuration.Global::{=}
