-- Same as pipeline.dhall, but the package is imported `as Source`.
-- Normal form size is still about 193 MB after evaluation.

let Sourcegraph = ./package.dhall as Source

let Render = Sourcegraph.Render

let c = Sourcegraph.Configuration.Global::{=}

in  Render c
