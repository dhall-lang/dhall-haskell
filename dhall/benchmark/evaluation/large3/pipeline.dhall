-- Normal form size is 193 MB.

let Sourcegraph = ./package.dhall

let Render = Sourcegraph.Render

let c = Sourcegraph.Configuration.Global::{=}

in  Render c
