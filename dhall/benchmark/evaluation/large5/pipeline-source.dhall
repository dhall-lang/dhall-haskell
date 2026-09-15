let Large5 = ./package.dhall as Source

let Render = Large5.Render

let c = Large5.Configuration.Global::{=}

in  Render c
