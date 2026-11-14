-- Shared, unhashed "prelude" import, reached transitively through
-- different site files (site1.dhall .. site6.dhall) rather than being
-- imported directly from one file. Bundles a couple of ordinary helper
-- functions together with one expensive field, the way a real Prelude
-- mixes cheap and costly definitions.
let increment = λ(x : Natural) → x + 1

let double = λ(x : Natural) → x * 2

let factor = 10000000

let expensive = Natural/fold factor Natural increment 0

in  { increment, double, expensive }
