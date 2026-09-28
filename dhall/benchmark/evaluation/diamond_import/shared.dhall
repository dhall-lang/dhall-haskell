-- Shared, unhashed import. Expensive to beta-normalize, same idiom as
-- `../large6/slow/normalize.dhall`.
let a = 0

let f = λ(x : Natural) → x + 1

let factor = 10000000

in  Natural/fold factor Natural f a
