-- The printed normal form has size about 16 * 10^N bytes.
-- The CBOR representation is about 10^N bytes.
let n = 7

let h = λ(x : Natural) → λ(b : Bool) → if b then x else x + 1

let k =
    -- Dhall does not notice that `Natural/isZero` is already checked.
      λ(x : Natural) →
        if    Natural/isZero x
        then  h x (Natural/isZero x)
        else  h (x + 1) (Natural/isZero x)

let r =
    -- Compute 2 * n + p in a complicated way.
      λ(p : Natural) → Natural/fold n Natural k p

in  r
