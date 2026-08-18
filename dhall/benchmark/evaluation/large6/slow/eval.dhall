-- Artificial evaluation burden: ~0.5 seconds when this function is applied,
-- e.g. `long-eval 1`. The cost is in `Natural/fold` over `(x * factor)` steps
-- with `factor = 2400000`; parsing and type-checking the function itself are cheap.
-- evaluation takes about 0.5 seconds
λ(x : Natural) →
  let a = 12354123412341234123412341234123

  let b = 59123123481203981209837412098374

  let factor = 2400000

  let f = λ(x : Natural) → Natural/subtract x b

  let n = Natural/fold (x * factor) Natural f a

  let n1 = Natural/subtract 1 n

  in  Natural/subtract n n1
