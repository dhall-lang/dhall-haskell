let simpleAssert = assert : 1 + 1 ≡ 2

let assertIn1Lam = λ(n : Natural) → assert : Natural/subtract 0 n ≡ n

let assertIn2Lams =
        λ(m : Natural)
      → λ(n : Natural)
      → assert : Natural/subtract m m ≡ Natural/subtract n n

let assertInLetInLam = λ(m : Natural) → let n = m + 0 in assert : m ≡ n

in  {=}
