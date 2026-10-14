-- evaluation takes about 1 second with MacBook Pro M1
λ(x : Natural) →
  let a = 12354123412341234123412341234123

  let b = 59123123481203981209837412098374

  let factor = 6000000

  let f = λ(x : Natural) → Natural/subtract x b

  let n = Natural/fold (x * factor) Natural f a

  let n1 = Natural/subtract 1 n

  in  Natural/subtract n n1
