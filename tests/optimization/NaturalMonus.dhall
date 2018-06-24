λ(x : Natural) →
λ(y : Natural) →
  Natural/fold y Natural (
    λ(n : Natural) → (
      Natural/fold n
                   { prev : Natural, next : Natural }
                   (λ(p : { prev : Natural, next : Natural }) → { prev = p.next, next = p.next + 1})
                   { prev = 0, next = 0 }).prev
  ) x
