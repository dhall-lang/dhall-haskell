-- Optimized version of iterate.dhall using a Natural/fold with an accumulator of type { next : a, rest : list → list }.

let limit = 300000

let iterate =
      λ(n : Natural) →
      λ(a : Type) →
      λ(f : a → a) →
      λ(x : a) →
        List/build
          a
          ( λ(list : Type) →
            λ(cons : a → list → list) →
            λ(nil : list) →
              let state =
                    Natural/fold
                      n
                      { next : a, rest : list → list }
                      ( λ(p : { next : a, rest : list → list }) →
                          { next = f p.next
                          , rest = λ(tail : list) → p.rest (cons p.next tail)
                          }
                      )
                      { next = x, rest = λ(tail : list) → tail }

              in  state.rest nil
          )

let a =
      List/length
        (List Natural)
        (iterate limit (List Natural) (λ(x : List Natural) → x # [ 1 ]) [ 1 ])

let b = List/length Natural (iterate limit Natural (λ(x : Natural) → x + 1) 1)

in  a
