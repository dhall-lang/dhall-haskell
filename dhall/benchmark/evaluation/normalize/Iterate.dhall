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

in  List/length Natural (iterate limit Natural (λ(x : Natural) → x + 1) 0)
