let limit = 300000

let iterate =
      λ(n : Natural) →
      λ(a : Type) →
      λ(f : a → a) →
      λ(x : a) →
        List/reverse a (List/build
          a
          ( λ(list : Type) →
            λ(cons : a → list → list) →
            λ(nil : list) →
              let state =
                    Natural/fold
                      n
                      { next : a, rest : list }
                      ( λ(p : { next : a, rest : list }) →
                          { next = f p.next, rest = cons p.next p.rest }
                      )
                      { next = x, rest = nil }

              in  state.rest
          ))

let a =
      List/length
        (List Natural)
        (iterate limit (List Natural) (λ(x : List Natural) → x # [ 1 ]) [ 1 ])

let b = List/length Natural (iterate limit Natural (λ(x : Natural) → x + 1) 1)

in  a
