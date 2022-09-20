let Make = (./fixpoint-helper.dhall).Make

in  \(r : Type) -> \(make : Make r) -> [] : List r
