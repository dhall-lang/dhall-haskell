let outer =
      let a = ./a.dhall
      in a
in
let inner =
      let a = ./b.dhall
      in a
in { outer, inner }
