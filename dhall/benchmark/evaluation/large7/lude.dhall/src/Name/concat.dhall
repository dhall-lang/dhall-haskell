let Prelude = ../Deps/Prelude.dhall

let Name = ./Type.dhall

let Self = ./Type.dhall

let WordOrNumber = ./WordOrNumber/package.dhall

let toWordOrNumberList = ./toWordOrNumberList.dhall

in  \(head : Self) ->
    \(tail : List Self) ->
        { head = head.head
        , tail =
              head.tail
            # Prelude.List.concatMap
                Self
                WordOrNumber.Type
                toWordOrNumberList
                tail
        }
      : Self
