let Name = ./Type.dhall

let Self = ./Type.dhall

let WordOrNumber = ./WordOrNumber/package.dhall

in  \(name : Self) ->
        (let head = WordOrNumber.Type.Word name.head in [ head ] # name.tail)
      : List WordOrNumber.Type
