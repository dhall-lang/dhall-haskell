let Prelude = ../Deps/Prelude.dhall

in  \(Element : Type) ->
      let Result = Prelude.NonEmpty.Type Element

      in  \(ls : List Element) ->
            merge
              { None = None Result
              , Some =
                  \(head : Element) ->
                    Some { head, tail = Prelude.List.drop 1 Element ls }
              }
              (Prelude.List.head Element ls)
            : Optional Result
