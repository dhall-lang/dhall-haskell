-- example0.dhall

let Person
    : Type
    =   ∀(Person : Type)
      → ∀(MakePerson : { children : List Person, name : Text } → Person)
      → Person

let example
    : Person
    =   λ(Person : Type)
      → λ(MakePerson : { children : List Person, name : Text } → Person)
      → MakePerson
        { children =
            [ MakePerson { children = [] : List Person, name = "Mary" }
            , MakePerson { children = [] : List Person, name = "Jane" }
            ]
        , name =
            "John"
        }

let everybody
    : Person → List Text
    = let concat = http://prelude.dhall-lang.org/List/concat
      
      in    λ(x : Person)
          → x
            (List Text)
            (   λ(p : { children : List (List Text), name : Text })
              → [ p.name ] # concat Text p.children
            )

let result : List Text = everybody example

in  result
