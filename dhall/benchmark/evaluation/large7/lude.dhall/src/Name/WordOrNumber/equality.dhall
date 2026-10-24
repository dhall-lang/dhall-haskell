let Self = ./Type.dhall

let Typeclasses = ../../Deps/Typeclasses.dhall

let order = ./order.dhall

in    Typeclasses.Classes.Ordering.toEquality Self order
    : Typeclasses.Classes.Equality.Type Self
