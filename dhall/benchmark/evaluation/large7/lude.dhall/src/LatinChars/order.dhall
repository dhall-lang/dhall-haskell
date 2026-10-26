let Self = ./Type.dhall

let Typeclasses = ../Deps/Typeclasses.dhall

let Char = ../LatinChar/package.dhall

let Order = Typeclasses.Classes.Ordering

in    Typeclasses.Instances.NonEmpty.ordering Char.Type Char.order
    : Order.Type Self
