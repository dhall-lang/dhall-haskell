let Component1/Generate = ./1/generate.dhall

let Component2/Generate = ./2/generate.dhall

let Component3/Generate = ./3/generate.dhall

let Component4/Generate = ./4/generate.dhall

let Configuration/global = ../configuration/global.dhall

let Render =
      λ(c : Configuration/global.Type) →
        Component1/Generate c
        # Component2/Generate c
        # Component3/Generate c
        # Component4/Generate c

in  Render
