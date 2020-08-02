{-|
Example taken from [dhall documentation](https://docs.dhall-lang.org/tutorials/Language-Tour.html#prelude)
-}


let List/generate = https://prelude.dhall-lang.org/v15.0.0/List/generate

in List/generate 10 Text (\(n : Natural) -> "Result #${Natural/show n}")

