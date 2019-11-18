let foo = 1

in    λ(bar : Integer)
    → let exposePort =
              λ(portSpec : { ext : Integer, int : Integer })
            → Integer/show portSpec.ext ++ ":" ++ Integer/show portSpec.int

      in  let exposeSamePort =
                λ(port : Integer) → exposePort { ext = port, int = port }

          in  { blah = bar }
