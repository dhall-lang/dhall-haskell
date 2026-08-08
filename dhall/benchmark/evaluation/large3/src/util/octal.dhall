let Octal = < Oo777 | Oo600 >

let toNatural
    : Octal → Natural
    = λ(o : Octal) → merge { Oo777 = 511, Oo600 = 384 } o

let example0 = assert : toNatural Octal.Oo777 ≡ 511

let example1 = assert : toNatural Octal.Oo600 ≡ 384

in  { Enum = Octal, toNatural }
