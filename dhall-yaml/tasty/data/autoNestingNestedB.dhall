let Inner = < I0 | I1 : { i1 : Bool } >

let Outer = < A | B : { b : Inner } >

in Outer.B { b = Inner.I1 { i1 = True } }
