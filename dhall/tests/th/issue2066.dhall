let Bar = { baz : Integer }

let Foo = { foo : Integer, bar : Bar }

let Qux = < Foo : Foo | Bar : Bar >

in  { Foo, Bar, Qux }
