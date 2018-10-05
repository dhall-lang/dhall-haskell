    let Foo = < Bar : {} | Baz : {} >

in    λ(a : Type)
    → λ(f : {} → a)
    → λ(ts : Foo)
    → merge a { Bar = λ(a : {}) → f a, Baz = f } ts
