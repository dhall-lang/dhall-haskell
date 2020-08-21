{-|
On this example, `dhall-docs` will detect that the argument `a` of the only
function in this file is of record type. Therefore, any interactions on selector
expressions over the `a` variable will interact with the fields on the record-type.

Note that the treatment for `Lam` variables is different from `Let` bindings:
the annotation doesn't affect on the later case. This can be seen in the
[test case for let bindings](./JumpToRecordFieldWhenLetAnnotationPresentShouldIgnoreAnnotation.dhall)
-}

λ(b : Type) →
λ(c : Type) →
λ(a : { list : List b, cons : b → c → c, nil : c }) →
  List/fold b a.list c a.cons a.nil
