../../../../../Prelude/Optional/build
Integer
( λ(optional : Type)
→ λ(just : Integer → optional)
→ λ(nothing : optional)
→ just 1
)
