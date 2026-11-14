{-|
Test for equality using an `Ordering` instance.

This function extracts equality from ordering by testing if the comparison
result is `Equal`. This is the canonical way to derive equality from ordering.

## Parameters
- `A : Type` - The type of values to compare
- `ordering : Ordering A` - The ordering instance to use
- `left : A` - The first value to compare
- `right : A` - The second value to compare

## Returns
`True` if the values are equal according to the ordering, `False` otherwise

## Usage
```dhall
let naturalOrdering = ../../Instances/Natural/ordering.dhall
let isEqual = equal Natural naturalOrdering 5 5  -- True
let notEqual = equal Natural naturalOrdering 3 7  -- False
```

## Implementation
Uses pattern matching on the `Order` result to extract the equality case.
-}
let Ordering = ./Type.dhall

in  \(A : Type) ->
    \(ordering : Ordering A) ->
    \(left : A) ->
    \(right : A) ->
      merge
        { Less = False, Equal = True, Greater = False }
        (ordering.order left right)
