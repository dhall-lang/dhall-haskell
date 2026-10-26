{-|
Natural number ordering instance.

This instance implements the `Ordering` typeclass for `Natural` numbers using
Dhall's built-in comparison functions to provide total ordering.

## Implementation Strategy
Uses a cascaded comparison approach:
1. Check if `left < right` using `Natural/lessThan` → `Order.Less`
2. Check if `right < left` using `Natural/lessThan` → `Order.Greater`
3. Otherwise they must be equal → `Order.Equal`

## Usage
```dhall
let naturalOrdering = ./ordering.dhall
let result1 = naturalOrdering.order 3 7   -- Order.Less
let result2 = naturalOrdering.order 7 3   -- Order.Greater
let result3 = naturalOrdering.order 5 5   -- Order.Equal
```

## Laws
This implementation satisfies all Ordering laws:
- **Totality**: Every pair of naturals has exactly one ordering relationship
- **Reflexivity**: `order x x = Equal` for all naturals `x`
- **Antisymmetry**: If `order x y = Equal` then `order y x = Equal`
- **Transitivity**: Ordering relationships compose correctly

## Efficiency
The implementation is efficient, using at most two comparison operations
per call and leveraging Dhall's optimized natural number operations.
-}
let Prelude = ../../Deps/Prelude.dhall

let Ordering = ../../Classes/Ordering/Type.dhall

let Order = ../../Classes/Ordering/Order/Type.dhall

let order
    : Natural -> Natural -> Order
    = \(left : Natural) ->
      \(right : Natural) ->
        if    Prelude.Natural.lessThan left right
        then  Order.Less
        else  if Prelude.Natural.lessThan right left
        then  Order.Greater
        else  Order.Equal

in  { order } : Ordering Natural
