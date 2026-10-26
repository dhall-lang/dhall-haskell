{-|
NonEmpty list ordering instance using lexicographic comparison.

This instance implements the `Ordering` typeclass for `NonEmpty` types by comparing
first the head elements, then the tail lists if heads are equal.

## Parameters
- `Element : Type` - The type of elements in the non-empty list
- `elementOrder : Ordering Element` - Ordering instance for the element type

## Algorithm
1. **Head comparison**: Compare head elements using element ordering
2. **Early termination**: If heads differ (`Less` or `Greater`), return that result
3. **Tail comparison**: If heads are equal, compare tail lists using List ordering

## Usage
```dhall
let NaturalOrdering = ../Natural/ordering.dhall
let nonEmptyOrdering = ordering Natural NaturalOrdering

let ne1 = { head = 1, tail = [2, 3] }
let ne2 = { head = 1, tail = [2, 4] }
let ne3 = { head = 2, tail = [1, 1] }

-- Head elements differ
let result1 = nonEmptyOrdering.order ne1 ne3  -- Order.Less (1 < 2)

-- Same heads, compare tails
let result2 = nonEmptyOrdering.order ne1 ne2  -- Order.Less ([2,3] < [2,4])

-- Completely equal
let ne4 = { head = 1, tail = [2, 3] }
let result3 = nonEmptyOrdering.order ne1 ne4  -- Order.Equal
```

## Implementation Details
- Uses pattern matching on the head comparison result
- Only evaluates tail comparison when heads are equal (lazy evaluation)
- Delegates to List ordering for tail comparison, maintaining consistency

## Efficiency
- **Best case**: O(1) when heads differ
- **Worst case**: O(min(m,n)) when comparing full structures
- **Leverages**: Optimized List ordering implementation for tails
-}
let Prelude = ../../Deps/Prelude.dhall

let Ordering = ../../Classes/Ordering/Type.dhall

let Order = ../../Classes/Ordering/Order/Type.dhall

let listOrdering = ../List/ordering.dhall

let order =
      \(Element : Type) ->
      \(elementOrder : Ordering Element) ->
      \(left : Prelude.NonEmpty.Type Element) ->
      \(right : Prelude.NonEmpty.Type Element) ->
        merge
          { Less = Order.Less
          , Greater = Order.Greater
          , Equal =
              (listOrdering Element elementOrder).order
                left.tail
                right.tail
          }
          (elementOrder.order left.head right.head)

in  \(Element : Type) ->
    \(elementOrder : Ordering Element) ->
        { order = order Element elementOrder }
      : Ordering (Prelude.NonEmpty.Type Element)
