{-|
List ordering instance using lexicographic comparison.

This instance implements the `Ordering` typeclass for `List` types by comparing
lists in lexicographic (dictionary) order - the same ordering used in most
programming languages and dictionaries.

## Parameters
- `Element : Type` - The type of elements in the list
- `elementOrder : Ordering Element` - Ordering instance for the element type

## Algorithm
1. **Element-wise comparison**: Zip lists and compare corresponding elements
2. **First difference wins**: Return the order of the first differing element pair
3. **Length comparison**: If all compared elements are equal, compare lengths

## Usage
```dhall
let NaturalOrdering = ../Natural/ordering.dhall
let listOrdering = ordering Natural NaturalOrdering

-- Lexicographic ordering examples
let r1 = listOrdering.order [1, 2, 3] [1, 2, 4]     -- Order.Less (3 < 4)
let r2 = listOrdering.order [1, 3] [1, 2, 5]        -- Order.Greater (3 > 2)
let r3 = listOrdering.order [1, 2] [1, 2, 3]        -- Order.Less (shorter)
let r4 = listOrdering.order [1, 2, 3] [1, 2]        -- Order.Greater (longer)
let r5 = listOrdering.order [] [1]                  -- Order.Less (empty < non-empty)
let r6 = listOrdering.order [1, 2, 3] [1, 2, 3]     -- Order.Equal
```

## Implementation Details
- Uses `List.zip` to pair corresponding elements
- `List.foldLeft` processes pairs until a difference is found
- Natural length comparison handles the case where all elements are equal
- Empty lists are handled correctly (empty < non-empty)

## Complexity
- **Time**: O(min(m,n)) where m, n are the list lengths
- **Space**: O(min(m,n)) for the zipped pairs
-}
let Prelude = ../../Deps/Prelude.dhall

let Ordering = ../../Classes/Ordering/Type.dhall

let Order = ../../Classes/Ordering/Order/Type.dhall

let naturalOrdering = ../Natural/ordering.dhall

let order =
      \(Element : Type) ->
      \(elementOrder : Ordering Element) ->
      \(left : List Element) ->
      \(right : List Element) ->
        let Pair = { _1 : Element, _2 : Element }

        let zipped = Prelude.List.zip Element left Element right

        let zippedOrder =
              Prelude.List.foldLeft
                Pair
                zipped
                Order
                ( \(state : Order) ->
                  \(pair : Pair) ->
                    merge
                      { Less = Order.Less
                      , Greater = Order.Greater
                      , Equal = elementOrder.order pair._1 pair._2
                      }
                      state
                )
                Order.Equal

        in  merge
              { Less = Order.Less
              , Greater = Order.Greater
              , Equal =
                  let leftLength = Prelude.List.length Element left

                  let rightLength = Prelude.List.length Element right

                  in  naturalOrdering.order leftLength rightLength
              }
              zippedOrder

in  \(Element : Type) ->
    \(elementOrder : Ordering Element) ->
      { order = order Element elementOrder } : Ordering (List Element)
