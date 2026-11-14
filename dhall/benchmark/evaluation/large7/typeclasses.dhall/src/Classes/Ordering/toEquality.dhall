{-|
Convert an `Ordering` instance to an `Equality` instance.

This function demonstrates the relationship between `Ordering` and `Equality`:
every type that has a total ordering also has equality (two values are equal
if they are neither less than nor greater than each other).

## Parameters
- `A : Type` - The type for which to create the equality instance
- `ordering : Ordering A` - The ordering instance to convert

## Returns
An `Equality A` instance that uses the ordering's `equal` function

## Usage
```dhall
let naturalOrdering = ../../Instances/Natural/ordering.dhall
let derivedEquality = toEquality Natural naturalOrdering
let isEqual = derivedEquality.equal 5 5  -- True
```

## Implementation Notes
This conversion is always valid because any total ordering implies equality.
The derived equality function uses the ordering's comparison to determine equality.
-}
-- Adapt an instance of `Ordering` to `Equality`.
let Ordering = ./Type.dhall

let Equality = ../Equality/Type.dhall

in  \(A : Type) ->
    \(ordering : Ordering A) ->
      { equal = ./equal.dhall A ordering } : Equality A
