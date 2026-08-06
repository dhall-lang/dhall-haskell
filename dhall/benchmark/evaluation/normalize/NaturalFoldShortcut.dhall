-- Reach a fixed point after 100 steps; Natural/fold should short-circuit the
-- remaining ~1e8 iterations when the accumulator is a Natural literal.
Natural/fold 100000000 Natural (λ(x : Natural) → Natural/subtract 1 x) 100
