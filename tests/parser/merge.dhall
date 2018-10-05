  λ(x : <>)
→ { bar = merge Integer {=} x
  , foo =
      merge
	  < Left : Bool | Right : Natural >
      { Left = λ(b : Bool) → b, Right = Natural/even }
      < Left = True | Right : Natural >
  }
