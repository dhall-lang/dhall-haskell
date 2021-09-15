let U = <A : {} | B : Natural >
in
  { recs = [ U.A {=} ]
  , nats = [ U.B 1 ]
  }
