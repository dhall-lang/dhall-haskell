let Make = \(T: Type) -> { Type = { a: T },  default = {=} }

in
(Make Natural) with default = { a = 2 }
