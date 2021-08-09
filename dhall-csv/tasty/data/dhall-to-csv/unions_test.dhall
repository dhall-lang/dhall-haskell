let EmptyAlternatives    = < Option1 | Option2 | Option3 >
let NonEmptyAlternatives = < NaturalOption : Natural | BoolOption : Bool | DoubleOption : Double | TextOption : Text>
let MixedAlternatives    = < EmptyAlternative | NonEmptyAlternative : Integer >
in
[ { union1 = EmptyAlternatives.Option1
  , union2 = NonEmptyAlternatives.NaturalOption 42
  , union3 = MixedAlternatives.EmptyAlternative
  }
, { union1 = EmptyAlternatives.Option2
  , union2 = NonEmptyAlternatives.BoolOption True
  , union3 = MixedAlternatives.NonEmptyAlternative -1
  }
, { union1 = EmptyAlternatives.Option3
  , union2 = NonEmptyAlternatives.DoubleOption 3.14
  , union3 = MixedAlternatives.EmptyAlternative
  }
, { union1 = EmptyAlternatives.Option1
  , union2 = NonEmptyAlternatives.TextOption "Hello world"
  , union3 = MixedAlternatives.NonEmptyAlternative +1
  }
]
