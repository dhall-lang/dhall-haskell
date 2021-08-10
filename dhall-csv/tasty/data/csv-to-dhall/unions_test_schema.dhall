let EmptyAlternatives    = < Option1 | Option2 | Option3 >
let NonEmptyAlternatives = < NaturalOption : Natural | BoolOption : Bool | DoubleOption : Double | TextOption : Text>
let MixedAlternatives    = < EmptyAlternative | NonEmptyAlternative : Integer >
in
List
    { union1 : EmptyAlternatives
    , union2 : NonEmptyAlternatives
    , union3 : MixedAlternatives
    }
