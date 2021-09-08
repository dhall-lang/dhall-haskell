[ { union1 = < Option1 | Option2 | Option3 >.Option1
  , union2 =
      < BoolOption : Bool
      | DoubleOption : Double
      | NaturalOption : Natural
      | TextOption : Text
      >.DoubleOption
        42.0
  , union3 =
      < EmptyAlternative | NonEmptyAlternative : Integer >.EmptyAlternative
  }
, { union1 = < Option1 | Option2 | Option3 >.Option2
  , union2 =
      < BoolOption : Bool
      | DoubleOption : Double
      | NaturalOption : Natural
      | TextOption : Text
      >.BoolOption
        True
  , union3 =
      < EmptyAlternative | NonEmptyAlternative : Integer >.NonEmptyAlternative
        -1
  }
, { union1 = < Option1 | Option2 | Option3 >.Option3
  , union2 =
      < BoolOption : Bool
      | DoubleOption : Double
      | NaturalOption : Natural
      | TextOption : Text
      >.DoubleOption
        3.14
  , union3 =
      < EmptyAlternative | NonEmptyAlternative : Integer >.EmptyAlternative
  }
, { union1 = < Option1 | Option2 | Option3 >.Option1
  , union2 =
      < BoolOption : Bool
      | DoubleOption : Double
      | NaturalOption : Natural
      | TextOption : Text
      >.DoubleOption
        NaN
  , union3 =
      < EmptyAlternative | NonEmptyAlternative : Integer >.NonEmptyAlternative
        +1
  }
]
