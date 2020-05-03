[ { arbitraryJSON =
      λ(JSON : Type) →
      λ ( json
        : { array : List JSON → JSON
          , bool : Bool → JSON
          , null : JSON
          , number : Double → JSON
          , object : List { mapKey : Text, mapValue : JSON } → JSON
          , string : Text → JSON
          }
        ) →
        json.number 1.0
  , bool = True
  , double = 0.0
  , extraField = None Bool
  , integer = +0
  , natural = 0
  , null = None <>
  , optional = Some 1
  , preferDouble = 0.0
  , preferInteger = +0
  , text = "abc"
  , unifyEmpty = [] : List Natural
  , unifySimple = < Bool : Bool | Text : Text >.Bool True
  }
, { arbitraryJSON =
      λ(JSON : Type) →
      λ ( json
        : { array : List JSON → JSON
          , bool : Bool → JSON
          , null : JSON
          , number : Double → JSON
          , object : List { mapKey : Text, mapValue : JSON } → JSON
          , string : Text → JSON
          }
        ) →
        json.array ([] : List JSON)
  , bool = False
  , double = 1.1
  , extraField = Some True
  , integer = -1
  , natural = 1
  , null = None <>
  , optional = None Natural
  , preferDouble = 1.1
  , preferInteger = -1
  , text = "def"
  , unifyEmpty = [ 1 ]
  , unifySimple = < Bool : Bool | Text : Text >.Text "abc"
  }
]
