  λ(_ : Type)
→ λ ( _
    : { array : List _ → _@1
      , bool : Bool → _@1
      , null : _
      , number : Double → _@1
      , object : List { mapKey : Text, mapValue : _ } → _@1
      , string : Text → _@1
      }
    )
→ _.object
    [ { mapKey = "array", mapValue = _.array ([] : List _@1) }
    , { mapKey = "bool", mapValue = _.bool False }
    , { mapKey = "null", mapValue = _.null }
    , { mapKey = "number", mapValue = _.number 1.0 }
    , { mapKey = "string", mapValue = _.string "ABC" }
    ]
