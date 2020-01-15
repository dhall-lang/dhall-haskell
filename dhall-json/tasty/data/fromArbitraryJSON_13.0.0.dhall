  λ(JSON : Type)
→ λ ( json
    : { array : List JSON → JSON
      , bool : Bool → JSON
      , double : Double → JSON
      , integer : Integer → JSON
      , null : JSON
      , object : List { mapKey : Text, mapValue : JSON } → JSON
      , string : Text → JSON
      }
    )
→ json.object
    [ { mapKey = "double", mapValue = json.double 1.0 }
    , { mapKey = "string", mapValue = json.string "ABC" }
    , { mapKey = "null", mapValue = json.null }
    , { mapKey = "array", mapValue = json.array ([] : List JSON) }
    , { mapKey = "integer", mapValue = json.double 1.0 }
    , { mapKey = "bool", mapValue = json.bool False }
    ]
