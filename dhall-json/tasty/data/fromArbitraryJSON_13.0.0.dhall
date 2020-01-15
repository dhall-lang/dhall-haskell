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
    [ { mapKey = "array", mapValue = json.array ([] : List JSON) }
    , { mapKey = "bool", mapValue = json.bool False }
    , { mapKey = "double", mapValue = json.double 1.5 }
    , { mapKey = "integer", mapValue = json.integer +1 }
    , { mapKey = "null", mapValue = json.null }
    , { mapKey = "string", mapValue = json.string "ABC" }
    ]
