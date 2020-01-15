  λ(JSON : Type)
→ λ ( json
    : { array : List JSON → JSON
      , bool : Bool → JSON
      , null : JSON
      , number : Double → JSON
      , object : List { mapKey : Text, mapValue : JSON } → JSON
      , string : Text → JSON
      }
    )
→ json.object
    [ { mapKey = "string", mapValue = json.string "ABC" }
    , { mapKey = "null", mapValue = json.null }
    , { mapKey = "array", mapValue = json.array ([] : List JSON) }
    , { mapKey = "number", mapValue = json.number 1.0 }
    , { mapKey = "bool", mapValue = json.bool False }
    ]
