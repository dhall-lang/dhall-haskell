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
    [ { mapKey = "array", mapValue = json.array ([] : List JSON) }
    , { mapKey = "bool", mapValue = json.bool False }
    , { mapKey = "null", mapValue = json.null }
    , { mapKey = "number", mapValue = json.number 1.0 }
    , { mapKey = "string", mapValue = json.string "ABC" }
    ]
