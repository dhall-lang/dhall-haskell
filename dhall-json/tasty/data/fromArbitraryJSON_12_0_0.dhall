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
    ( toMap
        { array = json.array ([] : List JSON)
        , bool = json.bool False
        , null = json.null
        , number = json.number 1.0
        , string = json.string "ABC"
        }
    )
