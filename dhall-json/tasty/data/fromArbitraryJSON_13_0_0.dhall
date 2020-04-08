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
    ( toMap
        { array = json.array ([] : List JSON)
        , bool = json.bool False
        , double = json.double 1.5
        , integer = json.integer +1
        , null = json.null
        , string = json.string "ABC"
        }
    )
