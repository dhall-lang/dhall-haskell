  ∀(JSON : Type)
→ ∀ ( json
    : { array :
          List JSON → JSON
      , bool :
          Bool → JSON
      , null :
          JSON
      , number :
          Double → JSON
      , object :
          List { mapKey : Text, mapValue : JSON } → JSON
      , string :
          Text → JSON
      }
    )
→ JSON
