let image =
      { Type =
          { prefix : Optional Text
          , suffix : Optional Text
          , registry : Optional Text
          , omitRegistry : Bool
          , omitSha256 : Bool
          }
      , default =
        { prefix = None Text
        , suffix = None Text
        , registry = None Text
        , omitRegistry = False
        , omitSha256 = False
        }
      }

in  image
