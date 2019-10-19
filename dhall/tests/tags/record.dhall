{- You can optionally add types

   `x : T` means that `x` has type `T`
-}

let Config : Type =
      {- What happens if you add another field here? -}
      { home : Text
      , privateKey : Text
      , publicKey : Text
      , name : { firstName : Text
               , secondName : Text
               }
      }

let makeUser : Text -> Config = \(user : Text) ->
      let home       : Text   = "/home/${user}"
      let privateKey : Text   = "${home}/.ssh/id_ed25519"
      let publicKey  : Text   = "${privateKey}.pub"
      let config     : Config =
            { home       = home
            , privateKey = privateKey
            , publicKey  = publicKey
            , name = { firstName = user
                     , secondName = ""
                     }
            }
      in  config

let configs : List Config =
      [ makeUser "bill"
      , makeUser "jane"
      ]

in  configs
