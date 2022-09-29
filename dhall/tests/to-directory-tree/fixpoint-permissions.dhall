let User = (./fixpoint-helper.dhall).User

let Group = (./fixpoint-helper.dhall).Group

let Make = (./fixpoint-helper.dhall).Make

let no-access = { execute = Some False, read = Some False, write = Some False }

let full-access = { execute = Some True, read = Some True, write = Some True }

in  \(r : Type) ->
    \(make : Make r) ->
      [ make.file
          { name = "file"
          , content = ""
          , user = None User
          , group = None Group
          , mode = Some
            { user = Some full-access
            , group = Some no-access
            , other = Some no-access
            }
          }
      ]
