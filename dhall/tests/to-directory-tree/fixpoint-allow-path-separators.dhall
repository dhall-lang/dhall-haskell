let User = (./fixpoint-helper.dhall).User

let Group = (./fixpoint-helper.dhall).Group

let Mode = (./fixpoint-helper.dhall).Mode

let Make = (./fixpoint-helper.dhall).Make

in  \(r : Type) ->
    \(make : Make r) ->
      [ make.file
          { name = "non-existent-1/file"
          , content = ""
          , user = None User
          , group = None Group
          , mode = None Mode
          }
      , make.directory
          { name = "non-existent-2/directory"
          , content = [] : List r
          , user = None User
          , group = None Group
          , mode = None Mode
          }
      ]
