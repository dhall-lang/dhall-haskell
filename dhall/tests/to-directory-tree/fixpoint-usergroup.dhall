let User = (./fixpoint-helper.dhall).User

let Group = (./fixpoint-helper.dhall).Group

let Mode = (./fixpoint-helper.dhall).Mode

let Make = (./fixpoint-helper.dhall).Make

in  \(r : Type) ->
    \(make : Make r) ->
      [ make.file
          { name = "ids"
          , content = ""
          , user = Some (User.UserId 0)
          , group = Some (Group.GroupId 0)
          , mode = None Mode
          }
      , make.file
          { name = "names"
          , content = ""
          , user = Some (User.UserName "user")
          , group = Some (Group.GroupName "group")
          , mode = None Mode
          }
      ]
