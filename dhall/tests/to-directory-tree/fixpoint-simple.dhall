let User = (./fixpoint-helper.dhall).User

let Group = (./fixpoint-helper.dhall).Group

let Mode = (./fixpoint-helper.dhall).Mode

let Make = (./fixpoint-helper.dhall).Make

in  \(r : Type) ->
    \(make : Make r) ->
        [ make.text-file
            { name = "file"
            , content = ""
            , user = None User
            , group = None Group
            , mode = None Mode
            }
        , make.directory
            { name = "directory"
            , content = [] : List r
            , user = None User
            , group = None Group
            , mode = None Mode
            }
        ]
