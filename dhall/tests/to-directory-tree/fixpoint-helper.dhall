let User = < UserId : Natural | UserName : Text >

let Group = < GroupId : Natural | GroupName : Text >

let Access =
      { execute : Optional Bool, read : Optional Bool, write : Optional Bool }

let Mode =
      { user : Optional Access
      , group : Optional Access
      , other : Optional Access
      }

let Entry =
      \(content : Type) ->
        { name : Text
        , content : content
        , user : Optional User
        , group : Optional Group
        , mode : Optional Mode
        }

let Make =
      \(r : Type) -> { directory : Entry (List r) -> r, binary-file : Entry Bytes -> r, text-file : Entry Text -> r }

in  { User, Group, Access, Mode, Entry, Make }
