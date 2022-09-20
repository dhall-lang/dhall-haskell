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

in  forall (result : Type) ->
      let DirectoryEntry = Entry (List result)

      let FileEntry = Entry Text

      let Make =
            { directory : DirectoryEntry -> result, file : FileEntry -> result }

      in  forall (make : Make) -> List result
