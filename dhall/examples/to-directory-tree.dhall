-- This is an example on how to build a directory tree using the so-called
-- fixpointed method. See the documenatation of the `Dhall.DirectoryTree` module
-- for further information on it.

-- First, define some types recognized by the `dhall to-directory-tree` command.

-- A user, either identified by its numeric user id or its name.
let User = < UserId : Natural | UserName : Text >

-- Similarly, a group.
let Group = < GroupId : Natural | GroupName : Text >

-- The following two type aliases are a well-typed represenation of the bitmask
-- for permissions used by the DAC access control found on Unix systems. See for
-- example the chmod(5) manual entry.

-- How much access we do grant...
let Access =
      { execute : Optional Bool, read : Optional Bool, write : Optional Bool }

-- ... for whom.
let Mode =
      { user : Optional Access
      , group : Optional Access
      , other : Optional Access
      }

-- A generic file system entry. It consists of a name, an abstract content and
-- some metadata which might be set (Some) or not (None).
let Entry =
      \(content : Type) ->
        { name : Text
        , content : content
        , user : Optional User
        , group : Optional Group
        , mode : Optional Mode
        }

-- This is the main program constructing our directory tree. It is a fixpoint
-- definition similar to how we deal with recursive types in arbitrary Dhall
-- programs but specialised to our use case. The first argument is the type of a
-- directory tree and the second one is a record where each field is a
-- constructor for a specific filesystem entry.
in  \(tree : Type) ->
    \ ( make
      : { directory : Entry (List tree) -> tree, file : Entry Text -> tree }
      ) ->

      -- Before we define the actual directory tree we define some Dhall schemas
      -- and shortcuts for convenience.

      -- A schema suitable for a directory...
      let Directory =
            { Type =
                { name : Text
                , content : List tree
                , user : Optional User
                , group : Optional Group
                , mode : Optional Mode
                }
            , default =
              { content = [] : List tree
              , user = None User
              , group = None Group
              , mode = None Mode
              }
            }

      -- ... and one for a file.
      let File =
            { Type =
                { name : Text
                , content : Text
                , user : Optional User
                , group : Optional Group
                , mode : Optional Mode
                }
            , default =
              { content = ""
              , user = None User
              , group = None Group
              , mode = None Mode
              }
            }

      -- Give someone full access to an filesystem entry.
      let full_access
          : Access
          = { execute = Some True, read = Some True, write = Some True }

      -- Give someone no access at all to an filesystem entry.
      let no_access
          : Access
          = { execute = Some False, read = Some False, write = Some False }

      -- These permissions
      --  * grant full access to the user.
      --  * retain the permissions of the primary group of the user.
      --  * deny access to everyone else.
      let semi_private
          : Mode
          = { user = Some full_access, group = None Access, other = Some no_access }

      -- Now let's start with the directory tree ...
      in    [ -- Some file with a gentle greeting. No metadata is set explicitly.
              make.file File::{ name = "some file", content = "Hello world!" }
              -- A directory with some metadata set explicitely.
            , make.directory
                Directory::{
                , name = "my private directory"
                  -- How owns the new directory: just_me
                , user = Some (User.UserName "just_me")
                  -- We stick with the user's default group here.
                , group = None Group
                , mode = Some semi_private
                , content = [] : List tree
                }
            ]
          : List tree
