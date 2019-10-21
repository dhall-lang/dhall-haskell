{- Need to generate a lot of users?

   Use the `generate` function from the Dhall Prelude
-}

let generate = https://prelude.dhall-lang.org/List/generate

{- You can import Dhall expressions from URLs that support
   CORS

   The command-line tools also let you import from files,
   environment variables, and URLs without CORS support.

   Browse https://prelude.dhall-lang.org for more utilities
-}

let makeUser = \(user : Text) ->
      let home       = "/home/${user}"
      let privateKey = "${home}/.ssh/id_ed25519"
      let publicKey  = "${privateKey}.pub"
      in  { home = home
          , privateKey = privateKey
          , publicKey = publicKey
          }

let buildUser = \(index : Natural) ->
      {- `Natural/show` is a "built-in", meaning that
         you can use `Natural/show` without an import
      -}
      makeUser "build${Natural/show index}"

let Config =
      { home : Text
      , privateKey : Text
      , publicKey : Text
      }

in  {- Try generating 20 users instead of 10 -}
    generate 10 Config buildUser
