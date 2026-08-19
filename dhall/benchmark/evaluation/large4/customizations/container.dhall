let container/resources = ../combinators/container-resources.dhall

let vsType = { version : Text, sha256 : Text }

let container =
      { Type =
          { resources : container/resources.Type
          , vs : Optional vsType
          , additionalEnv : Optional (List { name : Text, value : Text })
          }
      , default =
        { resources = container/resources.default
        , vs = None vsType
        , additionalEnv = None (List { name : Text, value : Text })
        }
      }

in  container
