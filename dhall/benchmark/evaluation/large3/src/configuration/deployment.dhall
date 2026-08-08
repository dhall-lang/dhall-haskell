let Configuration/universal = ./universal.dhall

let configuration =
      { Type = { replicas : Optional Natural } ⩓ Configuration/universal.Type
      , default = { replicas = None Natural } ∧ Configuration/universal.default
      }

in  configuration
