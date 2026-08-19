let Configuration/cloud-provider = ../../configuration/cloud-provider.dhall

let configuration =
      { Type = { CloudProvider : Configuration/cloud-provider }
      , default.CloudProvider = Configuration/cloud-provider.CUSTOM
      }

in  configuration
