let Configuration/universal = ../../configuration/universal.dhall

let Configuration/deployment = ../../configuration/deployment.dhall

let Configuration/container = ../../configuration/container.dhall

let BundleManager/containers =
      { Type = { BundleManager : Configuration/container.Type }
      , default.BundleManager = Configuration/container.default
      }

let BundleManager/deployment =
      { Type =
            { Containers : BundleManager/containers.Type }
          ⩓ Configuration/deployment.Type
      , default =
            { Containers = BundleManager/containers.default }
          ∧ Configuration/deployment.default
      }

let BundleManager/configuration =
      { Type =
          { Deployment : BundleManager/deployment.Type
          , PersistentVolumeClaim : Configuration/universal.Type
          , Service : Configuration/universal.Type
          }
      , default =
        { Deployment = BundleManager/deployment.default
        , PersistentVolumeClaim = Configuration/universal.default
        , Service = Configuration/universal.default
        }
      }

let Worker/containers =
      { Type = { Worker : Configuration/container.Type }
      , default.Worker = Configuration/container.default
      }

let Worker/deployment =
      { Type =
            { Containers : Worker/containers.Type }
          ⩓ Configuration/deployment.Type
      , default =
            { Containers = Worker/containers.default }
          ∧ Configuration/deployment.default
      }

let Worker/configuration =
      { Type =
          { Deployment : Worker/deployment.Type
          , Service : Configuration/universal.Type
          }
      , default =
        { Deployment = Worker/deployment.default
        , Service = Configuration/universal.default
        }
      }

let configuration =
      { Type =
          { BundleManager : BundleManager/configuration.Type
          , Worker : Worker/configuration.Type
          }
      , default =
        { BundleManager = BundleManager/configuration.default
        , Worker = Worker/configuration.default
        }
      }

in  configuration
