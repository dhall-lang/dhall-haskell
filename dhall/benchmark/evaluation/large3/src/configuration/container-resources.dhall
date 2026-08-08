let Configuration =
      { Type =
          { memory : Optional Text
          , cpu : Optional Text
          , ephemeralStorage : Optional Text
          }
      , default =
        { memory = None Text, cpu = None Text, ephemeralStorage = None Text }
      }

let overlayConfig
    : Configuration.Type → Configuration.Type → Configuration.Type
    = λ(base : Configuration.Type) →
      λ(overlay : Configuration.Type) →
        let overlayedMemory =
              merge
                { Some = λ(x : Text) → Some x, None = base.memory }
                overlay.memory

        let overlayedCPU =
              merge { Some = λ(x : Text) → Some x, None = base.cpu } overlay.cpu

        let overlayedEphemeralStorage =
              merge
                { Some = λ(x : Text) → Some x, None = base.ephemeralStorage }
                overlay.ephemeralStorage

        let result =
              { memory = overlayedMemory
              , cpu = overlayedCPU
              , ephemeralStorage = overlayedEphemeralStorage
              }

        in  result

let tests =
      { t1 =
            assert
          :   overlayConfig
                Configuration::{ memory = Some "20Gi", cpu = Some "2" }
                Configuration::{
                , ephemeralStorage = Some "100MB"
                , cpu = Some "500m"
                }
            ≡ Configuration::{
              , ephemeralStorage = Some "100MB"
              , cpu = Some "500m"
              , memory = Some "20Gi"
              }
      , t2 =
            assert
          :   overlayConfig
                Configuration::{ cpu = Some "500m", memory = Some "200MB" }
                Configuration::{ cpu = Some "3" }
            ≡ Configuration::{ cpu = Some "3", memory = Some "200MB" }
      }

let resources =
      { Type = { limits : Configuration.Type, requests : Configuration.Type }
      , default =
        { limits = Configuration.default, requests = Configuration.default }
      , Configuration
      , overlay = overlayConfig
      }

in  resources
