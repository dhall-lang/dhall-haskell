let Configuration =
      { Type =
          { memory : Optional Text
          , cpu : Optional Text
          , ephemeralStorage : Optional Text
          }
      , default =
        { memory = None Text, cpu = None Text, ephemeralStorage = None Text }
      }

let ResourcesType =
      { requests : Configuration.Type, limits : Configuration.Type }

let mergeResources
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

let overlayMerge
    : ResourcesType → ResourcesType → ResourcesType
    = λ(base : ResourcesType) →
      λ(overlay : ResourcesType) →
        let finalLimits = mergeResources base.limits overlay.limits

        let finalRequests = mergeResources base.requests overlay.requests

        in  { requests = finalRequests, limits = finalLimits }

let tests =
      { t1 =
            assert
          :   mergeResources
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
          :   mergeResources
                Configuration::{ cpu = Some "500m", memory = Some "200MB" }
                Configuration::{ cpu = Some "3" }
            ≡ Configuration::{ cpu = Some "3", memory = Some "200MB" }
      }

let resources =
      { Type = ResourcesType
      , default =
        { limits = Configuration.default, requests = Configuration.default }
      , Configuration
      , mergeResources
      , overlayMerge
      }

in  resources
