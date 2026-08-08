let List/null = ../../../../../dhall-lang/Prelude/List/null.dhall

let Util/KeyValuePair = ./key-value-pair.dhall

let Kubernetes/ResourceRequirements =
      ../../../k8s/dhall-kubernetes/1.17/types/io.k8s.api.core.v1.ResourceRequirements.dhall

let Resources = ../configuration/container-resources.dhall

let Configuration = Resources.Configuration

let configurationToK8s
    : Configuration.Type → Optional (List { mapKey : Text, mapValue : Text })
    = λ(c : Configuration.Type) →
        let memoryItem =
              merge
                { Some = λ(x : Text) → [ { mapKey = "memory", mapValue = x } ]
                , None = [] : List Util/KeyValuePair
                }
                c.memory

        let cpuItem =
              merge
                { Some = λ(x : Text) → [ { mapKey = "cpu", mapValue = x } ]
                , None = [] : List Util/KeyValuePair
                }
                c.cpu

        let ephemeralStorage =
              merge
                { Some =
                    λ(x : Text) →
                      [ { mapKey = "ephemeral-storage", mapValue = x } ]
                , None = [] : List Util/KeyValuePair
                }
                c.ephemeralStorage

        let all = memoryItem # cpuItem # ephemeralStorage

        in  if    List/null Util/KeyValuePair all
            then  None (List Util/KeyValuePair)
            else  Some all

let tok8s
    : Resources.Type → Kubernetes/ResourceRequirements
    = λ(r : Resources.Type) →
        let result =
              { limits = configurationToK8s r.limits
              , requests = configurationToK8s r.requests
              }

        in  result

let tests =
      { t1 =
            assert
          :   tok8s
                { limits = Configuration::{
                  , memory = Some "20Gi"
                  , cpu = Some "2"
                  }
                , requests = Configuration::{
                  , ephemeralStorage = Some "100MB"
                  , cpu = Some "500m"
                  }
                }
            ≡ { limits = Some
                [ { mapKey = "memory", mapValue = "20Gi" }
                , { mapKey = "cpu", mapValue = "2" }
                ]
              , requests = Some
                [ { mapKey = "cpu", mapValue = "500m" }
                , { mapKey = "ephemeral-storage", mapValue = "100MB" }
                ]
              }
      , t2 =
            assert
          :   tok8s
                { limits = Configuration::{ ephemeralStorage = Some "200MB" }
                , requests = Configuration::{
                  , memory = Some "100MB"
                  , cpu = Some "500m"
                  }
                }
            ≡ { limits = Some
                [ { mapKey = "ephemeral-storage", mapValue = "200MB" } ]
              , requests = Some
                [ { mapKey = "memory", mapValue = "100MB" }
                , { mapKey = "cpu", mapValue = "500m" }
                ]
              }
      }

in  tok8s
