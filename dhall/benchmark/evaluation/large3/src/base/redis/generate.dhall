let Optional/default =
      ../../../../../../dhall-lang/Prelude/Optional/default.dhall

let Kubernetes/TCPSocketAction =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.TCPSocketAction.dhall

let Kubernetes/PersistentVolumeClaim =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.PersistentVolumeClaim.dhall

let Kubernetes/PersistentVolumeClaimSpec =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.PersistentVolumeClaimSpec.dhall

let Kubernetes/Volume = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Volume.dhall

let Kubernetes/PersistentVolumeClaimVolumeSource =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.PersistentVolumeClaimVolumeSource.dhall

let Kubernetes/Container =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Container.dhall

let Kubernetes/ContainerPort =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ContainerPort.dhall

let Kubernetes/Deployment =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.Deployment.dhall

let Kubernetes/DeploymentSpec =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.DeploymentSpec.dhall

let Kubernetes/DeploymentStrategy =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.DeploymentStrategy.dhall

let Kubernetes/LabelSelector =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall

let Kubernetes/ObjectMeta =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall

let Kubernetes/PodSecurityContext =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.PodSecurityContext.dhall

let Kubernetes/PodSpec = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.PodSpec.dhall

let Kubernetes/PodTemplateSpec =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.PodTemplateSpec.dhall

let Kubernetes/Probe = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Probe.dhall

let Kubernetes/ResourceRequirements =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ResourceRequirements.dhall

let Kubernetes/Service = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Service.dhall

let Kubernetes/ServicePort =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ServicePort.dhall

let Kubernetes/ServiceSpec =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ServiceSpec.dhall

let Kubernetes/VolumeMount =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.VolumeMount.dhall

let Configuration/global = ../../configuration/global.dhall

let component = ./component.dhall

let containerResources = ../../configuration/container-resources.dhall

let containerResources/tok8s = ../../util/container-resources-to-k8s.dhall

let Cache/PersistentVolumeClaim/generate =
      λ(c : Configuration/global.Type) →
        let persistentVolumeClaim =
              Kubernetes/PersistentVolumeClaim::{
              , metadata = Kubernetes/ObjectMeta::{
                , labels = Some
                  [ { mapKey = "deploy", mapValue = "sourcegraph" }
                  , { mapKey = "sourcegraph-resource-requires"
                    , mapValue = "no-cluster-admin"
                    }
                  ]
                , name = Some "redis-cache"
                }
              , spec = Some Kubernetes/PersistentVolumeClaimSpec::{
                , accessModes = Some [ "ReadWriteOnce" ]
                , resources = Some Kubernetes/ResourceRequirements::{
                  , requests = Some
                    [ { mapKey = "storage", mapValue = "100Gi" } ]
                  }
                , storageClassName = Some "sourcegraph"
                }
              }

        in  persistentVolumeClaim

let Cache/Service/generate =
      λ(c : Configuration/global.Type) →
        let service =
              Kubernetes/Service::{
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = Some
                  [ { mapKey = "prometheus.io/port", mapValue = "9121" }
                  , { mapKey = "sourcegraph.prometheus/scrape"
                    , mapValue = "true"
                    }
                  ]
                , labels = Some
                  [ { mapKey = "app", mapValue = "redis-cache" }
                  , { mapKey = "deploy", mapValue = "sourcegraph" }
                  , { mapKey = "sourcegraph-resource-requires"
                    , mapValue = "no-cluster-admin"
                    }
                  ]
                , name = Some "redis-cache"
                }
              , spec = Some Kubernetes/ServiceSpec::{
                , ports = Some
                  [ Kubernetes/ServicePort::{
                    , name = Some "redis"
                    , port = 6379
                    , targetPort = Some
                        (< Nat : Natural | String : Text >.String "redis")
                    }
                  ]
                , selector = Some
                  [ { mapKey = "app", mapValue = "redis-cache" } ]
                , type = Some "ClusterIP"
                }
              }

        in  service

let cacheContainer/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Redis.Cache.Deployment.Containers.Cache

        let image =
              Optional/default
                Text
                "index.docker.io/sourcegraph/redis-cache:3.17.2@sha256:7820219195ab3e8fdae5875cd690fed1b2a01fd1063bd94210c0e9d529c38e56"
                overrides.image

        let resources =
              containerResources/tok8s
                { limits =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "1"
                      , memory = Some "6Gi"
                      }
                      overrides.resources.limits
                , requests =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "1"
                      , memory = Some "6Gi"
                      }
                      overrides.resources.requests
                }

        let container =
              Kubernetes/Container::{
              , image = Some image
              , livenessProbe = Some Kubernetes/Probe::{
                , initialDelaySeconds = Some 30
                , tcpSocket = Some Kubernetes/TCPSocketAction::{
                  , port = < Nat : Natural | String : Text >.String "redis"
                  }
                }
              , name = "redis-cache"
              , ports = Some
                [ Kubernetes/ContainerPort::{
                  , containerPort = 6379
                  , name = Some "redis"
                  }
                ]
              , readinessProbe = Some Kubernetes/Probe::{
                , initialDelaySeconds = Some 5
                , tcpSocket = Some Kubernetes/TCPSocketAction::{
                  , port = < Nat : Natural | String : Text >.String "redis"
                  }
                }
              , resources = Some resources
              , terminationMessagePolicy = Some "FallbackToLogsOnError"
              , volumeMounts = Some
                [ Kubernetes/VolumeMount::{
                  , mountPath = "/redis-data"
                  , name = "redis-data"
                  }
                ]
              }

        in  container

let cacheExporterContainer/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Redis.Cache.Deployment.Containers.Exporter

        let image =
              Optional/default
                Text
                "index.docker.io/sourcegraph/redis_exporter:18-02-07_bb60087_v0.15.0@sha256:282d59b2692cca68da128a4e28d368ced3d17945cd1d273d3ee7ba719d77b753"
                overrides.image

        let resources =
              containerResources/tok8s
                { limits =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "10m"
                      , memory = Some "100Mi"
                      }
                      overrides.resources.limits
                , requests =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "10m"
                      , memory = Some "100Mi"
                      }
                      overrides.resources.requests
                }

        let container =
              Kubernetes/Container::{
              , image = Some image
              , name = "redis-exporter"
              , ports = Some
                [ Kubernetes/ContainerPort::{
                  , containerPort = 9121
                  , name = Some "redisexp"
                  }
                ]
              , resources = Some resources
              , terminationMessagePolicy = Some "FallbackToLogsOnError"
              }

        in  container

let Cache/Deployment/generate =
      λ(c : Configuration/global.Type) →
        let cacheContainer = cacheContainer/generate c

        let exporterContainer = cacheExporterContainer/generate c

        let deployment =
              Kubernetes/Deployment::{
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = Some
                  [ { mapKey = "description"
                    , mapValue = "Redis for storing short-lived caches."
                    }
                  ]
                , labels = Some
                  [ { mapKey = "deploy", mapValue = "sourcegraph" }
                  , { mapKey = "sourcegraph-resource-requires"
                    , mapValue = "no-cluster-admin"
                    }
                  ]
                , name = Some "redis-cache"
                }
              , spec = Some Kubernetes/DeploymentSpec::{
                , minReadySeconds = Some 10
                , replicas = Some 1
                , revisionHistoryLimit = Some 10
                , selector = Kubernetes/LabelSelector::{
                  , matchLabels = Some
                    [ { mapKey = "app", mapValue = "redis-cache" } ]
                  }
                , strategy = Some Kubernetes/DeploymentStrategy::{
                  , type = Some "Recreate"
                  }
                , template = Kubernetes/PodTemplateSpec::{
                  , metadata = Some Kubernetes/ObjectMeta::{
                    , labels = Some
                      [ { mapKey = "app", mapValue = "redis-cache" }
                      , { mapKey = "deploy", mapValue = "sourcegraph" }
                      ]
                    }
                  , spec = Some Kubernetes/PodSpec::{
                    , containers = [ cacheContainer, exporterContainer ]
                    , securityContext = Some Kubernetes/PodSecurityContext::{
                      , runAsUser = Some 0
                      }
                    , volumes = Some
                      [ Kubernetes/Volume::{
                        , name = "redis-data"
                        , persistentVolumeClaim = Some Kubernetes/PersistentVolumeClaimVolumeSource::{
                          , claimName = "redis-cache"
                          }
                        }
                      ]
                    }
                  }
                }
              }

        in  deployment

let Store/PersistentVolumeClaim/generate =
      λ(c : Configuration/global.Type) →
        let persistentVolumeClaim =
              Kubernetes/PersistentVolumeClaim::{
              , metadata = Kubernetes/ObjectMeta::{
                , labels = Some
                  [ { mapKey = "deploy", mapValue = "sourcegraph" }
                  , { mapKey = "sourcegraph-resource-requires"
                    , mapValue = "no-cluster-admin"
                    }
                  ]
                , name = Some "redis-store"
                }
              , spec = Some Kubernetes/PersistentVolumeClaimSpec::{
                , accessModes = Some [ "ReadWriteOnce" ]
                , resources = Some Kubernetes/ResourceRequirements::{
                  , requests = Some
                    [ { mapKey = "storage", mapValue = "100Gi" } ]
                  }
                , storageClassName = Some "sourcegraph"
                }
              }

        in  persistentVolumeClaim

let Store/Service/generate =
      λ(c : Configuration/global.Type) →
        let service =
              Kubernetes/Service::{
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = Some
                  [ { mapKey = "prometheus.io/port", mapValue = "9121" }
                  , { mapKey = "sourcegraph.prometheus/scrape"
                    , mapValue = "true"
                    }
                  ]
                , labels = Some
                  [ { mapKey = "app", mapValue = "redis-store" }
                  , { mapKey = "deploy", mapValue = "sourcegraph" }
                  , { mapKey = "sourcegraph-resource-requires"
                    , mapValue = "no-cluster-admin"
                    }
                  ]
                , name = Some "redis-store"
                }
              , spec = Some Kubernetes/ServiceSpec::{
                , ports = Some
                  [ Kubernetes/ServicePort::{
                    , name = Some "redis"
                    , port = 6379
                    , targetPort = Some
                        (< Nat : Natural | String : Text >.String "redis")
                    }
                  ]
                , selector = Some
                  [ { mapKey = "app", mapValue = "redis-store" } ]
                , type = Some "ClusterIP"
                }
              }

        in  service

let storeContainer/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Redis.Store.Deployment.Containers.Store

        let image =
              Optional/default
                Text
                "index.docker.io/sourcegraph/redis-store:3.17.2@sha256:e8467a8279832207559bdfbc4a89b68916ecd5b44ab5cf7620c995461c005168"
                overrides.image

        let resources =
              containerResources/tok8s
                { limits =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "1"
                      , memory = Some "6Gi"
                      }
                      overrides.resources.limits
                , requests =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "1"
                      , memory = Some "6Gi"
                      }
                      overrides.resources.requests
                }

        let container =
              Kubernetes/Container::{
              , image = Some image
              , livenessProbe = Some Kubernetes/Probe::{
                , initialDelaySeconds = Some 30
                , tcpSocket = Some Kubernetes/TCPSocketAction::{
                  , port = < Nat : Natural | String : Text >.String "redis"
                  }
                }
              , name = "redis-store"
              , ports = Some
                [ Kubernetes/ContainerPort::{
                  , containerPort = 6379
                  , name = Some "redis"
                  }
                ]
              , readinessProbe = Some Kubernetes/Probe::{
                , initialDelaySeconds = Some 5
                , tcpSocket = Some Kubernetes/TCPSocketAction::{
                  , port = < Nat : Natural | String : Text >.String "redis"
                  }
                }
              , resources = Some resources
              , terminationMessagePolicy = Some "FallbackToLogsOnError"
              , volumeMounts = Some
                [ Kubernetes/VolumeMount::{
                  , mountPath = "/redis-data"
                  , name = "redis-data"
                  }
                ]
              }

        in  container

let storeExporterContainer/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Redis.Store.Deployment.Containers.Exporter

        let image =
              Optional/default
                Text
                "index.docker.io/sourcegraph/redis_exporter:18-02-07_bb60087_v0.15.0@sha256:282d59b2692cca68da128a4e28d368ced3d17945cd1d273d3ee7ba719d77b753"
                overrides.image

        let resources =
              containerResources/tok8s
                { limits =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "10m"
                      , memory = Some "100Mi"
                      }
                      overrides.resources.limits
                , requests =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "10m"
                      , memory = Some "100Mi"
                      }
                      overrides.resources.requests
                }

        let container =
              Kubernetes/Container::{
              , image = Some image
              , name = "redis-exporter"
              , ports = Some
                [ Kubernetes/ContainerPort::{
                  , containerPort = 9121
                  , name = Some "redisexp"
                  }
                ]
              , resources = Some resources
              , terminationMessagePolicy = Some "FallbackToLogsOnError"
              }

        in  container

let Store/Deployment/generate =
      λ(c : Configuration/global.Type) →
        let storeContainer = storeContainer/generate c

        let exporterContainer = storeExporterContainer/generate c

        let deployment =
              Kubernetes/Deployment::{
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = Some
                  [ { mapKey = "description"
                    , mapValue =
                        "Redis for storing semi-persistent data like user sessions."
                    }
                  ]
                , labels = Some
                  [ { mapKey = "deploy", mapValue = "sourcegraph" }
                  , { mapKey = "sourcegraph-resource-requires"
                    , mapValue = "no-cluster-admin"
                    }
                  ]
                , name = Some "redis-store"
                }
              , spec = Some Kubernetes/DeploymentSpec::{
                , minReadySeconds = Some 10
                , replicas = Some 1
                , revisionHistoryLimit = Some 10
                , selector = Kubernetes/LabelSelector::{
                  , matchLabels = Some
                    [ { mapKey = "app", mapValue = "redis-store" } ]
                  }
                , strategy = Some Kubernetes/DeploymentStrategy::{
                  , type = Some "Recreate"
                  }
                , template = Kubernetes/PodTemplateSpec::{
                  , metadata = Some Kubernetes/ObjectMeta::{
                    , labels = Some
                      [ { mapKey = "app", mapValue = "redis-store" }
                      , { mapKey = "deploy", mapValue = "sourcegraph" }
                      ]
                    }
                  , spec = Some Kubernetes/PodSpec::{
                    , containers = [ storeContainer, exporterContainer ]
                    , securityContext = Some Kubernetes/PodSecurityContext::{
                      , runAsUser = Some 0
                      }
                    , volumes = Some
                      [ Kubernetes/Volume::{
                        , name = "redis-data"
                        , persistentVolumeClaim = Some Kubernetes/PersistentVolumeClaimVolumeSource::{
                          , claimName = "redis-store"
                          }
                        }
                      ]
                    }
                  }
                }
              }

        in  deployment

let Generate =
        ( λ(c : Configuration/global.Type) →
            { Cache =
              { Deployment = Cache/Deployment/generate c
              , PersistentVolumeClaim = Cache/PersistentVolumeClaim/generate c
              , Service = Cache/Service/generate c
              }
            , Store =
              { Deployment = Store/Deployment/generate c
              , PersistentVolumeClaim = Store/PersistentVolumeClaim/generate c
              , Service = Store/Service/generate c
              }
            }
        )
      : ∀(c : Configuration/global.Type) → component

in  Generate
