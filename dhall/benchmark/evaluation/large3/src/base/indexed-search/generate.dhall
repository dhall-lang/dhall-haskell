let Optional/default =
      ../../../../../../dhall-lang/Prelude/Optional/default.dhall

let Kubernetes/HTTPGetAction =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.HTTPGetAction.dhall

let Kubernetes/Container =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Container.dhall

let Kubernetes/ContainerPort =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ContainerPort.dhall

let Kubernetes/LabelSelector =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall

let Kubernetes/ObjectMeta =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall

let Kubernetes/PersistentVolumeClaim =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.PersistentVolumeClaim.dhall

let Kubernetes/PersistentVolumeClaimSpec =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.PersistentVolumeClaimSpec.dhall

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

let Kubernetes/StatefulSet =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.StatefulSet.dhall

let Kubernetes/StatefulSetSpec =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.StatefulSetSpec.dhall

let Kubernetes/StatefulSetUpdateStrategy =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.StatefulSetUpdateStrategy.dhall

let Kubernetes/Volume = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Volume.dhall

let Kubernetes/VolumeMount =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.VolumeMount.dhall

let Configuration/global = ../../configuration/global.dhall

let component = ./component.dhall

let containerResources = ../../configuration/container-resources.dhall

let containerResources/tok8s = ../../util/container-resources-to-k8s.dhall

let IndexerService/generate =
      λ(c : Configuration/global.Type) →
        let service =
              Kubernetes/Service::{
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = Some
                  [ { mapKey = "description"
                    , mapValue =
                        "Headless service that provides a stable network identity for the indexed-search stateful set."
                    }
                  , { mapKey = "prometheus.io/port", mapValue = "6072" }
                  , { mapKey = "sourcegraph.prometheus/scrape"
                    , mapValue = "true"
                    }
                  ]
                , labels = Some
                  [ { mapKey = "app", mapValue = "indexed-search-indexer" }
                  , { mapKey = "deploy", mapValue = "sourcegraph" }
                  , { mapKey = "sourcegraph-resource-requires"
                    , mapValue = "no-cluster-admin"
                    }
                  ]
                , name = Some "indexed-search-indexer"
                }
              , spec = Some Kubernetes/ServiceSpec::{
                , clusterIP = Some "None"
                , ports = Some
                  [ Kubernetes/ServicePort::{
                    , port = 6072
                    , targetPort = Some
                        (< Nat : Natural | String : Text >.Nat 6072)
                    }
                  ]
                , selector = Some
                  [ { mapKey = "app", mapValue = "indexed-search" } ]
                , type = Some "ClusterIP"
                }
              }

        in  service

let Service/generate =
      λ(c : Configuration/global.Type) →
        let service =
              Kubernetes/Service::{
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = Some
                  [ { mapKey = "description"
                    , mapValue =
                        "Headless service that provides a stable network identity for the indexed-search stateful set."
                    }
                  , { mapKey = "prometheus.io/port", mapValue = "6070" }
                  , { mapKey = "sourcegraph.prometheus/scrape"
                    , mapValue = "true"
                    }
                  ]
                , labels = Some
                  [ { mapKey = "app", mapValue = "indexed-search" }
                  , { mapKey = "deploy", mapValue = "sourcegraph" }
                  , { mapKey = "sourcegraph-resource-requires"
                    , mapValue = "no-cluster-admin"
                    }
                  ]
                , name = Some "indexed-search"
                }
              , spec = Some Kubernetes/ServiceSpec::{
                , clusterIP = Some "None"
                , ports = Some [ Kubernetes/ServicePort::{ port = 6070 } ]
                , selector = Some
                  [ { mapKey = "app", mapValue = "indexed-search" } ]
                , type = Some "ClusterIP"
                }
              }

        in  service

let zoektWebServerContainer/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.IndexedSearch.StatefulSet.Containers.ZoektWebServer

        let environment = overrides.additionalEnvironmentVariables

        let image =
              Optional/default
                Text
                "index.docker.io/sourcegraph/indexed-searcher:3.17.2@sha256:8324943e1b52466dc2052cf82bfd22b18ad045346d2b0ea403b4674f48214602"
                overrides.image

        let resources =
              containerResources/tok8s
                { limits =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "2"
                      , memory = Some "4G"
                      }
                      overrides.resources.limits
                , requests =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "500m"
                      , memory = Some "2G"
                      }
                      overrides.resources.requests
                }

        let container =
              Kubernetes/Container::{
              , image = Some image
              , env = environment
              , name = "zoekt-webserver"
              , ports = Some
                [ Kubernetes/ContainerPort::{
                  , containerPort = 6070
                  , name = Some "http"
                  }
                ]
              , readinessProbe = Some Kubernetes/Probe::{
                , failureThreshold = Some 1
                , httpGet = Some Kubernetes/HTTPGetAction::{
                  , path = Some "/healthz"
                  , port = < Nat : Natural | String : Text >.String "http"
                  , scheme = Some "HTTP"
                  }
                , periodSeconds = Some 1
                }
              , resources = Some resources
              , terminationMessagePolicy = Some "FallbackToLogsOnError"
              , volumeMounts = Some
                [ Kubernetes/VolumeMount::{ mountPath = "/data", name = "data" }
                ]
              }

        in  container

let zoektIndexServerContainer/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.IndexedSearch.StatefulSet.Containers.ZoektIndexServer

        let environment = overrides.additionalEnvironmentVariables

        let image =
              Optional/default
                Text
                "index.docker.io/sourcegraph/search-indexer:3.17.2@sha256:f31ec682b907bde2975acda88ee99ac268ef32a79309a6036ef5f26e8af0dcac"
                overrides.image

        let resources =
              containerResources/tok8s
                { limits =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "8"
                      , memory = Some "8G"
                      }
                      overrides.resources.limits
                , requests =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "4"
                      , memory = Some "4G"
                      }
                      overrides.resources.requests
                }

        let container =
              Kubernetes/Container::{
              , image = Some image
              , env = environment
              , name = "zoekt-indexserver"
              , ports = Some
                [ Kubernetes/ContainerPort::{
                  , containerPort = 6072
                  , name = Some "index-http"
                  }
                ]
              , resources = Some resources
              , terminationMessagePolicy = Some "FallbackToLogsOnError"
              , volumeMounts = Some
                [ Kubernetes/VolumeMount::{ mountPath = "/data", name = "data" }
                ]
              }

        in  container

let StatefulSet/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.IndexedSearch.StatefulSet

        let replicas = Optional/default Natural 1 overrides.replicas

        let pvcSize =
              Optional/default Text "200Gi" overrides.persistentVolumeSize

        let zoektWebServerContainer = zoektWebServerContainer/generate c

        let zoektIndexServerContainer = zoektIndexServerContainer/generate c

        let statefulSet =
              Kubernetes/StatefulSet::{
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = Some
                  [ { mapKey = "description"
                    , mapValue = "Backend for indexed text search operations."
                    }
                  ]
                , labels = Some
                  [ { mapKey = "deploy", mapValue = "sourcegraph" }
                  , { mapKey = "sourcegraph-resource-requires"
                    , mapValue = "no-cluster-admin"
                    }
                  ]
                , name = Some "indexed-search"
                }
              , spec = Some Kubernetes/StatefulSetSpec::{
                , replicas = Some replicas
                , revisionHistoryLimit = Some 10
                , selector = Kubernetes/LabelSelector::{
                  , matchLabels = Some
                    [ { mapKey = "app", mapValue = "indexed-search" } ]
                  }
                , serviceName = "indexed-search"
                , template = Kubernetes/PodTemplateSpec::{
                  , metadata = Some Kubernetes/ObjectMeta::{
                    , labels = Some
                      [ { mapKey = "app", mapValue = "indexed-search" }
                      , { mapKey = "deploy", mapValue = "sourcegraph" }
                      ]
                    }
                  , spec = Some Kubernetes/PodSpec::{
                    , containers =
                      [ zoektWebServerContainer, zoektIndexServerContainer ]
                    , securityContext = Some Kubernetes/PodSecurityContext::{
                      , runAsUser = Some 0
                      }
                    , volumes = Some [ Kubernetes/Volume::{ name = "data" } ]
                    }
                  }
                , updateStrategy = Some Kubernetes/StatefulSetUpdateStrategy::{
                  , type = Some "RollingUpdate"
                  }
                , volumeClaimTemplates = Some
                  [ Kubernetes/PersistentVolumeClaim::{
                    , metadata = Kubernetes/ObjectMeta::{
                      , labels = Some
                        [ { mapKey = "deploy", mapValue = "sourcegraph" } ]
                      , name = Some "data"
                      }
                    , spec = Some Kubernetes/PersistentVolumeClaimSpec::{
                      , accessModes = Some [ "ReadWriteOnce" ]
                      , resources = Some Kubernetes/ResourceRequirements::{
                        , requests = Some
                          [ { mapKey = "storage", mapValue = pvcSize } ]
                        }
                      , storageClassName = Some "sourcegraph"
                      }
                    }
                  ]
                }
              }

        in  statefulSet

let Generate =
        ( λ(c : Configuration/global.Type) →
            { StatefulSet = StatefulSet/generate c
            , Service = Service/generate c
            , IndexerService = IndexerService/generate c
            }
        )
      : ∀(c : Configuration/global.Type) → component

in  Generate
