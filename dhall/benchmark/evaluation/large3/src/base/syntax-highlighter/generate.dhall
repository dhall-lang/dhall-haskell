let Optional/default =
      ../../../../../../dhall-lang/Prelude/Optional/default.dhall

let Kubernetes/HTTPGetAction =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.HTTPGetAction.dhall

let Kubernetes/TCPSocketAction =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.TCPSocketAction.dhall

let Kubernetes/Probe = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Probe.dhall

let Kubernetes/RollingUpdateDeployment =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.RollingUpdateDeployment.dhall

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

let Kubernetes/Service = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Service.dhall

let Kubernetes/ServicePort =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ServicePort.dhall

let Kubernetes/ServiceSpec =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ServiceSpec.dhall

let Configuration/global = ../../configuration/global.dhall

let component = ./component.dhall

let containerResources = ../../configuration/container-resources.dhall

let containerResources/tok8s = ../../util/container-resources-to-k8s.dhall

let Service/generate =
      λ(c : Configuration/global.Type) →
        let service =
              Kubernetes/Service::{
              , metadata = Kubernetes/ObjectMeta::{
                , labels = Some
                  [ { mapKey = "app", mapValue = "syntect-server" }
                  , { mapKey = "deploy", mapValue = "sourcegraph" }
                  , { mapKey = "sourcegraph-resource-requires"
                    , mapValue = "no-cluster-admin"
                    }
                  ]
                , name = Some "syntect-server"
                }
              , spec = Some Kubernetes/ServiceSpec::{
                , ports = Some
                  [ Kubernetes/ServicePort::{
                    , name = Some "http"
                    , port = 9238
                    , targetPort = Some
                        (< Nat : Natural | String : Text >.String "http")
                    }
                  ]
                , selector = Some
                  [ { mapKey = "app", mapValue = "syntect-server" } ]
                , type = Some "ClusterIP"
                }
              }

        in  service

let Deployment/generate =
      λ(c : Configuration/global.Type) →
        let overrides =
              c.SyntaxHighlighter.Deployment.Containers.SyntaxHighlighter

        let image =
              Optional/default
                Text
                "index.docker.io/sourcegraph/syntax-highlighter:3.17.2@sha256:aa93514b7bc3aaf7a4e9c92e5ff52ee5052db6fb101255a69f054e5b8cdb46ff"
                overrides.image

        let resources =
              containerResources/tok8s
                { limits =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "4"
                      , memory = Some "6G"
                      }
                      overrides.resources.limits
                , requests =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "250m"
                      , memory = Some "2G"
                      }
                      overrides.resources.requests
                }

        let deployment =
              Kubernetes/Deployment::{
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = Some
                  [ { mapKey = "description"
                    , mapValue = "Backend for syntax highlighting operations."
                    }
                  ]
                , labels = Some
                  [ { mapKey = "deploy", mapValue = "sourcegraph" }
                  , { mapKey = "sourcegraph-resource-requires"
                    , mapValue = "no-cluster-admin"
                    }
                  ]
                , name = Some "syntect-server"
                }
              , spec = Some Kubernetes/DeploymentSpec::{
                , minReadySeconds = Some 10
                , replicas = Some 1
                , revisionHistoryLimit = Some 10
                , selector = Kubernetes/LabelSelector::{
                  , matchLabels = Some
                    [ { mapKey = "app", mapValue = "syntect-server" } ]
                  }
                , strategy = Some Kubernetes/DeploymentStrategy::{
                  , rollingUpdate = Some Kubernetes/RollingUpdateDeployment::{
                    , maxSurge = Some (< Nat : Natural | String : Text >.Nat 1)
                    , maxUnavailable = Some
                        (< Nat : Natural | String : Text >.Nat 0)
                    }
                  , type = Some "RollingUpdate"
                  }
                , template = Kubernetes/PodTemplateSpec::{
                  , metadata = Some Kubernetes/ObjectMeta::{
                    , labels = Some
                      [ { mapKey = "app", mapValue = "syntect-server" }
                      , { mapKey = "deploy", mapValue = "sourcegraph" }
                      ]
                    }
                  , spec = Some Kubernetes/PodSpec::{
                    , containers =
                      [ Kubernetes/Container::{
                        , image = Some image
                        , livenessProbe = Some Kubernetes/Probe::{
                          , httpGet = Some Kubernetes/HTTPGetAction::{
                            , path = Some "/health"
                            , port =
                                < Nat : Natural | String : Text >.String "http"
                            , scheme = Some "HTTP"
                            }
                          , initialDelaySeconds = Some 5
                          , timeoutSeconds = Some 5
                          }
                        , name = "syntect-server"
                        , ports = Some
                          [ Kubernetes/ContainerPort::{
                            , containerPort = 9238
                            , name = Some "http"
                            }
                          ]
                        , readinessProbe = Some Kubernetes/Probe::{
                          , tcpSocket = Some Kubernetes/TCPSocketAction::{
                            , port =
                                < Nat : Natural | String : Text >.String "http"
                            }
                          }
                        , resources = Some resources
                        , terminationMessagePolicy = Some
                            "FallbackToLogsOnError"
                        }
                      ]
                    , securityContext = Some Kubernetes/PodSecurityContext::{
                      , runAsUser = Some 0
                      }
                    }
                  }
                }
              }

        in  deployment

let Generate =
        ( λ(c : Configuration/global.Type) →
            { Deployment = Deployment/generate c, Service = Service/generate c }
        )
      : ∀(c : Configuration/global.Type) → component

in  Generate
