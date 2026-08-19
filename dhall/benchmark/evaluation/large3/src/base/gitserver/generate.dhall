let Optional/default =
      ../../../../../../dhall-lang/Prelude/Optional/default.dhall

let Kubernetes/Container =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Container.dhall

let Kubernetes/ContainerPort =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ContainerPort.dhall

let Kubernetes/NatOrString =
      ../../../../k8s/dhall-kubernetes/1.17/types/io.k8s.apimachinery.pkg.util.intstr.NatOrString.dhall

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

let Kubernetes/TCPSocketAction =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.TCPSocketAction.dhall

let Kubernetes/Volume = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Volume.dhall

let Kubernetes/SecretVolumeSource =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.SecretVolumeSource.dhall

let Kubernetes/VolumeMount =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.VolumeMount.dhall

let Configuration/global = ../../configuration/global.dhall

let Util/JaegerAgent = ../../util/jaeger-agent.dhall

let Util/KeyValuePair = ../../util/key-value-pair.dhall

let component = ./component.dhall

let containerResources = ../../configuration/container-resources.dhall

let containerResources/tok8s = ../../util/container-resources-to-k8s.dhall

let Octal = ../../util/octal.dhall

let Service/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Gitserver.Service

        let additionalAnnotations =
              Optional/default
                (List Util/KeyValuePair)
                ([] : List Util/KeyValuePair)
                overrides.additionalAnnotations

        let additionalLabels =
              Optional/default
                (List Util/KeyValuePair)
                ([] : List Util/KeyValuePair)
                overrides.additionalLabels

        let service =
              Kubernetes/Service::{
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = Some
                    (   toMap
                          { `sourcegraph.prometheus/scrape` = "true"
                          , `prometheus.io/port` = "6060"
                          , description =
                              "Headless service that provides a stable network identity for the gitserver stateful set."
                          }
                      # additionalAnnotations
                    )
                , labels = Some
                    (   toMap
                          { sourcegraph-resource-requires = "no-cluster-admin"
                          , app = "gitserver"
                          , type = "gitserver"
                          , deploy = "sourcegraph"
                          }
                      # additionalLabels
                    )
                , namespace = overrides.namespace
                , name = Some "gitserver"
                }
              , spec = Some Kubernetes/ServiceSpec::{
                , clusterIP = Some "None"
                , ports = Some
                  [ Kubernetes/ServicePort::{
                    , name = Some "unused"
                    , port = 10811
                    , targetPort = Some (Kubernetes/NatOrString.Nat 10811)
                    }
                  ]
                , selector = Some
                  [ { mapKey = "app", mapValue = "gitserver" }
                  , { mapKey = "type", mapValue = "gitserver" }
                  ]
                , type = Some "ClusterIP"
                }
              }

        in  service

let gitserverContainer/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Gitserver.StatefulSet.Containers.Gitserver

        let environment = overrides.additionalEnvironmentVariables

        let image =
              Optional/default
                Text
                "index.docker.io/sourcegraph/gitserver:3.17.2@sha256:a2dac3ed8c9bbd7f930ae8d2bb446878f43f326002774fd85647945a90535733"
                overrides.image

        let resources =
              containerResources/tok8s
                { limits =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "4"
                      , memory = Some "8G"
                      }
                      overrides.resources.limits
                , requests =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "4"
                      , memory = Some "8G"
                      }
                      overrides.resources.requests
                }

        let container =
              Kubernetes/Container::{
              , args = Some [ "run" ]
              , image = Some image
              , env = environment
              , livenessProbe = Some Kubernetes/Probe::{
                , initialDelaySeconds = Some 5
                , tcpSocket = Some Kubernetes/TCPSocketAction::{
                  , port = Kubernetes/NatOrString.String "rpc"
                  }
                , timeoutSeconds = Some 5
                }
              , name = "gitserver"
              , ports = Some
                [ Kubernetes/ContainerPort::{
                  , containerPort = 3178
                  , name = Some "rpc"
                  }
                ]
              , resources = Some resources
              , terminationMessagePolicy = Some "FallbackToLogsOnError"
              , volumeMounts = Some
                [ Kubernetes/VolumeMount::{
                  , mountPath = "/data/repos"
                  , name = "repos"
                  }
                ]
              }

        in  container

let StatefulSet/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Gitserver.StatefulSet

        let additionalAnnotations =
              Optional/default
                (List Util/KeyValuePair)
                ([] : List Util/KeyValuePair)
                overrides.additionalAnnotations

        let additionalLabels =
              Optional/default
                (List Util/KeyValuePair)
                ([] : List Util/KeyValuePair)
                overrides.additionalLabels

        let replicas = Optional/default Natural 1 overrides.replicas

        let pvcSize =
              Optional/default Text "200Gi" overrides.persistentVolumeSize

        let gitserverContainer = gitserverContainer/generate c

        let sshVolume =
              merge
                { Some =
                    λ(x : Text) →
                      [ Kubernetes/Volume::{
                        , name = "ssh"
                        , secret = Some Kubernetes/SecretVolumeSource::{
                          , secretName = Some x
                          , defaultMode = Some
                              (Octal.toNatural Octal.Enum.Oo600)
                          }
                        }
                      ]
                , None = [] : List Kubernetes/Volume.Type
                }
                overrides.sshSecretName

        let statefulSet =
              Kubernetes/StatefulSet::{
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = Some
                    (   [ { mapKey = "description"
                          , mapValue =
                              "Stores clones of repositories to perform Git operations."
                          }
                        ]
                      # additionalAnnotations
                    )
                , labels = Some
                    (   [ { mapKey = "deploy", mapValue = "sourcegraph" }
                        , { mapKey = "sourcegraph-resource-requires"
                          , mapValue = "no-cluster-admin"
                          }
                        ]
                      # additionalLabels
                    )
                , namespace = overrides.namespace
                , name = Some "gitserver"
                }
              , spec = Some Kubernetes/StatefulSetSpec::{
                , replicas = Some replicas
                , revisionHistoryLimit = Some 10
                , selector = Kubernetes/LabelSelector::{
                  , matchLabels = Some
                    [ { mapKey = "app", mapValue = "gitserver" } ]
                  }
                , serviceName = "gitserver"
                , template = Kubernetes/PodTemplateSpec::{
                  , metadata = Some Kubernetes/ObjectMeta::{
                    , labels = Some
                      [ { mapKey = "app", mapValue = "gitserver" }
                      , { mapKey = "deploy", mapValue = "sourcegraph" }
                      , { mapKey = "group", mapValue = "backend" }
                      , { mapKey = "type", mapValue = "gitserver" }
                      ]
                    }
                  , spec = Some Kubernetes/PodSpec::{
                    , containers = [ gitserverContainer, Util/JaegerAgent ]
                    , schedulerName = None Text
                    , securityContext = Some Kubernetes/PodSecurityContext::{
                      , runAsUser = Some 0
                      }
                    , volumes = Some
                        ([ Kubernetes/Volume::{ name = "repos" } ] # sshVolume)
                    }
                  }
                , updateStrategy = Some
                  { rollingUpdate = None { partition : Optional Natural }
                  , type = Some "RollingUpdate"
                  }
                , volumeClaimTemplates = Some
                  [ Kubernetes/PersistentVolumeClaim::{
                    , metadata = Kubernetes/ObjectMeta::{ name = Some "repos" }
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
            }
        )
      : ∀(c : Configuration/global.Type) → component

in  Generate
