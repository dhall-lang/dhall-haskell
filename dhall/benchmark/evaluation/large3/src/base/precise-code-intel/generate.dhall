let Optional/default =
      ../../../../../../dhall-lang/Prelude/Optional/default.dhall

let Kubernetes/Deployment =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.Deployment.dhall

let Kubernetes/DeploymentSpec =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.DeploymentSpec.dhall

let Kubernetes/DeploymentStrategy =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.DeploymentStrategy.dhall

let Kubernetes/EnvVar = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.EnvVar.dhall

let Kubernetes/EnvVarSource =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.EnvVarSource.dhall

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

let Kubernetes/Volume = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Volume.dhall

let Kubernetes/VolumeMount =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.VolumeMount.dhall

let Kubernetes/ObjectFieldSelector =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ObjectFieldSelector.dhall

let Kubernetes/PersistentVolumeClaimVolumeSource =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.PersistentVolumeClaimVolumeSource.dhall

let Kubernetes/RollingUpdateDeployment =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.RollingUpdateDeployment.dhall

let Configuration/global = ../../configuration/global.dhall

let containerResources = ../../configuration/container-resources.dhall

let containerResources/tok8s = ../../util/container-resources-to-k8s.dhall

let component = ./component.dhall

let Util/KeyValuePair = ../../util/key-value-pair.dhall

let BundleManager/PersistentVolumeClaim/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.PreciseCodeIntel.BundleManager.PersistentVolumeClaim

        let additionalLabels =
              Optional/default
                (List Util/KeyValuePair)
                ([] : List Util/KeyValuePair)
                overrides.additionalLabels

        let persistentVolumeClaim =
              Kubernetes/PersistentVolumeClaim::{
              , metadata = Kubernetes/ObjectMeta::{
                , labels = Some
                    (   [ { mapKey = "deploy", mapValue = "sourcegraph" }
                        , { mapKey = "sourcegraph-resource-requires"
                          , mapValue = "no-cluster-admin"
                          }
                        ]
                      # additionalLabels
                    )
                , annotations = overrides.additionalAnnotations
                , name = Some "bundle-manager"
                }
              , spec = Some Kubernetes/PersistentVolumeClaimSpec::{
                , accessModes = Some [ "ReadWriteOnce" ]
                , resources = Some Kubernetes/ResourceRequirements::{
                  , requests = Some
                    [ { mapKey = "storage", mapValue = "200Gi" } ]
                  }
                , storageClassName = Some "sourcegraph"
                }
              }

        in  persistentVolumeClaim

let BundleManager/Service/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.PreciseCodeIntel.BundleManager.Service

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
                    (   [ { mapKey = "prometheus.io/port", mapValue = "6060" }
                        , { mapKey = "sourcegraph.prometheus/scrape"
                          , mapValue = "true"
                          }
                        ]
                      # additionalAnnotations
                    )
                , labels = Some
                    (   [ { mapKey = "app"
                          , mapValue = "precise-code-intel-bundle-manager"
                          }
                        , { mapKey = "deploy", mapValue = "sourcegraph" }
                        , { mapKey = "sourcegraph-resource-requires"
                          , mapValue = "no-cluster-admin"
                          }
                        ]
                      # additionalLabels
                    )
                , name = Some "precise-code-intel-bundle-manager"
                , namespace = overrides.namespace
                }
              , spec = Some Kubernetes/ServiceSpec::{
                , ports = Some
                  [ Kubernetes/ServicePort::{
                    , name = Some "http"
                    , port = 3187
                    , targetPort = Some
                        (< Nat : Natural | String : Text >.String "http")
                    }
                  , Kubernetes/ServicePort::{
                    , name = Some "debug"
                    , port = 6060
                    , targetPort = Some
                        (< Nat : Natural | String : Text >.String "debug")
                    }
                  ]
                , selector = Some
                  [ { mapKey = "app"
                    , mapValue = "precise-code-intel-bundle-manager"
                    }
                  ]
                , type = Some "ClusterIP"
                }
              }

        in  service

let BundleManager/Deployment/Containers/BundleManager/generate =
      λ(c : Configuration/global.Type) →
        let overrides =
              c.PreciseCodeIntel.BundleManager.Deployment.Containers.BundleManager

        let image =
              Optional/default
                Text
                "index.docker.io/sourcegraph/precise-code-intel-bundle-manager:3.17.2@sha256:7dff0e7e8c7a3451ce12cf5eb5e4073bb9502752926acf33f13eb370dc570cc8"
                overrides.image

        let additionalEnvironmentVariables =
              Optional/default
                (List Kubernetes/EnvVar.Type)
                ([] : List Kubernetes/EnvVar.Type)
                overrides.additionalEnvironmentVariables

        let resources =
              containerResources/tok8s
                { limits =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "2"
                      , memory = Some "2G"
                      }
                      overrides.resources.limits
                , requests =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "500m"
                      , memory = Some "500M"
                      }
                      overrides.resources.requests
                }

        in  Kubernetes/Container::{
            , env = Some
                (   [ Kubernetes/EnvVar::{
                      , name = "PRECISE_CODE_INTEL_BUNDLE_DIR"
                      , value = Some "/lsif-storage"
                      }
                    , Kubernetes/EnvVar::{
                      , name = "POD_NAME"
                      , valueFrom = Some Kubernetes/EnvVarSource::{
                        , fieldRef = Some Kubernetes/ObjectFieldSelector::{
                          , fieldPath = "metadata.name"
                          }
                        }
                      }
                    ]
                  # additionalEnvironmentVariables
                )
            , image = Some image
            , livenessProbe = Some Kubernetes/Probe::{
              , httpGet = Some Kubernetes/HTTPGetAction::{
                , path = Some "/healthz"
                , port = < Nat : Natural | String : Text >.String "http"
                , scheme = Some "HTTP"
                }
              , initialDelaySeconds = Some 60
              , timeoutSeconds = Some 5
              }
            , name = "precise-code-intel-bundle-manager"
            , ports = Some
              [ Kubernetes/ContainerPort::{
                , containerPort = 3187
                , name = Some "http"
                }
              , Kubernetes/ContainerPort::{
                , containerPort = 6060
                , name = Some "debug"
                }
              ]
            , readinessProbe = Some Kubernetes/Probe::{
              , httpGet = Some Kubernetes/HTTPGetAction::{
                , path = Some "/healthz"
                , port = < Nat : Natural | String : Text >.String "http"
                , scheme = Some "HTTP"
                }
              , periodSeconds = Some 5
              , timeoutSeconds = Some 5
              }
            , resources = Some resources
            , terminationMessagePolicy = Some "FallbackToLogsOnError"
            , volumeMounts = Some
              [ Kubernetes/VolumeMount::{
                , mountPath = "/lsif-storage"
                , name = "bundle-manager"
                }
              ]
            }

let BundleManager/Deployment/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.PreciseCodeIntel.BundleManager.Deployment

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

        let container =
              BundleManager/Deployment/Containers/BundleManager/generate c

        let deployment =
              Kubernetes/Deployment::{
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = Some
                    (   [ { mapKey = "description"
                          , mapValue =
                              "Stores and manages precise code intelligence bundles."
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
                , name = Some "precise-code-intel-bundle-manager"
                , namespace = overrides.namespace
                }
              , spec = Some Kubernetes/DeploymentSpec::{
                , minReadySeconds = Some 10
                , replicas = Some replicas
                , revisionHistoryLimit = Some 10
                , selector = Kubernetes/LabelSelector::{
                  , matchLabels = Some
                    [ { mapKey = "app"
                      , mapValue = "precise-code-intel-bundle-manager"
                      }
                    ]
                  }
                , strategy = Some Kubernetes/DeploymentStrategy::{
                  , type = Some "Recreate"
                  }
                , template = Kubernetes/PodTemplateSpec::{
                  , metadata = Some Kubernetes/ObjectMeta::{
                    , labels = Some
                      [ { mapKey = "app"
                        , mapValue = "precise-code-intel-bundle-manager"
                        }
                      , { mapKey = "deploy", mapValue = "sourcegraph" }
                      ]
                    }
                  , spec = Some Kubernetes/PodSpec::{
                    , containers = [ container ]
                    , securityContext = Some Kubernetes/PodSecurityContext::{
                      , runAsUser = Some 0
                      }
                    , volumes = Some
                      [ Kubernetes/Volume::{
                        , name = "bundle-manager"
                        , persistentVolumeClaim = Some Kubernetes/PersistentVolumeClaimVolumeSource::{
                          , claimName = "bundle-manager"
                          }
                        }
                      ]
                    }
                  }
                }
              }

        in  deployment

let Worker/Service/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.PreciseCodeIntel.Worker.Service

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
                    (   [ { mapKey = "prometheus.io/port", mapValue = "6060" }
                        , { mapKey = "sourcegraph.prometheus/scrape"
                          , mapValue = "true"
                          }
                        ]
                      # additionalAnnotations
                    )
                , labels = Some
                    (   [ { mapKey = "app"
                          , mapValue = "precise-code-intel-worker"
                          }
                        , { mapKey = "deploy", mapValue = "sourcegraph" }
                        , { mapKey = "sourcegraph-resource-requires"
                          , mapValue = "no-cluster-admin"
                          }
                        ]
                      # additionalLabels
                    )
                , name = Some "precise-code-intel-worker"
                , namespace = overrides.namespace
                }
              , spec = Some Kubernetes/ServiceSpec::{
                , ports = Some
                  [ Kubernetes/ServicePort::{
                    , name = Some "http"
                    , port = 3188
                    , targetPort = Some
                        (< Nat : Natural | String : Text >.String "http")
                    }
                  , Kubernetes/ServicePort::{
                    , name = Some "debug"
                    , port = 6060
                    , targetPort = Some
                        (< Nat : Natural | String : Text >.String "debug")
                    }
                  ]
                , selector = Some
                  [ { mapKey = "app", mapValue = "precise-code-intel-worker" } ]
                , type = Some "ClusterIP"
                }
              }

        in  service

let Worker/Deployment/Containers/Worker/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.PreciseCodeIntel.Worker.Deployment.Containers.Worker

        let image =
              Optional/default
                Text
                "index.docker.io/sourcegraph/precise-code-intel-worker:3.17.2@sha256:123ddcab97c273599b569a76bcd2c7dd7c423c1de816fda1c35b781e004b4dde"
                overrides.image

        let additionalEnvironmentVariables =
              Optional/default
                (List Kubernetes/EnvVar.Type)
                ([] : List Kubernetes/EnvVar.Type)
                overrides.additionalEnvironmentVariables

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

        in  Kubernetes/Container::{
            , env = Some
                (   [ Kubernetes/EnvVar::{
                      , name = "NUM_WORKERS"
                      , value = Some "4"
                      }
                    , Kubernetes/EnvVar::{
                      , name = "PRECISE_CODE_INTEL_BUNDLE_MANAGER_URL"
                      , value = Some
                          "http://precise-code-intel-bundle-manager:3187"
                      }
                    , Kubernetes/EnvVar::{
                      , name = "POD_NAME"
                      , valueFrom = Some Kubernetes/EnvVarSource::{
                        , fieldRef = Some Kubernetes/ObjectFieldSelector::{
                          , fieldPath = "metadata.name"
                          }
                        }
                      }
                    ]
                  # additionalEnvironmentVariables
                )
            , image = Some image
            , livenessProbe = Some Kubernetes/Probe::{
              , httpGet = Some Kubernetes/HTTPGetAction::{
                , path = Some "/healthz"
                , port = < Nat : Natural | String : Text >.String "http"
                , scheme = Some "HTTP"
                }
              , initialDelaySeconds = Some 60
              , timeoutSeconds = Some 5
              }
            , name = "precise-code-intel-worker"
            , ports = Some
              [ Kubernetes/ContainerPort::{
                , containerPort = 3188
                , name = Some "http"
                }
              , Kubernetes/ContainerPort::{
                , containerPort = 6060
                , name = Some "debug"
                }
              ]
            , readinessProbe = Some Kubernetes/Probe::{
              , httpGet = Some Kubernetes/HTTPGetAction::{
                , path = Some "/healthz"
                , port = < Nat : Natural | String : Text >.String "http"
                , scheme = Some "HTTP"
                }
              , periodSeconds = Some 5
              , timeoutSeconds = Some 5
              }
            , resources = Some resources
            , terminationMessagePolicy = Some "FallbackToLogsOnError"
            }

let Worker/Deployment/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.PreciseCodeIntel.Worker.Deployment

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

        let container = Worker/Deployment/Containers/Worker/generate c

        let deployment =
              Kubernetes/Deployment::{
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = Some
                    (   [ { mapKey = "description"
                          , mapValue =
                              "Handles conversion of uploaded precise code intelligence bundles."
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
                , name = Some "precise-code-intel-worker"
                , namespace = overrides.namespace
                }
              , spec = Some Kubernetes/DeploymentSpec::{
                , minReadySeconds = Some 10
                , replicas = Some replicas
                , revisionHistoryLimit = Some 10
                , selector = Kubernetes/LabelSelector::{
                  , matchLabels = Some
                    [ { mapKey = "app", mapValue = "precise-code-intel-worker" }
                    ]
                  }
                , strategy = Some Kubernetes/DeploymentStrategy::{
                  , rollingUpdate = Some Kubernetes/RollingUpdateDeployment::{
                    , maxSurge = Some (< Nat : Natural | String : Text >.Nat 1)
                    , maxUnavailable = Some
                        (< Nat : Natural | String : Text >.Nat 1)
                    }
                  , type = Some "RollingUpdate"
                  }
                , template = Kubernetes/PodTemplateSpec::{
                  , metadata = Some Kubernetes/ObjectMeta::{
                    , labels = Some
                      [ { mapKey = "app"
                        , mapValue = "precise-code-intel-worker"
                        }
                      , { mapKey = "deploy", mapValue = "sourcegraph" }
                      ]
                    }
                  , spec = Some Kubernetes/PodSpec::{
                    , containers = [ container ]
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
            { BundleManager =
              { Deployment = BundleManager/Deployment/generate c
              , PersistentVolumeClaim =
                  BundleManager/PersistentVolumeClaim/generate c
              , Service = BundleManager/Service/generate c
              }
            , Worker =
              { Deployment = Worker/Deployment/generate c
              , Service = Worker/Service/generate c
              }
            }
        )
      : ∀(c : Configuration/global.Type) → component

in  Generate
