let Optional/default =
      ../../../../../../dhall-lang/Prelude/Optional/default.dhall

let Kubernetes/ConfigMap =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ConfigMap.dhall

let Kubernetes/ConfigMapVolumeSource =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ConfigMapVolumeSource.dhall

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

let Kubernetes/EnvVar = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.EnvVar.dhall

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

let Kubernetes/SecurityContext =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.SecurityContext.dhall

let Kubernetes/Service = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Service.dhall

let Kubernetes/ServicePort =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ServicePort.dhall

let Kubernetes/ServiceSpec =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ServiceSpec.dhall

let Kubernetes/Volume = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Volume.dhall

let Kubernetes/VolumeMount =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.VolumeMount.dhall

let Configuration/global = ../../configuration/global.dhall

let Util/DeploySourcegraphLabel = ../../util/deploy-sourcegraph-label.dhall

let Util/KeyValuePair = ../../util/key-value-pair.dhall

let component = ./component.dhall

let containerResources = ../../configuration/container-resources.dhall

let containerResources/tok8s = ../../util/container-resources-to-k8s.dhall

let Octal = ../../util/octal.dhall

let ConfigMap/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Postgres.ConfigMap

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

        let configMap =
              Kubernetes/ConfigMap::{
              , data = Some
                [ { mapKey = "postgresql.conf"
                  , mapValue = ./postgresql.conf as Text
                  }
                ]
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = Some
                    (   [ { mapKey = "description"
                          , mapValue = "Configuration for PostgreSQL"
                          }
                        ]
                      # additionalAnnotations
                    )
                , labels = Some
                    (   Util/DeploySourcegraphLabel
                      # [ { mapKey = "sourcegraph-resource-requires"
                          , mapValue = "no-cluster-admin"
                          }
                        ]
                      # additionalLabels
                    )
                , namespace = overrides.namespace
                , name = Some "pgsql-conf"
                }
              }

        in  configMap

let postgresContainer/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Postgres.Deployment.Containers.Postgres

        let environment = overrides.additionalEnvironmentVariables

        let image =
              Optional/default
                Text
                "index.docker.io/sourcegraph/postgres-11.4:3.16.1@sha256:63090799b34b3115a387d96fe2227a37999d432b774a1d9b7966b8c5d81b56ad"
                overrides.image

        let resources =
              containerResources/tok8s
                { limits =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "4"
                      , memory = Some "2Gi"
                      }
                      overrides.resources.limits
                , requests =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "4"
                      , memory = Some "2Gi"
                      }
                      overrides.resources.requests
                }

        let container =
              Kubernetes/Container::{
              , image = Some image
              , livenessProbe = Some Kubernetes/Probe::{
                , exec = Some { command = Some [ "/liveness.sh" ] }
                , initialDelaySeconds = Some 15
                }
              , name = "pgsql"
              , ports = Some
                [ Kubernetes/ContainerPort::{
                  , containerPort = 5432
                  , name = Some "pgsql"
                  }
                ]
              , readinessProbe = Some Kubernetes/Probe::{
                , exec = Some { command = Some [ "/ready.sh" ] }
                }
              , env = environment
              , resources = Some resources
              , terminationMessagePolicy = Some "FallbackToLogsOnError"
              , volumeMounts = Some
                [ Kubernetes/VolumeMount::{ mountPath = "/data", name = "disk" }
                , Kubernetes/VolumeMount::{
                  , mountPath = "/conf"
                  , name = "pgsql-conf"
                  }
                ]
              }

        in  container

let postgresExporterContainer/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Postgres.Deployment.Containers.PostgresExporter

        let additionalEnvironmentVariables =
              Optional/default
                (List Kubernetes/EnvVar.Type)
                ([] : List Kubernetes/EnvVar.Type)
                overrides.additionalEnvironmentVariables

        let environment =
                [ Kubernetes/EnvVar::{
                  , name = "DATA_SOURCE_NAME"
                  , value = Some
                      "postgres://sg:@localhost:5432/?sslmode=disable"
                  }
                ]
              # additionalEnvironmentVariables

        let image =
              Optional/default
                Text
                "wrouesnel/postgres_exporter:v0.7.0@sha256:785c919627c06f540d515aac88b7966f352403f73e931e70dc2cbf783146a98b"
                overrides.image

        let resources =
              containerResources/tok8s
                { limits =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "10m"
                      , memory = Some "50Mi"
                      }
                      overrides.resources.limits
                , requests =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "10m"
                      , memory = Some "50Mi"
                      }
                      overrides.resources.requests
                }

        let container =
              Kubernetes/Container::{
              , env = Some environment
              , image = Some image
              , name = "pgsql-exporter"
              , resources = Some resources
              , terminationMessagePolicy = Some "FallbackToLogsOnError"
              }

        in  container

let initContainer/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Postgres.Deployment.Containers.Init

        let environment = overrides.additionalEnvironmentVariables

        let image =
              Optional/default
                Text
                "sourcegraph/alpine:3.10@sha256:4d05cd5669726fc38823e92320659a6d1ef7879e62268adec5df658a0bacf65c"
                overrides.image

        let container =
              Kubernetes/Container::{
              , command = Some
                [ "sh"
                , "-c"
                , "if [ -d /data/pgdata-11 ]; then chmod 750 /data/pgdata-11; fi"
                ]
              , env = environment
              , image = Some image
              , name = "correct-data-dir-permissions"
              , securityContext = Some Kubernetes/SecurityContext::{
                , runAsUser = Some 0
                }
              , volumeMounts = Some
                [ Kubernetes/VolumeMount::{ mountPath = "/data", name = "disk" }
                ]
              }

        in  container

let Deployment/generate =
      λ(c : Configuration/global.Type) →
        let additionalAnnotations =
              Optional/default
                (List Util/KeyValuePair)
                ([] : List Util/KeyValuePair)
                c.Postgres.Deployment.additionalAnnotations

        let additionalLabels =
              Optional/default
                (List Util/KeyValuePair)
                ([] : List Util/KeyValuePair)
                c.Postgres.Deployment.additionalLabels

        let postgresContainer = postgresContainer/generate c

        let postgresExporterContainer = postgresExporterContainer/generate c

        let initContainer = initContainer/generate c

        let deployment =
              Kubernetes/Deployment::{
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = Some
                    (   [ { mapKey = "description"
                          , mapValue = "Postgres database for various data."
                          }
                        ]
                      # additionalAnnotations
                    )
                , labels = Some
                    (   Util/DeploySourcegraphLabel
                      # [ { mapKey = "sourcegraph-resource-requires"
                          , mapValue = "no-cluster-admin"
                          }
                        ]
                      # additionalLabels
                    )
                , namespace = c.Postgres.Deployment.namespace
                , name = Some "pgsql"
                }
              , spec = Some Kubernetes/DeploymentSpec::{
                , minReadySeconds = Some 10
                , replicas = Some 1
                , revisionHistoryLimit = Some 10
                , selector = Kubernetes/LabelSelector::{
                  , matchLabels = Some
                    [ { mapKey = "app", mapValue = "pgsql" } ]
                  }
                , strategy = Some Kubernetes/DeploymentStrategy::{
                  , type = Some "Recreate"
                  }
                , template = Kubernetes/PodTemplateSpec::{
                  , metadata = Some Kubernetes/ObjectMeta::{
                    , labels = Some
                      [ { mapKey = "app", mapValue = "pgsql" }
                      , { mapKey = "deploy", mapValue = "sourcegraph" }
                      , { mapKey = "group", mapValue = "backend" }
                      ]
                    }
                  , spec = Some Kubernetes/PodSpec::{
                    , containers =
                      [ postgresContainer, postgresExporterContainer ]
                    , initContainers = Some [ initContainer ]
                    , securityContext = Some Kubernetes/PodSecurityContext::{
                      , runAsUser = Some 0
                      }
                    , volumes = Some
                      [ Kubernetes/Volume::{
                        , name = "disk"
                        , persistentVolumeClaim = Some
                          { claimName = "pgsql", readOnly = None Bool }
                        }
                      , Kubernetes/Volume::{
                        , configMap = Some Kubernetes/ConfigMapVolumeSource::{
                          , defaultMode = Some
                              (Octal.toNatural Octal.Enum.Oo777)
                          , name = Some "pgsql-conf"
                          }
                        , name = "pgsql-conf"
                        }
                      ]
                    }
                  }
                }
              }

        in  deployment

let PersistentVolumeClaim/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Postgres.PersistentVolumeClaim

        let annotations = overrides.additionalAnnotations

        let additionalLabels =
              Optional/default
                (List Util/KeyValuePair)
                ([] : List Util/KeyValuePair)
                overrides.additionalLabels

        let labels =
                toMap
                  { sourcegraph-resource-requires = "no-cluster-admin"
                  , deploy = "sourcegraph"
                  }
              # additionalLabels

        let persistentVolumeClaim =
              Kubernetes/PersistentVolumeClaim::{
              , apiVersion = Some "v1"
              , kind = Some "PersistentVolumeClaim"
              , metadata = Kubernetes/ObjectMeta::{
                , annotations
                , labels = Some labels
                , namespace = overrides.namespace
                , name = Some "pgsql"
                }
              , spec = Some Kubernetes/PersistentVolumeClaimSpec::{
                , accessModes = Some [ "ReadWriteOnce" ]
                , resources = Some Kubernetes/ResourceRequirements::{
                  , requests = Some (toMap { storage = "200Gi" })
                  }
                , storageClassName = Some "sourcegraph"
                }
              }

        in  persistentVolumeClaim

let Service/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Postgres.Service

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

        let annotations =
                toMap
                  { `sourcegraph.prometheus/scrape` = "true"
                  , `prometheus.io/port` = "9187"
                  }
              # additionalAnnotations

        let labels =
                toMap
                  { sourcegraph-resource-requires = "no-cluster-admin"
                  , deploy = "sourcegraph"
                  }
              # additionalLabels

        let service =
              Kubernetes/Service::{
              , apiVersion = "v1"
              , kind = "Service"
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = Some annotations
                , labels = Some labels
                , namespace = overrides.namespace
                , name = Some "pgsql"
                }
              , spec = Some Kubernetes/ServiceSpec::{
                , ports = Some
                  [ Kubernetes/ServicePort::{
                    , name = Some "pgsql"
                    , port = 5432
                    , targetPort = Some (Kubernetes/NatOrString.String "pgsql")
                    }
                  ]
                , selector = Some (toMap { app = "pgsql" })
                , type = Some "ClusterIP"
                }
              }

        in  service

let Generate =
        ( λ(c : Configuration/global.Type) →
            { Deployment = Deployment/generate c
            , Service = Service/generate c
            , PersistentVolumeClaim = PersistentVolumeClaim/generate c
            , ConfigMap = ConfigMap/generate c
            }
        )
      : ∀(c : Configuration/global.Type) → component

in  Generate
