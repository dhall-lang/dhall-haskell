let Natural/enumerate =
      ../../../../../../dhall-lang/Prelude/Natural/enumerate.dhall

let Optional/default =
      ../../../../../../dhall-lang/Prelude/Optional/default.dhall

let Text/concatMapSep =
      ../../../../../../dhall-lang/Prelude/Text/concatMapSep.dhall

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

let Kubernetes/EnvVarSource =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.EnvVarSource.dhall

let Kubernetes/HTTPGetAction =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.HTTPGetAction.dhall

let Kubernetes/HTTPIngressPath =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.extensions.v1beta1.HTTPIngressPath.dhall

let Kubernetes/HTTPIngressRuleValue =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.extensions.v1beta1.HTTPIngressRuleValue.dhall

let Kubernetes/Ingress =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.networking.v1beta1.Ingress.dhall

let Kubernetes/IngressRule =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.networking.v1beta1.IngressRule.dhall

let Kubernetes/IngressSpec =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.networking.v1beta1.IngressSpec.dhall

let Kubernetes/NatOrString =
      ../../../../k8s/dhall-kubernetes/1.17/types/io.k8s.apimachinery.pkg.util.intstr.NatOrString.dhall

let Kubernetes/LabelSelector =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall

let Kubernetes/ObjectMeta =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall

let Kubernetes/PodSecurityContext =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.PodSecurityContext.dhall

let Kubernetes/PodSpec = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.PodSpec.dhall

let Kubernetes/PodTemplateSpec =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.PodTemplateSpec.dhall

let Kubernetes/PolicyRule =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.PolicyRule.dhall

let Kubernetes/Probe = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Probe.dhall

let Kubernetes/Role = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.Role.dhall

let Kubernetes/RoleBinding =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.RoleBinding.dhall

let Kubernetes/RoleRef = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.RoleRef.dhall

let Kubernetes/Service = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Service.dhall

let Kubernetes/ServiceAccount =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ServiceAccount.dhall

let Kubernetes/ServicePort =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ServicePort.dhall

let Kubernetes/ServiceSpec =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ServiceSpec.dhall

let Kubernetes/Subject = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.Subject.dhall

let Kubernetes/VolumeMount =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.VolumeMount.dhall

let Configuration/global = ../../configuration/global.dhall

let Util/EmptyCacheSSDVolume = ../../util/empty-cache-ssd-volume.dhall

let Util/JaegerAgent = ../../util/jaeger-agent.dhall

let Util/KeyValuePair = ../../util/key-value-pair.dhall

let component = ./component.dhall

let containerResources = ../../configuration/container-resources.dhall

let containerResources/tok8s = ../../util/container-resources-to-k8s.dhall

let makeGitserverEnvVar =
      λ(replicas : Natural) →
        let indicies = Natural/enumerate replicas

        let makeEndpoint =
              λ(i : Natural) → "gitserver-${Natural/show i}.gitserver:3178"

        in  Text/concatMapSep " " Natural makeEndpoint indicies

let test0GitserverEnvVar =
        assert
      :   makeGitserverEnvVar 3
        ≡ "gitserver-0.gitserver:3178 gitserver-1.gitserver:3178 gitserver-2.gitserver:3178"

let test1GitserverEnvVar =
      assert : makeGitserverEnvVar 1 ≡ "gitserver-0.gitserver:3178"

let frontendContainer/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Frontend.Deployment.Containers.SourcegraphFrontend

        let additionalEnvironmentVariables =
              Optional/default
                (List Kubernetes/EnvVar.Type)
                ([] : List Kubernetes/EnvVar.Type)
                overrides.additionalEnvironmentVariables

        let gitserverReplicas =
              Optional/default Natural 1 c.Gitserver.StatefulSet.replicas

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
                      , cpu = Some "2"
                      , memory = Some "2G"
                      }
                      overrides.resources.requests
                }

        let environment =
                [ Kubernetes/EnvVar::{ name = "PGDATABASE", value = Some "sg" }
                , Kubernetes/EnvVar::{ name = "PGHOST", value = Some "pgsql" }
                , Kubernetes/EnvVar::{ name = "PGPORT", value = Some "5432" }
                , Kubernetes/EnvVar::{
                  , name = "PGSSLMODE"
                  , value = Some "disable"
                  }
                , Kubernetes/EnvVar::{ name = "PGUSER", value = Some "sg" }
                , Kubernetes/EnvVar::{
                  , name = "SRC_GIT_SERVERS"
                  , value = Some (makeGitserverEnvVar gitserverReplicas)
                  }
                , Kubernetes/EnvVar::{
                  , name = "POD_NAME"
                  , valueFrom = Some Kubernetes/EnvVarSource::{
                    , fieldRef = Some
                      { apiVersion = None Text, fieldPath = "metadata.name" }
                    }
                  }
                , Kubernetes/EnvVar::{
                  , name = "CACHE_DIR"
                  , value = Some "/mnt/cache/\$(POD_NAME)"
                  }
                , Kubernetes/EnvVar::{
                  , name = "GRAFANA_SERVER_URL"
                  , value = Some "http://grafana:30070"
                  }
                , Kubernetes/EnvVar::{
                  , name = "JAEGER_SERVER_URL"
                  , value = Some "http://jaeger-query:16686"
                  }
                , Kubernetes/EnvVar::{
                  , name = "PRECISE_CODE_INTEL_BUNDLE_MANAGER_URL"
                  , value = Some "http://precise-code-intel-bundle-manager:3187"
                  }
                , Kubernetes/EnvVar::{
                  , name = "PROMETHEUS_URL"
                  , value = Some "http://prometheus:30090"
                  }
                ]
              # additionalEnvironmentVariables

        let image =
              Optional/default
                Text
                "index.docker.io/sourcegraph/frontend:3.17.2@sha256:2378899365619635ce7acd983582407688d4def72a3fd62ae6fa0c23a0554fde"
                overrides.image

        let container =
              Kubernetes/Container::{
              , args = Some [ "serve" ]
              , env = Some environment
              , image = Some image
              , livenessProbe = Some Kubernetes/Probe::{
                , httpGet = Some Kubernetes/HTTPGetAction::{
                  , path = Some "/healthz"
                  , port = Kubernetes/NatOrString.String "http"
                  , scheme = Some "HTTP"
                  }
                , initialDelaySeconds = Some 300
                , timeoutSeconds = Some 5
                }
              , name = "frontend"
              , ports = Some
                [ Kubernetes/ContainerPort::{
                  , containerPort = 3080
                  , name = Some "http"
                  }
                , Kubernetes/ContainerPort::{
                  , containerPort = 3090
                  , name = Some "http-internal"
                  }
                ]
              , readinessProbe = Some Kubernetes/Probe::{
                , httpGet = Some Kubernetes/HTTPGetAction::{
                  , path = Some "/healthz"
                  , port = Kubernetes/NatOrString.String "http"
                  , scheme = Some "HTTP"
                  }
                , periodSeconds = Some 5
                , timeoutSeconds = Some 5
                }
              , resources = Some resources
              , terminationMessagePolicy = Some "FallbackToLogsOnError"
              , volumeMounts = Some
                [ Kubernetes/VolumeMount::{
                  , mountPath = "/mnt/cache"
                  , name = "cache-ssd"
                  }
                ]
              }

        in  container

let Deployment/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Frontend.Deployment

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

        let frontendContainer = frontendContainer/generate c

        let deployment =
              Kubernetes/Deployment::{
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = Some
                    (   [ { mapKey = "description"
                          , mapValue =
                              "Serves the frontend of Sourcegraph via HTTP(S)."
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
                , name = Some "sourcegraph-frontend"
                }
              , spec = Some Kubernetes/DeploymentSpec::{
                , minReadySeconds = Some 10
                , replicas = Some replicas
                , revisionHistoryLimit = Some 10
                , selector = Kubernetes/LabelSelector::{
                  , matchLabels = Some
                    [ { mapKey = "app", mapValue = "sourcegraph-frontend" } ]
                  }
                , strategy = Some Kubernetes/DeploymentStrategy::{
                  , rollingUpdate = Some
                    { maxSurge = Some (Kubernetes/NatOrString.Nat 2)
                    , maxUnavailable = Some (Kubernetes/NatOrString.Nat 0)
                    }
                  , type = Some "RollingUpdate"
                  }
                , template = Kubernetes/PodTemplateSpec::{
                  , metadata = Some Kubernetes/ObjectMeta::{
                    , labels = Some
                      [ { mapKey = "app", mapValue = "sourcegraph-frontend" }
                      , { mapKey = "deploy", mapValue = "sourcegraph" }
                      ]
                    }
                  , spec = Some Kubernetes/PodSpec::{
                    , containers = [ frontendContainer, Util/JaegerAgent ]
                    , securityContext = Some Kubernetes/PodSecurityContext::{
                      , runAsUser = Some 0
                      }
                    , serviceAccountName = Some "sourcegraph-frontend"
                    , volumes = Some [ Util/EmptyCacheSSDVolume ]
                    }
                  }
                }
              }

        in  deployment

let Ingress/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Frontend.Ingress

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

        let ingress =
              Kubernetes/Ingress::{
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = Some
                    (   [ { mapKey = "kubernetes.io/ingress.class"
                          , mapValue = "nginx"
                          }
                        , { mapKey =
                              "nginx.ingress.kubernetes.io/proxy-body-size"
                          , mapValue = "150m"
                          }
                        ]
                      # additionalAnnotations
                    )
                , labels = Some
                    (   [ { mapKey = "app", mapValue = "sourcegraph-frontend" }
                        , { mapKey = "deploy", mapValue = "sourcegraph" }
                        , { mapKey = "sourcegraph-resource-requires"
                          , mapValue = "no-cluster-admin"
                          }
                        ]
                      # additionalLabels
                    )
                , namespace = overrides.namespace
                , name = Some "sourcegraph-frontend"
                }
              , spec = Some Kubernetes/IngressSpec::{
                , tls = overrides.tls
                , rules = Some
                  [ Kubernetes/IngressRule::{
                    , http = Some Kubernetes/HTTPIngressRuleValue::{
                      , paths =
                        [ Kubernetes/HTTPIngressPath::{
                          , backend =
                            { serviceName = "sourcegraph-frontend"
                            , servicePort = Kubernetes/NatOrString.Nat 30080
                            }
                          , path = Some "/"
                          }
                        ]
                      }
                    }
                  ]
                }
              }

        in  ingress

let Role/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Frontend.Role

        let additionalLabels =
              Optional/default
                (List Util/KeyValuePair)
                ([] : List Util/KeyValuePair)
                overrides.additionalLabels

        let role =
              Kubernetes/Role::{
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = overrides.additionalAnnotations
                , labels = Some
                    (   [ { mapKey = "category", mapValue = "rbac" }
                        , { mapKey = "deploy", mapValue = "sourcegraph" }
                        , { mapKey = "sourcegraph-resource-requires"
                          , mapValue = "cluster-admin"
                          }
                        ]
                      # additionalLabels
                    )
                , namespace = overrides.namespace
                , name = Some "sourcegraph-frontend"
                }
              , rules = Some
                [ Kubernetes/PolicyRule::{
                  , apiGroups = Some [ "" ]
                  , resources = Some [ "endpoints", "services" ]
                  , verbs = [ "get", "list", "watch" ]
                  }
                ]
              }

        in  role

let RoleBinding/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Frontend.RoleBinding

        let additionalLabels =
              Optional/default
                (List Util/KeyValuePair)
                ([] : List Util/KeyValuePair)
                overrides.additionalLabels

        let roleBinding =
              Kubernetes/RoleBinding::{
              , metadata = Kubernetes/ObjectMeta::{
                , labels = Some
                    (   [ { mapKey = "category", mapValue = "rbac" }
                        , { mapKey = "deploy", mapValue = "sourcegraph" }
                        , { mapKey = "sourcegraph-resource-requires"
                          , mapValue = "cluster-admin"
                          }
                        ]
                      # additionalLabels
                    )
                , namespace = overrides.namespace
                , name = Some "sourcegraph-frontend"
                }
              , roleRef = Kubernetes/RoleRef::{
                , apiGroup = ""
                , kind = "Role"
                , name = "sourcegraph-frontend"
                }
              , subjects = Some
                [ Kubernetes/Subject::{
                  , kind = "ServiceAccount"
                  , name = "sourcegraph-frontend"
                  }
                ]
              }

        in  roleBinding

let Service/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Frontend.Service

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
                    (   [ { mapKey = "app", mapValue = "sourcegraph-frontend" }
                        , { mapKey = "deploy", mapValue = "sourcegraph" }
                        , { mapKey = "sourcegraph-resource-requires"
                          , mapValue = "no-cluster-admin"
                          }
                        ]
                      # additionalLabels
                    )
                , namespace = overrides.namespace
                , name = Some "sourcegraph-frontend"
                }
              , spec = Some Kubernetes/ServiceSpec::{
                , ports = Some
                  [ Kubernetes/ServicePort::{
                    , name = Some "http"
                    , port = 30080
                    , targetPort = Some (Kubernetes/NatOrString.String "http")
                    }
                  ]
                , selector = Some
                  [ { mapKey = "app", mapValue = "sourcegraph-frontend" } ]
                , type = Some "ClusterIP"
                }
              }

        in  service

let ServiceAccount/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Frontend.ServiceAccount

        let additionalLabels =
              Optional/default
                (List Util/KeyValuePair)
                ([] : List Util/KeyValuePair)
                overrides.additionalLabels

        let serviceAccount =
              Kubernetes/ServiceAccount::{
              , imagePullSecrets = Some [ { name = Some "docker-registry" } ]
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = overrides.additionalAnnotations
                , labels = Some
                    (   [ { mapKey = "category", mapValue = "rbac" }
                        , { mapKey = "deploy", mapValue = "sourcegraph" }
                        , { mapKey = "sourcegraph-resource-requires"
                          , mapValue = "no-cluster-admin"
                          }
                        ]
                      # additionalLabels
                    )
                , namespace = overrides.namespace
                , name = Some "sourcegraph-frontend"
                }
              }

        in  serviceAccount

let ServiceInternal/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Frontend.ServiceInternal

        let additionalLabels =
              Optional/default
                (List Util/KeyValuePair)
                ([] : List Util/KeyValuePair)
                c.Frontend.ServiceInternal.additionalLabels

        let serviceInternal =
              Kubernetes/Service::{
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = overrides.additionalAnnotations
                , labels = Some
                    (   [ { mapKey = "app", mapValue = "sourcegraph-frontend" }
                        , { mapKey = "deploy", mapValue = "sourcegraph" }
                        , { mapKey = "sourcegraph-resource-requires"
                          , mapValue = "no-cluster-admin"
                          }
                        ]
                      # additionalLabels
                    )
                , namespace = overrides.namespace
                , name = Some "sourcegraph-frontend-internal"
                }
              , spec = Some Kubernetes/ServiceSpec::{
                , ports = Some
                  [ Kubernetes/ServicePort::{
                    , name = Some "http-internal"
                    , port = 80
                    , targetPort = Some
                        (Kubernetes/NatOrString.String "http-internal")
                    }
                  ]
                , selector = Some
                  [ { mapKey = "app", mapValue = "sourcegraph-frontend" } ]
                , type = Some "ClusterIP"
                }
              }

        in  serviceInternal

let Generate =
        ( λ(c : Configuration/global.Type) →
            { Deployment = Deployment/generate c
            , Ingress = Ingress/generate c
            , Role = Role/generate c
            , RoleBinding = RoleBinding/generate c
            , Service = Service/generate c
            , ServiceAccount = ServiceAccount/generate c
            , ServiceInternal = ServiceInternal/generate c
            }
        )
      : ∀(c : Configuration/global.Type) → component

in  Generate
