let Optional/default =
      ../../../../../../dhall-lang/Prelude/Optional/default.dhall

let Kubernetes/DaemonSet =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.DaemonSet.dhall

let Kubernetes/DaemonSetSpec =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.DaemonSetSpec.dhall

let Kubernetes/PodSecurityPolicy =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.policy.v1beta1.PodSecurityPolicy.dhall

let Kubernetes/ClusterRole =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.ClusterRole.dhall

let Kubernetes/ClusterRoleBinding =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.ClusterRoleBinding.dhall

let Kubernetes/Container =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Container.dhall

let Kubernetes/ContainerPort =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ContainerPort.dhall

let Kubernetes/LabelSelector =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall

let Kubernetes/ObjectMeta =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall

let Kubernetes/PodSpec = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.PodSpec.dhall

let Kubernetes/PodTemplateSpec =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.PodTemplateSpec.dhall

let Kubernetes/PolicyRule =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.PolicyRule.dhall

let Kubernetes/RoleRef = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.RoleRef.dhall

let Kubernetes/ServiceAccount =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ServiceAccount.dhall

let Kubernetes/Subject = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.Subject.dhall

let Kubernetes/Volume = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Volume.dhall

let Kubernetes/VolumeMount =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.VolumeMount.dhall

let Kubernetes/HostPathVolumeSource =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.HostPathVolumeSource.dhall

let Kubernetes/PodSecurityPolicySpec =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.extensions.v1beta1.PodSecurityPolicySpec.dhall

let Kubernetes/AllowedHostPath =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.policy.v1beta1.AllowedHostPath.dhall

let Kubernetes/FSGroupStrategyOptions =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.policy.v1beta1.FSGroupStrategyOptions.dhall

let Kubernetes/RunAsGroupStrategyOptions =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.extensions.v1beta1.RunAsGroupStrategyOptions.dhall

let Kubernetes/SELinuxStrategyOptions =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.policy.v1beta1.SELinuxStrategyOptions.dhall

let Configuration/global = ../../configuration/global.dhall

let Component = ./component.dhall

let containerResources = ../../configuration/container-resources.dhall

let containerResources/tok8s = ../../util/container-resources-to-k8s.dhall

let DaemonSet/generate =
      λ(c : Configuration/global.Type) →
        let overrides = c.Cadvisor.DaemonSet.Containers.Cadvisor

        let image =
              Optional/default
                Text
                "index.docker.io/sourcegraph/cadvisor:3.17.2@sha256:9fb42b067d1f9cc84558f61b6ec42f8cfe7ad874625c7673efa9b1f047fa3ced"
                overrides.image

        let resources =
              containerResources/tok8s
                { limits =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "300m"
                      , memory = Some "2000Mi"
                      }
                      overrides.resources.limits
                , requests =
                    containerResources.overlay
                      containerResources.Configuration::{
                      , cpu = Some "150m"
                      , memory = Some "200Mi"
                      }
                      overrides.resources.requests
                }

        let daemonSet =
              Kubernetes/DaemonSet::{
              , metadata = Kubernetes/ObjectMeta::{
                , annotations = Some
                  [ { mapKey = "description"
                    , mapValue =
                        "DaemonSet to ensure all nodes run a cAdvisor pod."
                    }
                  , { mapKey = "seccomp.security.alpha.kubernetes.io/pod"
                    , mapValue = "docker/default"
                    }
                  ]
                , labels = Some
                  [ { mapKey = "deploy", mapValue = "sourcegraph" }
                  , { mapKey = "sourcegraph-resource-requires"
                    , mapValue = "cluster-admin"
                    }
                  ]
                , name = Some "cadvisor"
                }
              , spec = Some Kubernetes/DaemonSetSpec::{
                , selector = Kubernetes/LabelSelector::{
                  , matchLabels = Some
                    [ { mapKey = "app", mapValue = "cadvisor" } ]
                  }
                , template = Kubernetes/PodTemplateSpec::{
                  , metadata = Some Kubernetes/ObjectMeta::{
                    , annotations = Some
                      [ { mapKey = "description"
                        , mapValue = "Collects and exports container metrics."
                        }
                      , { mapKey = "prometheus.io/port", mapValue = "48080" }
                      , { mapKey = "sourcegraph.prometheus/scrape"
                        , mapValue = "true"
                        }
                      ]
                    , labels = Some
                      [ { mapKey = "app", mapValue = "cadvisor" }
                      , { mapKey = "deploy", mapValue = "sourcegraph" }
                      ]
                    }
                  , spec = Some Kubernetes/PodSpec::{
                    , automountServiceAccountToken = Some False
                    , containers =
                      [ Kubernetes/Container::{
                        , args = Some
                          [ "--store_container_labels=false"
                          , "--whitelisted_container_labels=io.kubernetes.container.name,io.kubernetes.pod.name,io.kubernetes.pod.namespace,io.kubernetes.pod.uid"
                          ]
                        , image = Some image
                        , name = "cadvisor"
                        , ports = Some
                          [ Kubernetes/ContainerPort::{
                            , containerPort = 48080
                            , name = Some "http"
                            , protocol = Some "TCP"
                            }
                          ]
                        , resources = Some resources
                        , volumeMounts = Some
                          [ Kubernetes/VolumeMount::{
                            , mountPath = "/rootfs"
                            , name = "rootfs"
                            , readOnly = Some True
                            }
                          , Kubernetes/VolumeMount::{
                            , mountPath = "/var/run"
                            , name = "var-run"
                            , readOnly = Some True
                            }
                          , Kubernetes/VolumeMount::{
                            , mountPath = "/sys"
                            , name = "sys"
                            , readOnly = Some True
                            }
                          , Kubernetes/VolumeMount::{
                            , mountPath = "/var/lib/docker"
                            , name = "docker"
                            , readOnly = Some True
                            }
                          , Kubernetes/VolumeMount::{
                            , mountPath = "/dev/disk"
                            , name = "disk"
                            , readOnly = Some True
                            }
                          ]
                        }
                      ]
                    , serviceAccountName = Some "cadvisor"
                    , terminationGracePeriodSeconds = Some 30
                    , volumes = Some
                      [ Kubernetes/Volume::{
                        , hostPath = Some Kubernetes/HostPathVolumeSource::{
                          , path = "/"
                          }
                        , name = "rootfs"
                        }
                      , Kubernetes/Volume::{
                        , hostPath = Some Kubernetes/HostPathVolumeSource::{
                          , path = "/var/run"
                          }
                        , name = "var-run"
                        }
                      , Kubernetes/Volume::{
                        , hostPath = Some Kubernetes/HostPathVolumeSource::{
                          , path = "/sys"
                          }
                        , name = "sys"
                        }
                      , Kubernetes/Volume::{
                        , hostPath = Some Kubernetes/HostPathVolumeSource::{
                          , path = "/var/lib/docker"
                          }
                        , name = "docker"
                        }
                      , Kubernetes/Volume::{
                        , hostPath = Some Kubernetes/HostPathVolumeSource::{
                          , path = "/dev/disk"
                          }
                        , name = "disk"
                        }
                      ]
                    }
                  }
                }
              }

        in  daemonSet

let ClusterRole/generate =
      λ(c : Configuration/global.Type) →
        let clusterRole =
              Kubernetes/ClusterRole::{
              , metadata = Kubernetes/ObjectMeta::{
                , labels = Some
                  [ { mapKey = "app", mapValue = "cadvisor" }
                  , { mapKey = "category", mapValue = "rbac" }
                  , { mapKey = "deploy", mapValue = "sourcegraph" }
                  , { mapKey = "sourcegraph-resource-requires"
                    , mapValue = "cluster-admin"
                    }
                  ]
                , name = Some "cadvisor"
                }
              , rules = Some
                [ Kubernetes/PolicyRule::{
                  , apiGroups = Some [ "policy" ]
                  , resourceNames = Some [ "cadvisor" ]
                  , resources = Some [ "podsecuritypolicies" ]
                  , verbs = [ "use" ]
                  }
                ]
              }

        in  clusterRole

let ClusterRoleBinding/generate =
      λ(c : Configuration/global.Type) →
        let clusterRoleBinding =
              Kubernetes/ClusterRoleBinding::{
              , metadata = Kubernetes/ObjectMeta::{
                , labels = Some
                  [ { mapKey = "app", mapValue = "cadvisor" }
                  , { mapKey = "category", mapValue = "rbac" }
                  , { mapKey = "deploy", mapValue = "sourcegraph" }
                  , { mapKey = "sourcegraph-resource-requires"
                    , mapValue = "cluster-admin"
                    }
                  ]
                , name = Some "cadvisor"
                }
              , roleRef = Kubernetes/RoleRef::{
                , apiGroup = "rbac.authorization.k8s.io"
                , kind = "ClusterRole"
                , name = "cadvisor"
                }
              , subjects = Some
                [ Kubernetes/Subject::{
                  , kind = "ServiceAccount"
                  , name = "cadvisor"
                  , namespace = Some "default"
                  }
                ]
              }

        in  clusterRoleBinding

let ServiceAccount/generate =
      λ(c : Configuration/global.Type) →
        let serviceAccount =
              Kubernetes/ServiceAccount::{
              , metadata = Kubernetes/ObjectMeta::{
                , labels = Some
                  [ { mapKey = "app", mapValue = "cadvisor" }
                  , { mapKey = "category", mapValue = "rbac" }
                  , { mapKey = "deploy", mapValue = "sourcegraph" }
                  , { mapKey = "sourcegraph-resource-requires"
                    , mapValue = "cluster-admin"
                    }
                  ]
                , name = Some "cadvisor"
                }
              }

        in  serviceAccount

let PodSecurityPolicy/generate =
      λ(c : Configuration/global.Type) →
        let podSecurityPolicy =
              Kubernetes/PodSecurityPolicy::{
              , metadata = Kubernetes/ObjectMeta::{
                , labels = Some
                  [ { mapKey = "app", mapValue = "cadvisor" }
                  , { mapKey = "deploy", mapValue = "sourcegraph" }
                  , { mapKey = "sourcegraph-resource-requires"
                    , mapValue = "cluster-admin"
                    }
                  ]
                , name = Some "cadvisor"
                }
              , spec = Some Kubernetes/PodSecurityPolicySpec::{
                , allowedHostPaths = Some
                  [ Kubernetes/AllowedHostPath::{ pathPrefix = Some "/" }
                  , Kubernetes/AllowedHostPath::{ pathPrefix = Some "/var/run" }
                  , Kubernetes/AllowedHostPath::{ pathPrefix = Some "/sys" }
                  , Kubernetes/AllowedHostPath::{
                    , pathPrefix = Some "/var/lib/docker"
                    }
                  , Kubernetes/AllowedHostPath::{
                    , pathPrefix = Some "/dev/disk"
                    }
                  ]
                , fsGroup = Kubernetes/FSGroupStrategyOptions::{
                  , rule = Some "RunAsAny"
                  }
                , runAsUser = Kubernetes/RunAsGroupStrategyOptions::{
                  , rule = "RunAsAny"
                  }
                , seLinux = Kubernetes/SELinuxStrategyOptions::{
                  , rule = "RunAsAny"
                  }
                , supplementalGroups = Kubernetes/FSGroupStrategyOptions::{
                  , rule = Some "RunAsAny"
                  }
                , volumes = Some [ "*" ]
                }
              }

        in  podSecurityPolicy

let Generate =
        ( λ(c : Configuration/global.Type) →
            { DaemonSet = DaemonSet/generate c
            , ClusterRole = ClusterRole/generate c
            , ClusterRoleBinding = ClusterRoleBinding/generate c
            , ServiceAccount = ServiceAccount/generate c
            , PodSecurityPolicy = PodSecurityPolicy/generate c
            }
        )
      : ∀(c : Configuration/global.Type) → Component

in  Generate
