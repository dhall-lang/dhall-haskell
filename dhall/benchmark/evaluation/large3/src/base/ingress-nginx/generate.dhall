let Kubernetes/Container =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Container.dhall

let Kubernetes/ContainerPort =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ContainerPort.dhall

let Kubernetes/DeploymentSpec =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.DeploymentSpec.dhall

let Kubernetes/EnvVar = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.EnvVar.dhall

let Kubernetes/EnvVarSource =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.EnvVarSource.dhall

let Kubernetes/HTTPGetAction =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.HTTPGetAction.dhall

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

let Kubernetes/RoleRef = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.RoleRef.dhall

let Kubernetes/ServicePort =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ServicePort.dhall

let Kubernetes/ServiceSpec =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ServiceSpec.dhall

let Kubernetes/Subject = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.Subject.dhall

let Kubernetes/VolumeMount =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.VolumeMount.dhall

let Kubernetes/Job = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.batch.v1.Job.dhall

let Kubernetes/ClusterRole =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.ClusterRole.dhall

let Kubernetes/Role = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.Role.dhall

let Kubernetes/RoleBinding =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.RoleBinding.dhall

let Kubernetes/ValidatingWebhookConfiguration =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.admissionregistration.v1.ValidatingWebhookConfiguration.dhall

let Kubernetes/ValidatingWebhook =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.admissionregistration.v1.ValidatingWebhook.dhall

let Kubernetes/WebhookClientConfig =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.admissionregistration.v1.WebhookClientConfig.dhall

let Kubernetes/RuleWithOperations =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.admissionregistration.v1.RuleWithOperations.dhall

let Kubernetes/ObjectFieldSelector =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ObjectFieldSelector.dhall

let Kubernetes/Lifecycle =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Lifecycle.dhall

let Kubernetes/Handler = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Handler.dhall

let Kubernetes/ExecAction =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ExecAction.dhall

let Kubernetes/ResourceRequirements =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ResourceRequirements.dhall

let Kubernetes/SecurityContext =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.SecurityContext.dhall

let Kubernetes/Capabilities =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Capabilities.dhall

let Kubernetes/Volume = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Volume.dhall

let Kubernetes/SecretVolumeSource =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.SecretVolumeSource.dhall

let Kubernetes/ConfigMap =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ConfigMap.dhall

let Kubernetes/Deployment =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.Deployment.dhall

let Kubernetes/Service = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Service.dhall

let Kubernetes/ServiceAccount =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ServiceAccount.dhall

let Kubernetes/JobSpec =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.batch.v1.JobSpec.dhall

let Kubernetes/Namespace =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Namespace.dhall

let Configuration/global = ../../configuration/global.dhall

let component = ./component.dhall

let shape = ./shape.dhall

let Generate =
        ( λ(c : Configuration/global.Type) →
            let overrides = c.IngressNginx

            let IPAddress = overrides.IPAddress

            let ingressNginx
                : shape
                = { AdmissionWebhook =
                    { ClusterRole.ingress-nginx-admission = Kubernetes/ClusterRole::{
                      , metadata = Kubernetes/ObjectMeta::{
                        , annotations = Some
                          [ { mapKey = "helm.sh/hook"
                            , mapValue =
                                "pre-install,pre-upgrade,post-install,post-upgrade"
                            }
                          , { mapKey = "helm.sh/hook-delete-policy"
                            , mapValue = "before-hook-creation,hook-succeeded"
                            }
                          ]
                        , labels = Some
                          [ { mapKey = "app.kubernetes.io/component"
                            , mapValue = "admission-webhook"
                            }
                          , { mapKey = "app.kubernetes.io/instance"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/managed-by"
                            , mapValue = "Helm"
                            }
                          , { mapKey = "app.kubernetes.io/name"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/version"
                            , mapValue = "0.34.1"
                            }
                          , { mapKey = "helm.sh/chart"
                            , mapValue = "ingress-nginx-2.11.1"
                            }
                          ]
                        , name = Some "ingress-nginx-admission"
                        , namespace = Some "ingress-nginx"
                        }
                      , rules = Some
                        [ Kubernetes/PolicyRule::{
                          , apiGroups = Some [ "admissionregistration.k8s.io" ]
                          , resources = Some
                            [ "validatingwebhookconfigurations" ]
                          , verbs = [ "get", "update" ]
                          }
                        ]
                      }
                    , ClusterRoleBinding.ingress-nginx-admission = Kubernetes/RoleBinding::{
                      , kind = "ClusterRoleBinding"
                      , metadata = Kubernetes/ObjectMeta::{
                        , annotations = Some
                          [ { mapKey = "helm.sh/hook"
                            , mapValue =
                                "pre-install,pre-upgrade,post-install,post-upgrade"
                            }
                          , { mapKey = "helm.sh/hook-delete-policy"
                            , mapValue = "before-hook-creation,hook-succeeded"
                            }
                          ]
                        , labels = Some
                          [ { mapKey = "app.kubernetes.io/component"
                            , mapValue = "admission-webhook"
                            }
                          , { mapKey = "app.kubernetes.io/instance"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/managed-by"
                            , mapValue = "Helm"
                            }
                          , { mapKey = "app.kubernetes.io/name"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/version"
                            , mapValue = "0.34.1"
                            }
                          , { mapKey = "helm.sh/chart"
                            , mapValue = "ingress-nginx-2.11.1"
                            }
                          ]
                        , name = Some "ingress-nginx-admission"
                        , namespace = Some "ingress-nginx"
                        }
                      , roleRef = Kubernetes/RoleRef::{
                        , apiGroup = "rbac.authorization.k8s.io"
                        , kind = "ClusterRole"
                        , name = "ingress-nginx-admission"
                        }
                      , subjects = Some
                        [ Kubernetes/Subject::{
                          , kind = "ServiceAccount"
                          , name = "ingress-nginx-admission"
                          , namespace = Some "ingress-nginx"
                          }
                        ]
                      }
                    , Job =
                      { ingress-nginx-admission-create = Kubernetes/Job::{
                        , metadata = Kubernetes/ObjectMeta::{
                          , annotations = Some
                            [ { mapKey = "helm.sh/hook"
                              , mapValue = "pre-install,pre-upgrade"
                              }
                            , { mapKey = "helm.sh/hook-delete-policy"
                              , mapValue = "before-hook-creation,hook-succeeded"
                              }
                            ]
                          , labels = Some
                            [ { mapKey = "app.kubernetes.io/component"
                              , mapValue = "admission-webhook"
                              }
                            , { mapKey = "app.kubernetes.io/instance"
                              , mapValue = "ingress-nginx"
                              }
                            , { mapKey = "app.kubernetes.io/managed-by"
                              , mapValue = "Helm"
                              }
                            , { mapKey = "app.kubernetes.io/name"
                              , mapValue = "ingress-nginx"
                              }
                            , { mapKey = "app.kubernetes.io/version"
                              , mapValue = "0.34.1"
                              }
                            , { mapKey = "helm.sh/chart"
                              , mapValue = "ingress-nginx-2.11.1"
                              }
                            ]
                          , name = Some "ingress-nginx-admission-create"
                          , namespace = Some "ingress-nginx"
                          }
                        , spec = Some Kubernetes/JobSpec::{
                          , template = Kubernetes/PodTemplateSpec::{
                            , metadata = Some Kubernetes/ObjectMeta::{
                              , labels = Some
                                [ { mapKey = "app.kubernetes.io/component"
                                  , mapValue = "admission-webhook"
                                  }
                                , { mapKey = "app.kubernetes.io/instance"
                                  , mapValue = "ingress-nginx"
                                  }
                                , { mapKey = "app.kubernetes.io/managed-by"
                                  , mapValue = "Helm"
                                  }
                                , { mapKey = "app.kubernetes.io/name"
                                  , mapValue = "ingress-nginx"
                                  }
                                , { mapKey = "app.kubernetes.io/version"
                                  , mapValue = "0.34.1"
                                  }
                                , { mapKey = "helm.sh/chart"
                                  , mapValue = "ingress-nginx-2.11.1"
                                  }
                                ]
                              , name = Some "ingress-nginx-admission-create"
                              }
                            , spec = Some Kubernetes/PodSpec::{
                              , containers =
                                [ Kubernetes/Container::{
                                  , args = Some
                                    [ "create"
                                    , "--host=ingress-nginx-controller-admission,ingress-nginx-controller-admission.ingress-nginx.svc"
                                    , "--namespace=ingress-nginx"
                                    , "--secret-name=ingress-nginx-admission"
                                    ]
                                  , image = Some
                                      "docker.io/jettech/kube-webhook-certgen:v1.2.2"
                                  , imagePullPolicy = Some "IfNotPresent"
                                  , name = "create"
                                  }
                                ]
                              , restartPolicy = Some "OnFailure"
                              , securityContext = Some Kubernetes/PodSecurityContext::{
                                , runAsNonRoot = Some True
                                , runAsUser = Some 2000
                                }
                              , serviceAccountName = Some
                                  "ingress-nginx-admission"
                              }
                            }
                          }
                        }
                      , ingress-nginx-admission-patch = Kubernetes/Job::{
                        , metadata = Kubernetes/ObjectMeta::{
                          , annotations = Some
                            [ { mapKey = "helm.sh/hook"
                              , mapValue = "post-install,post-upgrade"
                              }
                            , { mapKey = "helm.sh/hook-delete-policy"
                              , mapValue = "before-hook-creation,hook-succeeded"
                              }
                            ]
                          , labels = Some
                            [ { mapKey = "app.kubernetes.io/component"
                              , mapValue = "admission-webhook"
                              }
                            , { mapKey = "app.kubernetes.io/instance"
                              , mapValue = "ingress-nginx"
                              }
                            , { mapKey = "app.kubernetes.io/managed-by"
                              , mapValue = "Helm"
                              }
                            , { mapKey = "app.kubernetes.io/name"
                              , mapValue = "ingress-nginx"
                              }
                            , { mapKey = "app.kubernetes.io/version"
                              , mapValue = "0.34.1"
                              }
                            , { mapKey = "helm.sh/chart"
                              , mapValue = "ingress-nginx-2.11.1"
                              }
                            ]
                          , name = Some "ingress-nginx-admission-patch"
                          , namespace = Some "ingress-nginx"
                          }
                        , spec = Some Kubernetes/JobSpec::{
                          , template = Kubernetes/PodTemplateSpec::{
                            , metadata = Some Kubernetes/ObjectMeta::{
                              , labels = Some
                                [ { mapKey = "app.kubernetes.io/component"
                                  , mapValue = "admission-webhook"
                                  }
                                , { mapKey = "app.kubernetes.io/instance"
                                  , mapValue = "ingress-nginx"
                                  }
                                , { mapKey = "app.kubernetes.io/managed-by"
                                  , mapValue = "Helm"
                                  }
                                , { mapKey = "app.kubernetes.io/name"
                                  , mapValue = "ingress-nginx"
                                  }
                                , { mapKey = "app.kubernetes.io/version"
                                  , mapValue = "0.34.1"
                                  }
                                , { mapKey = "helm.sh/chart"
                                  , mapValue = "ingress-nginx-2.11.1"
                                  }
                                ]
                              , name = Some "ingress-nginx-admission-patch"
                              }
                            , spec = Some Kubernetes/PodSpec::{
                              , containers =
                                [ Kubernetes/Container::{
                                  , args = Some
                                    [ "patch"
                                    , "--webhook-name=ingress-nginx-admission"
                                    , "--namespace=ingress-nginx"
                                    , "--patch-mutating=false"
                                    , "--secret-name=ingress-nginx-admission"
                                    , "--patch-failure-policy=Fail"
                                    ]
                                  , image = Some
                                      "docker.io/jettech/kube-webhook-certgen:v1.2.2"
                                  , imagePullPolicy = Some "IfNotPresent"
                                  , name = "patch"
                                  }
                                ]
                              , restartPolicy = Some "OnFailure"
                              , securityContext = Some Kubernetes/PodSecurityContext::{
                                , runAsNonRoot = Some True
                                , runAsUser = Some 2000
                                }
                              , serviceAccountName = Some
                                  "ingress-nginx-admission"
                              }
                            }
                          }
                        }
                      }
                    , Role.ingress-nginx-admission = Kubernetes/Role::{
                      , metadata = Kubernetes/ObjectMeta::{
                        , annotations = Some
                          [ { mapKey = "helm.sh/hook"
                            , mapValue =
                                "pre-install,pre-upgrade,post-install,post-upgrade"
                            }
                          , { mapKey = "helm.sh/hook-delete-policy"
                            , mapValue = "before-hook-creation,hook-succeeded"
                            }
                          ]
                        , labels = Some
                          [ { mapKey = "app.kubernetes.io/component"
                            , mapValue = "admission-webhook"
                            }
                          , { mapKey = "app.kubernetes.io/instance"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/managed-by"
                            , mapValue = "Helm"
                            }
                          , { mapKey = "app.kubernetes.io/name"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/version"
                            , mapValue = "0.34.1"
                            }
                          , { mapKey = "helm.sh/chart"
                            , mapValue = "ingress-nginx-2.11.1"
                            }
                          ]
                        , name = Some "ingress-nginx-admission"
                        , namespace = Some "ingress-nginx"
                        }
                      , rules = Some
                        [ Kubernetes/PolicyRule::{
                          , apiGroups = Some [ "" ]
                          , resources = Some [ "secrets" ]
                          , verbs = [ "get", "create" ]
                          }
                        ]
                      }
                    , RoleBinding.ingress-nginx-admission = Kubernetes/RoleBinding::{
                      , metadata = Kubernetes/ObjectMeta::{
                        , annotations = Some
                          [ { mapKey = "helm.sh/hook"
                            , mapValue =
                                "pre-install,pre-upgrade,post-install,post-upgrade"
                            }
                          , { mapKey = "helm.sh/hook-delete-policy"
                            , mapValue = "before-hook-creation,hook-succeeded"
                            }
                          ]
                        , labels = Some
                          [ { mapKey = "app.kubernetes.io/component"
                            , mapValue = "admission-webhook"
                            }
                          , { mapKey = "app.kubernetes.io/instance"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/managed-by"
                            , mapValue = "Helm"
                            }
                          , { mapKey = "app.kubernetes.io/name"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/version"
                            , mapValue = "0.34.1"
                            }
                          , { mapKey = "helm.sh/chart"
                            , mapValue = "ingress-nginx-2.11.1"
                            }
                          ]
                        , name = Some "ingress-nginx-admission"
                        , namespace = Some "ingress-nginx"
                        }
                      , roleRef = Kubernetes/RoleRef::{
                        , apiGroup = "rbac.authorization.k8s.io"
                        , kind = "Role"
                        , name = "ingress-nginx-admission"
                        }
                      , subjects = Some
                        [ Kubernetes/Subject::{
                          , kind = "ServiceAccount"
                          , name = "ingress-nginx-admission"
                          , namespace = Some "ingress-nginx"
                          }
                        ]
                      }
                    , ServiceAccount.ingress-nginx-admission = Kubernetes/ServiceAccount::{
                      , metadata = Kubernetes/ObjectMeta::{
                        , annotations = Some
                          [ { mapKey = "helm.sh/hook"
                            , mapValue =
                                "pre-install,pre-upgrade,post-install,post-upgrade"
                            }
                          , { mapKey = "helm.sh/hook-delete-policy"
                            , mapValue = "before-hook-creation,hook-succeeded"
                            }
                          ]
                        , labels = Some
                          [ { mapKey = "app.kubernetes.io/component"
                            , mapValue = "admission-webhook"
                            }
                          , { mapKey = "app.kubernetes.io/instance"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/managed-by"
                            , mapValue = "Helm"
                            }
                          , { mapKey = "app.kubernetes.io/name"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/version"
                            , mapValue = "0.34.1"
                            }
                          , { mapKey = "helm.sh/chart"
                            , mapValue = "ingress-nginx-2.11.1"
                            }
                          ]
                        , name = Some "ingress-nginx-admission"
                        , namespace = Some "ingress-nginx"
                        }
                      }
                    , ValidatingWebhookConfiguration.ingress-nginx-admission = Kubernetes/ValidatingWebhookConfiguration::{
                      , apiVersion = "admissionregistration.k8s.io/v1beta1"
                      , metadata = Kubernetes/ObjectMeta::{
                        , labels = Some
                          [ { mapKey = "app.kubernetes.io/component"
                            , mapValue = "admission-webhook"
                            }
                          , { mapKey = "app.kubernetes.io/instance"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/managed-by"
                            , mapValue = "Helm"
                            }
                          , { mapKey = "app.kubernetes.io/name"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/version"
                            , mapValue = "0.34.1"
                            }
                          , { mapKey = "helm.sh/chart"
                            , mapValue = "ingress-nginx-2.11.1"
                            }
                          ]
                        , name = Some "ingress-nginx-admission"
                        , namespace = Some "ingress-nginx"
                        }
                      , webhooks = Some
                        [ Kubernetes/ValidatingWebhook::{
                          , admissionReviewVersions = [ "v1", "v1beta1" ]
                          , clientConfig = Kubernetes/WebhookClientConfig::{
                            , service = Some
                              { name = "ingress-nginx-controller-admission"
                              , namespace = "ingress-nginx"
                              , path = Some "/extensions/v1beta1/ingresses"
                              , port = None Natural
                              }
                            }
                          , failurePolicy = Some "Fail"
                          , name = "validate.nginx.ingress.kubernetes.io"
                          , rules = Some
                            [ Kubernetes/RuleWithOperations::{
                              , apiGroups = Some
                                [ "extensions", "networking.k8s.io" ]
                              , apiVersions = Some [ "v1beta1" ]
                              , operations = Some [ "CREATE", "UPDATE" ]
                              , resources = Some [ "ingresses" ]
                              }
                            ]
                          , sideEffects = "None"
                          }
                        ]
                      }
                    }
                  , Controller =
                    { ConfigMap.ingress-nginx-controller = Kubernetes/ConfigMap::{
                      , metadata = Kubernetes/ObjectMeta::{
                        , labels = Some
                          [ { mapKey = "app.kubernetes.io/component"
                            , mapValue = "controller"
                            }
                          , { mapKey = "app.kubernetes.io/instance"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/managed-by"
                            , mapValue = "Helm"
                            }
                          , { mapKey = "app.kubernetes.io/name"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/version"
                            , mapValue = "0.34.1"
                            }
                          , { mapKey = "helm.sh/chart"
                            , mapValue = "ingress-nginx-2.11.1"
                            }
                          ]
                        , name = Some "ingress-nginx-controller"
                        , namespace = Some "ingress-nginx"
                        }
                      }
                    , Deployment.ingress-nginx-controller = Kubernetes/Deployment::{
                      , metadata = Kubernetes/ObjectMeta::{
                        , labels = Some
                          [ { mapKey = "app.kubernetes.io/component"
                            , mapValue = "controller"
                            }
                          , { mapKey = "app.kubernetes.io/instance"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/managed-by"
                            , mapValue = "Helm"
                            }
                          , { mapKey = "app.kubernetes.io/name"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/version"
                            , mapValue = "0.34.1"
                            }
                          , { mapKey = "helm.sh/chart"
                            , mapValue = "ingress-nginx-2.11.1"
                            }
                          ]
                        , name = Some "ingress-nginx-controller"
                        , namespace = Some "ingress-nginx"
                        }
                      , spec = Some Kubernetes/DeploymentSpec::{
                        , minReadySeconds = Some 0
                        , revisionHistoryLimit = Some 10
                        , selector = Kubernetes/LabelSelector::{
                          , matchLabels = Some
                            [ { mapKey = "app.kubernetes.io/component"
                              , mapValue = "controller"
                              }
                            , { mapKey = "app.kubernetes.io/instance"
                              , mapValue = "ingress-nginx"
                              }
                            , { mapKey = "app.kubernetes.io/name"
                              , mapValue = "ingress-nginx"
                              }
                            ]
                          }
                        , template = Kubernetes/PodTemplateSpec::{
                          , metadata = Some Kubernetes/ObjectMeta::{
                            , labels = Some
                              [ { mapKey = "app.kubernetes.io/component"
                                , mapValue = "controller"
                                }
                              , { mapKey = "app.kubernetes.io/instance"
                                , mapValue = "ingress-nginx"
                                }
                              , { mapKey = "app.kubernetes.io/name"
                                , mapValue = "ingress-nginx"
                                }
                              ]
                            }
                          , spec = Some Kubernetes/PodSpec::{
                            , containers =
                              [ Kubernetes/Container::{
                                , args = Some
                                  [ "/nginx-ingress-controller"
                                  , "--publish-service=ingress-nginx/ingress-nginx-controller"
                                  , "--election-id=ingress-controller-leader"
                                  , "--ingress-class=nginx"
                                  , "--configmap=ingress-nginx/ingress-nginx-controller"
                                  , "--validating-webhook=:8443"
                                  , "--validating-webhook-certificate=/usr/local/certificates/cert"
                                  , "--validating-webhook-key=/usr/local/certificates/key"
                                  ]
                                , env = Some
                                  [ Kubernetes/EnvVar::{
                                    , name = "POD_NAME"
                                    , valueFrom = Some Kubernetes/EnvVarSource::{
                                      , fieldRef = Some Kubernetes/ObjectFieldSelector::{
                                        , fieldPath = "metadata.name"
                                        }
                                      }
                                    }
                                  , Kubernetes/EnvVar::{
                                    , name = "POD_NAMESPACE"
                                    , valueFrom = Some Kubernetes/EnvVarSource::{
                                      , fieldRef = Some Kubernetes/ObjectFieldSelector::{
                                        , fieldPath = "metadata.namespace"
                                        }
                                      }
                                    }
                                  ]
                                , image = Some
                                    "us.gcr.io/k8s-artifacts-prod/ingress-nginx/controller:v0.34.1@sha256:0e072dddd1f7f8fc8909a2ca6f65e76c5f0d2fcfb8be47935ae3457e8bbceb20"
                                , imagePullPolicy = Some "IfNotPresent"
                                , lifecycle = Some Kubernetes/Lifecycle::{
                                  , preStop = Some Kubernetes/Handler::{
                                    , exec = Some Kubernetes/ExecAction::{
                                      , command = Some [ "/wait-shutdown" ]
                                      }
                                    }
                                  }
                                , livenessProbe = Some Kubernetes/Probe::{
                                  , failureThreshold = Some 5
                                  , httpGet = Some Kubernetes/HTTPGetAction::{
                                    , path = Some "/healthz"
                                    , port =
                                        < Nat : Natural | String : Text >.Nat
                                          10254
                                    , scheme = Some "HTTP"
                                    }
                                  , initialDelaySeconds = Some 10
                                  , periodSeconds = Some 10
                                  , successThreshold = Some 1
                                  , timeoutSeconds = Some 1
                                  }
                                , name = "controller"
                                , ports = Some
                                  [ Kubernetes/ContainerPort::{
                                    , containerPort = 80
                                    , name = Some "http"
                                    , protocol = Some "TCP"
                                    }
                                  , Kubernetes/ContainerPort::{
                                    , containerPort = 443
                                    , name = Some "https"
                                    , protocol = Some "TCP"
                                    }
                                  , Kubernetes/ContainerPort::{
                                    , containerPort = 8443
                                    , name = Some "webhook"
                                    , protocol = Some "TCP"
                                    }
                                  ]
                                , readinessProbe = Some Kubernetes/Probe::{
                                  , failureThreshold = Some 3
                                  , httpGet = Some Kubernetes/HTTPGetAction::{
                                    , path = Some "/healthz"
                                    , port =
                                        < Nat : Natural | String : Text >.Nat
                                          10254
                                    , scheme = Some "HTTP"
                                    }
                                  , initialDelaySeconds = Some 10
                                  , periodSeconds = Some 10
                                  , successThreshold = Some 1
                                  , timeoutSeconds = Some 1
                                  }
                                , resources = Some Kubernetes/ResourceRequirements::{
                                  , requests = Some
                                    [ { mapKey = "cpu", mapValue = "100m" }
                                    , { mapKey = "memory", mapValue = "90Mi" }
                                    ]
                                  }
                                , securityContext = Some Kubernetes/SecurityContext::{
                                  , allowPrivilegeEscalation = Some True
                                  , capabilities = Some Kubernetes/Capabilities::{
                                    , add = Some [ "NET_BIND_SERVICE" ]
                                    , drop = Some [ "ALL" ]
                                    }
                                  , runAsUser = Some 101
                                  }
                                , volumeMounts = Some
                                  [ Kubernetes/VolumeMount::{
                                    , mountPath = "/usr/local/certificates/"
                                    , name = "webhook-cert"
                                    , readOnly = Some True
                                    }
                                  ]
                                }
                              ]
                            , dnsPolicy = Some "ClusterFirst"
                            , serviceAccountName = Some "ingress-nginx"
                            , terminationGracePeriodSeconds = Some 300
                            , volumes = Some
                              [ Kubernetes/Volume::{
                                , name = "webhook-cert"
                                , secret = Some Kubernetes/SecretVolumeSource::{
                                  , secretName = Some "ingress-nginx-admission"
                                  }
                                }
                              ]
                            }
                          }
                        }
                      }
                    , Role.ingress-nginx = Kubernetes/Role::{
                      , metadata = Kubernetes/ObjectMeta::{
                        , labels = Some
                          [ { mapKey = "app.kubernetes.io/component"
                            , mapValue = "controller"
                            }
                          , { mapKey = "app.kubernetes.io/instance"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/managed-by"
                            , mapValue = "Helm"
                            }
                          , { mapKey = "app.kubernetes.io/name"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/version"
                            , mapValue = "0.34.1"
                            }
                          , { mapKey = "helm.sh/chart"
                            , mapValue = "ingress-nginx-2.11.1"
                            }
                          ]
                        , name = Some "ingress-nginx"
                        , namespace = Some "ingress-nginx"
                        }
                      , rules = Some
                        [ Kubernetes/PolicyRule::{
                          , apiGroups = Some [ "" ]
                          , resources = Some [ "namespaces" ]
                          , verbs = [ "get" ]
                          }
                        , Kubernetes/PolicyRule::{
                          , apiGroups = Some [ "" ]
                          , resources = Some
                            [ "configmaps", "pods", "secrets", "endpoints" ]
                          , verbs = [ "get", "list", "watch" ]
                          }
                        , Kubernetes/PolicyRule::{
                          , apiGroups = Some [ "" ]
                          , resources = Some [ "services" ]
                          , verbs = [ "get", "list", "update", "watch" ]
                          }
                        , Kubernetes/PolicyRule::{
                          , apiGroups = Some
                            [ "extensions", "networking.k8s.io" ]
                          , resources = Some [ "ingresses" ]
                          , verbs = [ "get", "list", "watch" ]
                          }
                        , Kubernetes/PolicyRule::{
                          , apiGroups = Some
                            [ "extensions", "networking.k8s.io" ]
                          , resources = Some [ "ingresses/status" ]
                          , verbs = [ "update" ]
                          }
                        , Kubernetes/PolicyRule::{
                          , apiGroups = Some [ "networking.k8s.io" ]
                          , resources = Some [ "ingressclasses" ]
                          , verbs = [ "get", "list", "watch" ]
                          }
                        , Kubernetes/PolicyRule::{
                          , apiGroups = Some [ "" ]
                          , resourceNames = Some
                            [ "ingress-controller-leader-nginx" ]
                          , resources = Some [ "configmaps" ]
                          , verbs = [ "get", "update" ]
                          }
                        , Kubernetes/PolicyRule::{
                          , apiGroups = Some [ "" ]
                          , resources = Some [ "configmaps" ]
                          , verbs = [ "create" ]
                          }
                        , Kubernetes/PolicyRule::{
                          , apiGroups = Some [ "" ]
                          , resources = Some [ "endpoints" ]
                          , verbs = [ "create", "get", "update" ]
                          }
                        , Kubernetes/PolicyRule::{
                          , apiGroups = Some [ "" ]
                          , resources = Some [ "events" ]
                          , verbs = [ "create", "patch" ]
                          }
                        ]
                      }
                    , RoleBinding.ingress-nginx = Kubernetes/RoleBinding::{
                      , metadata = Kubernetes/ObjectMeta::{
                        , labels = Some
                          [ { mapKey = "app.kubernetes.io/component"
                            , mapValue = "controller"
                            }
                          , { mapKey = "app.kubernetes.io/instance"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/managed-by"
                            , mapValue = "Helm"
                            }
                          , { mapKey = "app.kubernetes.io/name"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/version"
                            , mapValue = "0.34.1"
                            }
                          , { mapKey = "helm.sh/chart"
                            , mapValue = "ingress-nginx-2.11.1"
                            }
                          ]
                        , name = Some "ingress-nginx"
                        , namespace = Some "ingress-nginx"
                        }
                      , roleRef = Kubernetes/RoleRef::{
                        , apiGroup = "rbac.authorization.k8s.io"
                        , kind = "Role"
                        , name = "ingress-nginx"
                        }
                      , subjects = Some
                        [ Kubernetes/Subject::{
                          , kind = "ServiceAccount"
                          , name = "ingress-nginx"
                          , namespace = Some "ingress-nginx"
                          }
                        ]
                      }
                    , Service =
                      { ingress-nginx-controller = Kubernetes/Service::{
                        , metadata = Kubernetes/ObjectMeta::{
                          , labels = Some
                            [ { mapKey = "app.kubernetes.io/component"
                              , mapValue = "controller"
                              }
                            , { mapKey = "app.kubernetes.io/instance"
                              , mapValue = "ingress-nginx"
                              }
                            , { mapKey = "app.kubernetes.io/managed-by"
                              , mapValue = "Helm"
                              }
                            , { mapKey = "app.kubernetes.io/name"
                              , mapValue = "ingress-nginx"
                              }
                            , { mapKey = "app.kubernetes.io/version"
                              , mapValue = "0.34.1"
                              }
                            , { mapKey = "helm.sh/chart"
                              , mapValue = "ingress-nginx-2.11.1"
                              }
                            ]
                          , name = Some "ingress-nginx-controller"
                          , namespace = Some "ingress-nginx"
                          }
                        , spec = Some Kubernetes/ServiceSpec::{
                          , externalTrafficPolicy = Some "Local"
                          , ports = Some
                            [ Kubernetes/ServicePort::{
                              , name = Some "http"
                              , port = 80
                              , protocol = Some "TCP"
                              , targetPort = Some
                                  ( < Nat : Natural | String : Text >.String
                                      "http"
                                  )
                              }
                            , Kubernetes/ServicePort::{
                              , name = Some "https"
                              , port = 443
                              , protocol = Some "TCP"
                              , targetPort = Some
                                  ( < Nat : Natural | String : Text >.String
                                      "https"
                                  )
                              }
                            ]
                          , selector = Some
                            [ { mapKey = "app.kubernetes.io/component"
                              , mapValue = "controller"
                              }
                            , { mapKey = "app.kubernetes.io/instance"
                              , mapValue = "ingress-nginx"
                              }
                            , { mapKey = "app.kubernetes.io/name"
                              , mapValue = "ingress-nginx"
                              }
                            ]
                          , type = Some "LoadBalancer"
                          , loadBalancerIP = IPAddress
                          }
                        }
                      , ingress-nginx-controller-admission = Kubernetes/Service::{
                        , metadata = Kubernetes/ObjectMeta::{
                          , labels = Some
                            [ { mapKey = "app.kubernetes.io/component"
                              , mapValue = "controller"
                              }
                            , { mapKey = "app.kubernetes.io/instance"
                              , mapValue = "ingress-nginx"
                              }
                            , { mapKey = "app.kubernetes.io/managed-by"
                              , mapValue = "Helm"
                              }
                            , { mapKey = "app.kubernetes.io/name"
                              , mapValue = "ingress-nginx"
                              }
                            , { mapKey = "app.kubernetes.io/version"
                              , mapValue = "0.34.1"
                              }
                            , { mapKey = "helm.sh/chart"
                              , mapValue = "ingress-nginx-2.11.1"
                              }
                            ]
                          , name = Some "ingress-nginx-controller-admission"
                          , namespace = Some "ingress-nginx"
                          }
                        , spec = Some Kubernetes/ServiceSpec::{
                          , ports = Some
                            [ Kubernetes/ServicePort::{
                              , name = Some "https-webhook"
                              , port = 443
                              , targetPort = Some
                                  ( < Nat : Natural | String : Text >.String
                                      "webhook"
                                  )
                              }
                            ]
                          , selector = Some
                            [ { mapKey = "app.kubernetes.io/component"
                              , mapValue = "controller"
                              }
                            , { mapKey = "app.kubernetes.io/instance"
                              , mapValue = "ingress-nginx"
                              }
                            , { mapKey = "app.kubernetes.io/name"
                              , mapValue = "ingress-nginx"
                              }
                            ]
                          , type = Some "ClusterIP"
                          }
                        }
                      }
                    , ServiceAccount.ingress-nginx = Kubernetes/ServiceAccount::{
                      , metadata = Kubernetes/ObjectMeta::{
                        , labels = Some
                          [ { mapKey = "app.kubernetes.io/component"
                            , mapValue = "controller"
                            }
                          , { mapKey = "app.kubernetes.io/instance"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/managed-by"
                            , mapValue = "Helm"
                            }
                          , { mapKey = "app.kubernetes.io/name"
                            , mapValue = "ingress-nginx"
                            }
                          , { mapKey = "app.kubernetes.io/version"
                            , mapValue = "0.34.1"
                            }
                          , { mapKey = "helm.sh/chart"
                            , mapValue = "ingress-nginx-2.11.1"
                            }
                          ]
                        , name = Some "ingress-nginx"
                        , namespace = Some "ingress-nginx"
                        }
                      }
                    }
                  , ClusterRole.ingress-nginx = Kubernetes/ClusterRole::{
                    , metadata = Kubernetes/ObjectMeta::{
                      , labels = Some
                        [ { mapKey = "app.kubernetes.io/instance"
                          , mapValue = "ingress-nginx"
                          }
                        , { mapKey = "app.kubernetes.io/managed-by"
                          , mapValue = "Helm"
                          }
                        , { mapKey = "app.kubernetes.io/name"
                          , mapValue = "ingress-nginx"
                          }
                        , { mapKey = "app.kubernetes.io/version"
                          , mapValue = "0.34.1"
                          }
                        , { mapKey = "helm.sh/chart"
                          , mapValue = "ingress-nginx-2.11.1"
                          }
                        ]
                      , name = Some "ingress-nginx"
                      , namespace = Some "ingress-nginx"
                      }
                    , rules = Some
                      [ Kubernetes/PolicyRule::{
                        , apiGroups = Some [ "" ]
                        , resources = Some
                          [ "configmaps"
                          , "endpoints"
                          , "nodes"
                          , "pods"
                          , "secrets"
                          ]
                        , verbs = [ "list", "watch" ]
                        }
                      , Kubernetes/PolicyRule::{
                        , apiGroups = Some [ "" ]
                        , resources = Some [ "nodes" ]
                        , verbs = [ "get" ]
                        }
                      , Kubernetes/PolicyRule::{
                        , apiGroups = Some [ "" ]
                        , resources = Some [ "services" ]
                        , verbs = [ "get", "list", "update", "watch" ]
                        }
                      , Kubernetes/PolicyRule::{
                        , apiGroups = Some [ "extensions", "networking.k8s.io" ]
                        , resources = Some [ "ingresses" ]
                        , verbs = [ "get", "list", "watch" ]
                        }
                      , Kubernetes/PolicyRule::{
                        , apiGroups = Some [ "" ]
                        , resources = Some [ "events" ]
                        , verbs = [ "create", "patch" ]
                        }
                      , Kubernetes/PolicyRule::{
                        , apiGroups = Some [ "extensions", "networking.k8s.io" ]
                        , resources = Some [ "ingresses/status" ]
                        , verbs = [ "update" ]
                        }
                      , Kubernetes/PolicyRule::{
                        , apiGroups = Some [ "networking.k8s.io" ]
                        , resources = Some [ "ingressclasses" ]
                        , verbs = [ "get", "list", "watch" ]
                        }
                      ]
                    }
                  , ClusterRoleBinding.ingress-nginx = Kubernetes/RoleBinding::{
                    , kind = "ClusterRoleBinding"
                    , metadata = Kubernetes/ObjectMeta::{
                      , labels = Some
                        [ { mapKey = "app.kubernetes.io/instance"
                          , mapValue = "ingress-nginx"
                          }
                        , { mapKey = "app.kubernetes.io/managed-by"
                          , mapValue = "Helm"
                          }
                        , { mapKey = "app.kubernetes.io/name"
                          , mapValue = "ingress-nginx"
                          }
                        , { mapKey = "app.kubernetes.io/version"
                          , mapValue = "0.34.1"
                          }
                        , { mapKey = "helm.sh/chart"
                          , mapValue = "ingress-nginx-2.11.1"
                          }
                        ]
                      , name = Some "ingress-nginx"
                      , namespace = Some "ingress-nginx"
                      }
                    , roleRef = Kubernetes/RoleRef::{
                      , apiGroup = "rbac.authorization.k8s.io"
                      , kind = "ClusterRole"
                      , name = "ingress-nginx"
                      }
                    , subjects = Some
                      [ Kubernetes/Subject::{
                        , kind = "ServiceAccount"
                        , name = "ingress-nginx"
                        , namespace = Some "ingress-nginx"
                        }
                      ]
                    }
                  , Namespace.ingress-nginx = Kubernetes/Namespace::{
                    , metadata = Kubernetes/ObjectMeta::{
                      , labels = Some
                        [ { mapKey = "app.kubernetes.io/instance"
                          , mapValue = "ingress-nginx"
                          }
                        , { mapKey = "app.kubernetes.io/name"
                          , mapValue = "ingress-nginx"
                          }
                        ]
                      , name = Some "ingress-nginx"
                      }
                    }
                  }

            in  if overrides.Enabled then Some ingressNginx else None shape
        )
      : ∀(c : Configuration/global.Type) → component

in  Generate
