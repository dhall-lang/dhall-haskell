let name = "ipfs"

let labels =
      { `app.kubernetes.io/name` = name
      , `app.kubernetes.io/instance` = "wintering-rodent"
      , `app.kubernetes.io/version` = "0.4.0"
      , `app.kubernetes.io/managed-by` = "dhall"
      }

let matchLabels =
      labels.{ `app.kubernetes.io/name`, `app.kubernetes.io/instance` }

let k8s =
      https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/4ab28225a150498aef67c226d3c5f026c95b5a1e/package.dhall sha256:2c7ac35494f16b1f39afcf3467b2f3b0ab579edb0c711cddd2c93f1cbed358bd

let serviceName = "ipfs"

let apiPort = k8s.IntOrString.Int 5001

let gatewayPort = k8s.IntOrString.Int 8080

let toRule =
      λ ( args
        : { host : Text
          , path : Text
          , serviceName : Text
          , servicePort : k8s.IntOrString
          }
        ) →
        k8s.IngressRule::{
        , host = Some args.host
        , http = Some k8s.HTTPIngressRuleValue::{
          , paths =
            [ k8s.HTTPIngressPath::{
              , path = Some args.path
              , backend = k8s.IngressBackend::args.{ serviceName, servicePort }
              }
            ]
          }
        }

in  [ k8s.Resource.Ingress
        k8s.Ingress::{
        , metadata = k8s.ObjectMeta::{
          , labels = toMap labels
          , name = "${name}-api"
          }
        , spec = Some k8s.IngressSpec::{
          , rules =
            [ toRule
                { host = "localhost"
                , path = "/"
                , serviceName
                , servicePort = gatewayPort
                }
            , toRule
                { host = "localhost"
                , path = "/"
                , serviceName
                , servicePort = apiPort
                }
            ]
          }
        }
    , k8s.Resource.Service
        k8s.Service::{
        , metadata = k8s.ObjectMeta::{
          , name = serviceName
          , labels = toMap labels
          }
        , spec = Some k8s.ServiceSpec::{
          , ports =
            [ k8s.ServicePort::{
              , port = 5001
              , targetPort = Some apiPort
              , name = Some "api"
              }
            , k8s.ServicePort::{
              , port = 8080
              , targetPort = Some gatewayPort
              , name = Some "api"
              }
            ]
          , selector = toMap matchLabels
          }
        }
    , k8s.Resource.StatefulSet
        k8s.StatefulSet::{
        , metadata = k8s.ObjectMeta::{ name, labels = toMap labels }
        , spec = Some k8s.StatefulSetSpec::{
          , serviceName
          , selector = k8s.LabelSelector::{ matchLabels = toMap matchLabels }
          , template = k8s.PodTemplateSpec::{
            , metadata = k8s.ObjectMeta::{ name, labels = toMap labels }
            , spec = Some k8s.PodSpec::{
              , securityContext = Some k8s.PodSecurityContext::{
                , runAsUser = Some 1000
                , runAsGroup = Some 1000
                , fsGroup = Some 1000
                }
              , containers =
                [ k8s.Container::{
                  , name
                  , image = Some "ipfs/go-ipfs:v0.4.22"
                  , livenessProbe = k8s.Probe::{
                    , httpGet = Some k8s.HTTPGetAction::{
                      , path = Some "/debug/metrics/prometheus"
                      , port = k8s.IntOrString.String "api"
                      }
                    , initialDelaySeconds = Some 15
                    , periodSeconds = Some 3
                    }
                  , readinessProbe = k8s.Probe::{
                    , httpGet = Some k8s.HTTPGetAction::{
                      , path = Some "/debug/metrics/prometheus"
                      , port = k8s.IntOrString.String "api"
                      }
                    , initialDelaySeconds = Some 15
                    , periodSeconds = Some 3
                    }
                  , ports =
                    [ k8s.ContainerPort::{
                      , containerPort = 5001
                      , name = Some "api"
                      }
                    , k8s.ContainerPort::{
                      , containerPort = 8080
                      , name = Some "gateway"
                      }
                    ]
                  , volumeMounts =
                    [ k8s.VolumeMount::{
                      , name = "ipfs-storage"
                      , mountPath = "/data/ipfs"
                      }
                    ]
                  }
                ]
              }
            }
          }
        }
    ]
