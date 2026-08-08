let Kubernetes/Container =
      ../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Container.dhall

let Kubernetes/ContainerPort =
      ../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ContainerPort.dhall

let Kubernetes/EnvVar = ../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.EnvVar.dhall

let Kubernetes/EnvVarSource =
      ../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.EnvVarSource.dhall

in  Kubernetes/Container::{
    , name = "jaeger-agent"
    , image = Some
        "index.docker.io/sourcegraph/jaeger-agent:3.17.2@sha256:a29258e098c7d23392411abd359563afdd89529e9852ce1ba73f80188a72fd5c"
    , args = Some
      [ "--reporter.grpc.host-port=jaeger-collector:14250"
      , "--reporter.type=grpc"
      ]
    , env = Some
      [ Kubernetes/EnvVar::{
        , name = "POD_NAME"
        , valueFrom = Some Kubernetes/EnvVarSource::{
          , fieldRef = Some
            { apiVersion = Some "v1", fieldPath = "metadata.name" }
          }
        }
      ]
    , ports = Some
      [ Kubernetes/ContainerPort::{
        , containerPort = 5775
        , protocol = Some "UDP"
        }
      , Kubernetes/ContainerPort::{
        , containerPort = 5778
        , protocol = Some "TCP"
        }
      , Kubernetes/ContainerPort::{
        , containerPort = 6831
        , protocol = Some "UDP"
        }
      , Kubernetes/ContainerPort::{
        , containerPort = 6832
        , protocol = Some "UDP"
        }
      ]
    , resources = Some
      { limits = Some
        [ { mapKey = "cpu", mapValue = "1" }
        , { mapKey = "memory", mapValue = "500M" }
        ]
      , requests = Some
        [ { mapKey = "cpu", mapValue = "100m" }
        , { mapKey = "memory", mapValue = "100M" }
        ]
      }
    }
