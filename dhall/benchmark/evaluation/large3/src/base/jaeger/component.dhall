let Kubernetes/Deployment =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.Deployment.dhall

let Kubernetes/Service = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Service.dhall

let component =
      { Deployment : Kubernetes/Deployment.Type
      , Collector : Kubernetes/Service.Type
      , Query : Kubernetes/Service.Type
      }

in  component
