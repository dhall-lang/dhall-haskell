let Kubernetes/Deployment =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.Deployment.dhall

let Kubernetes/Service = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Service.dhall

let Kubernetes/PersistentVolumeClaim =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.PersistentVolumeClaim.dhall

let component =
      { BundleManager :
          { Deployment : Kubernetes/Deployment.Type
          , Service : Kubernetes/Service.Type
          , PersistentVolumeClaim : Kubernetes/PersistentVolumeClaim.Type
          }
      , Worker :
          { Deployment : Kubernetes/Deployment.Type
          , Service : Kubernetes/Service.Type
          }
      }

in  component
