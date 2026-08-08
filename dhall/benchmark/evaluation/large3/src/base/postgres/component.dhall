let Kubernetes/ConfigMap =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ConfigMap.dhall

let Kubernetes/Deployment =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.Deployment.dhall

let Kubernetes/PersistentVolumeClaim =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.PersistentVolumeClaim.dhall

let Kubernetes/Service = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Service.dhall

let component =
      { ConfigMap : Kubernetes/ConfigMap.Type
      , Deployment : Kubernetes/Deployment.Type
      , PersistentVolumeClaim : Kubernetes/PersistentVolumeClaim.Type
      , Service : Kubernetes/Service.Type
      }

in  component
