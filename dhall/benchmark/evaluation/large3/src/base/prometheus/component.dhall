let Kubernetes/ServiceAccount =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ServiceAccount.dhall

let Kubernetes/ConfigMap =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ConfigMap.dhall

let Kubernetes/Deployment =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.Deployment.dhall

let Kubernetes/PersistentVolumeClaim =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.PersistentVolumeClaim.dhall

let Kubernetes/ClusterRole =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.ClusterRole.dhall

let Kubernetes/ClusterRoleBinding =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.ClusterRoleBinding.dhall

let Kubernetes/Service = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Service.dhall

let component =
      { Deployment : Kubernetes/Deployment.Type
      , ClusterRole : Kubernetes/ClusterRole.Type
      , ConfigMap : Kubernetes/ConfigMap.Type
      , PersistentVolumeClaim : Kubernetes/PersistentVolumeClaim.Type
      , ClusterRoleBinding : Kubernetes/ClusterRoleBinding.Type
      , Service : Kubernetes/Service.Type
      , ServiceAccount : Kubernetes/ServiceAccount.Type
      }

in  component
