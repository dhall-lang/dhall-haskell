let Kubernetes/Deployment =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.Deployment.dhall

let Kubernetes/Ingress =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.networking.v1beta1.Ingress.dhall

let Kubernetes/Role = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.Role.dhall

let Kubernetes/RoleBinding =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.RoleBinding.dhall

let Kubernetes/Service = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Service.dhall

let Kubernetes/ServiceAccount =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ServiceAccount.dhall

let component =
      { Deployment : Kubernetes/Deployment.Type
      , Ingress : Kubernetes/Ingress.Type
      , Role : Kubernetes/Role.Type
      , RoleBinding : Kubernetes/RoleBinding.Type
      , Service : Kubernetes/Service.Type
      , ServiceAccount : Kubernetes/ServiceAccount.Type
      , ServiceInternal : Kubernetes/Service.Type
      }

in  component
