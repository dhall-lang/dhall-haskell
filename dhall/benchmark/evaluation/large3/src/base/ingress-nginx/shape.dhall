let Kubernetes/Job = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.batch.v1.Job.dhall

let Kubernetes/ClusterRole =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.ClusterRole.dhall

let Kubernetes/ClusterRoleBinding =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.ClusterRoleBinding.dhall

let Kubernetes/Role = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.Role.dhall

let Kubernetes/RoleBinding =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.rbac.v1.RoleBinding.dhall

let Kubernetes/ValidatingWebhookConfiguration =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.admissionregistration.v1.ValidatingWebhookConfiguration.dhall

let Kubernetes/ConfigMap =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ConfigMap.dhall

let Kubernetes/Deployment =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.apps.v1.Deployment.dhall

let Kubernetes/Service = ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Service.dhall

let Kubernetes/ServiceAccount =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.ServiceAccount.dhall

let Kubernetes/Namespace =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Namespace.dhall

let shape =
      { AdmissionWebhook :
          { ClusterRole :
              { ingress-nginx-admission : Kubernetes/ClusterRole.Type }
          , ClusterRoleBinding :
              { ingress-nginx-admission : Kubernetes/ClusterRoleBinding.Type }
          , Job :
              { ingress-nginx-admission-create : Kubernetes/Job.Type
              , ingress-nginx-admission-patch : Kubernetes/Job.Type
              }
          , Role : { ingress-nginx-admission : Kubernetes/Role.Type }
          , RoleBinding :
              { ingress-nginx-admission : Kubernetes/RoleBinding.Type }
          , ServiceAccount :
              { ingress-nginx-admission : Kubernetes/ServiceAccount.Type }
          , ValidatingWebhookConfiguration :
              { ingress-nginx-admission :
                  Kubernetes/ValidatingWebhookConfiguration.Type
              }
          }
      , Controller :
          { ConfigMap : { ingress-nginx-controller : Kubernetes/ConfigMap.Type }
          , Deployment :
              { ingress-nginx-controller : Kubernetes/Deployment.Type }
          , Role : { ingress-nginx : Kubernetes/Role.Type }
          , RoleBinding : { ingress-nginx : Kubernetes/RoleBinding.Type }
          , Service :
              { ingress-nginx-controller : Kubernetes/Service.Type
              , ingress-nginx-controller-admission : Kubernetes/Service.Type
              }
          , ServiceAccount : { ingress-nginx : Kubernetes/ServiceAccount.Type }
          }
      , ClusterRole : { ingress-nginx : Kubernetes/ClusterRole.Type }
      , ClusterRoleBinding :
          { ingress-nginx : Kubernetes/ClusterRoleBinding.Type }
      , Namespace : { ingress-nginx : Kubernetes/Namespace.Type }
      }

in  shape
