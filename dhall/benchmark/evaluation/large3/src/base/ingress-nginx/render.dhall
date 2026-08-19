let Kubernetes/List = ../../util/kubernetes-list.dhall

let Kubernetes/TypesUnion = ../../../../k8s/dhall-kubernetes/1.17/typesUnion.dhall

let Configuration/global = ../../configuration/global.dhall

let component = ./component.dhall

let shape = ./shape.dhall

let Generate = ./generate.dhall

let ToList =
        ( λ(c : component) →
            let items =
                  merge
                    { Some =
                        λ(ig : shape) →
                          [ Kubernetes/TypesUnion.ClusterRole
                              ig.AdmissionWebhook.ClusterRole.ingress-nginx-admission
                          , Kubernetes/TypesUnion.ClusterRoleBinding
                              ig.AdmissionWebhook.ClusterRoleBinding.ingress-nginx-admission
                          , Kubernetes/TypesUnion.Job
                              ig.AdmissionWebhook.Job.ingress-nginx-admission-create
                          , Kubernetes/TypesUnion.Job
                              ig.AdmissionWebhook.Job.ingress-nginx-admission-patch
                          , Kubernetes/TypesUnion.Role
                              ig.AdmissionWebhook.Role.ingress-nginx-admission
                          , Kubernetes/TypesUnion.RoleBinding
                              ig.AdmissionWebhook.RoleBinding.ingress-nginx-admission
                          , Kubernetes/TypesUnion.ServiceAccount
                              ig.AdmissionWebhook.ServiceAccount.ingress-nginx-admission
                          , Kubernetes/TypesUnion.ValidatingWebhookConfiguration
                              ig.AdmissionWebhook.ValidatingWebhookConfiguration.ingress-nginx-admission
                          , Kubernetes/TypesUnion.ConfigMap
                              ig.Controller.ConfigMap.ingress-nginx-controller
                          , Kubernetes/TypesUnion.Deployment
                              ig.Controller.Deployment.ingress-nginx-controller
                          , Kubernetes/TypesUnion.Role
                              ig.Controller.Role.ingress-nginx
                          , Kubernetes/TypesUnion.RoleBinding
                              ig.Controller.RoleBinding.ingress-nginx
                          , Kubernetes/TypesUnion.Service
                              ig.Controller.Service.ingress-nginx-controller
                          , Kubernetes/TypesUnion.Service
                              ig.Controller.Service.ingress-nginx-controller-admission
                          , Kubernetes/TypesUnion.ServiceAccount
                              ig.Controller.ServiceAccount.ingress-nginx
                          , Kubernetes/TypesUnion.ClusterRole
                              ig.ClusterRole.ingress-nginx
                          , Kubernetes/TypesUnion.ClusterRoleBinding
                              ig.ClusterRoleBinding.ingress-nginx
                          , Kubernetes/TypesUnion.Namespace
                              ig.Namespace.ingress-nginx
                          ]
                    , None = [] : List Kubernetes/TypesUnion
                    }
                    c

            in  Kubernetes/List::{ items }
        )
      : ∀(c : component) → Kubernetes/List.Type

let Render =
        (λ(c : Configuration/global.Type) → ToList (Generate c))
      : ∀(c : Configuration/global.Type) → Kubernetes/List.Type

in  Render
