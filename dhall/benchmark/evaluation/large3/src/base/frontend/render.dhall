let Kubernetes/List = ../../util/kubernetes-list.dhall

let Kubernetes/TypesUnion = ../../../../k8s/dhall-kubernetes/1.17/typesUnion.dhall

let Configuration/global = ../../configuration/global.dhall

let Component = ./component.dhall

let Generate = ./generate.dhall

let ToList =
        ( λ(c : Component) →
            Kubernetes/List::{
            , items =
              [ Kubernetes/TypesUnion.Deployment c.Deployment
              , Kubernetes/TypesUnion.Ingress c.Ingress
              , Kubernetes/TypesUnion.Role c.Role
              , Kubernetes/TypesUnion.RoleBinding c.RoleBinding
              , Kubernetes/TypesUnion.Service c.Service
              , Kubernetes/TypesUnion.ServiceAccount c.ServiceAccount
              , Kubernetes/TypesUnion.Service c.ServiceInternal
              ]
            }
        )
      : ∀(c : Component) → Kubernetes/List.Type

let Render =
        (λ(c : Configuration/global.Type) → ToList (Generate c))
      : ∀(c : Configuration/global.Type) → Kubernetes/List.Type

in  Render
