let Kubernetes/List = ../../util/kubernetes-list.dhall

let Kubernetes/TypesUnion = ../../../../k8s/dhall-kubernetes/1.17/typesUnion.dhall

let Configuration/global = ../../configuration/global.dhall

let component = ./component.dhall

let Generate = ./generate.dhall

let ToList =
        ( λ(c : component) →
            Kubernetes/List::{
            , items =
              [ Kubernetes/TypesUnion.Deployment c.Deployment
              , Kubernetes/TypesUnion.Service c.Collector
              , Kubernetes/TypesUnion.Service c.Query
              ]
            }
        )
      : ∀(c : component) → Kubernetes/List.Type

let Render =
        (λ(c : Configuration/global.Type) → ToList (Generate c))
      : ∀(c : Configuration/global.Type) → Kubernetes/List.Type

in  Render
