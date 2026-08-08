let Kubernetes/List = ../../util/kubernetes-list.dhall

let Kubernetes/TypesUnion = ../../../../k8s/dhall-kubernetes/1.17/typesUnion.dhall

let Configuration/global = ../../configuration/global.dhall

let component = ./component.dhall

let Generate = ./generate.dhall

let ToList =
        ( λ(c : component) →
            Kubernetes/List::{
            , items =
              [ Kubernetes/TypesUnion.Deployment c.Cache.Deployment
              , Kubernetes/TypesUnion.Service c.Cache.Service
              , Kubernetes/TypesUnion.PersistentVolumeClaim
                  c.Cache.PersistentVolumeClaim
              , Kubernetes/TypesUnion.Deployment c.Store.Deployment
              , Kubernetes/TypesUnion.Service c.Store.Service
              , Kubernetes/TypesUnion.PersistentVolumeClaim
                  c.Store.PersistentVolumeClaim
              ]
            }
        )
      : ∀(c : component) → Kubernetes/List.Type

let Render =
        (λ(c : Configuration/global.Type) → ToList (Generate c))
      : ∀(c : Configuration/global.Type) → Kubernetes/List.Type

in  Render
