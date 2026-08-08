let Kubernetes/List = ../../util/kubernetes-list.dhall

let Kubernetes/TypesUnion = ../../../../k8s/dhall-kubernetes/1.17/typesUnion.dhall

let Configuration/global = ../../configuration/global.dhall

let component = ./component.dhall

let Generate = ./generate.dhall

let ToList =
        ( λ(c : component) →
            Kubernetes/List::{
            , items =
              [ Kubernetes/TypesUnion.Deployment c.BundleManager.Deployment
              , Kubernetes/TypesUnion.Service c.BundleManager.Service
              , Kubernetes/TypesUnion.PersistentVolumeClaim
                  c.BundleManager.PersistentVolumeClaim
              , Kubernetes/TypesUnion.Deployment c.Worker.Deployment
              , Kubernetes/TypesUnion.Service c.Worker.Service
              ]
            }
        )
      : ∀(c : component) → Kubernetes/List.Type

let Render =
        (λ(c : Configuration/global.Type) → ToList (Generate c))
      : ∀(c : Configuration/global.Type) → Kubernetes/List.Type

in  Render
