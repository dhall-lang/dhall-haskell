let Kubernetes/TypesUnion = ../../../k8s/dhall-kubernetes/1.17/typesUnion.dhall

in  { Type =
          { apiVersion : Text, kind : Text, items : List Kubernetes/TypesUnion }
        : Type
    , default =
      { apiVersion = "v1"
      , kind = "List"
      , items = [] : List Kubernetes/TypesUnion
      }
    }
