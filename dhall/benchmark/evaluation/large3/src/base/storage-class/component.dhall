let Kubernetes/StorageClass =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.storage.v1.StorageClass.dhall

let component = Optional Kubernetes/StorageClass.Type

in  component
