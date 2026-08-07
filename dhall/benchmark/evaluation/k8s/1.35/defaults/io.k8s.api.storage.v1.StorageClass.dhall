{ apiVersion = "storage.k8s.io/v1"
, kind = "StorageClass"
, allowVolumeExpansion = None Bool
, allowedTopologies =
    None (List ./../types/io.k8s.api.core.v1.TopologySelectorTerm.dhall)
, mountOptions = None (List Text)
, parameters = None (List { mapKey : Text, mapValue : Text })
, reclaimPolicy = None Text
, volumeBindingMode = None Text
}
