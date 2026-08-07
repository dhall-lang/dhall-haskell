{ apiVersion : Text
, kind : Text
, metadata : ./io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
, provisioner : Text
, allowVolumeExpansion : Optional Bool
, allowedTopologies :
    Optional (List ./io.k8s.api.core.v1.TopologySelectorTerm.dhall)
, mountOptions : Optional (List Text)
, parameters : Optional (List { mapKey : Text, mapValue : Text })
, reclaimPolicy : Optional Text
, volumeBindingMode : Optional Text
}
