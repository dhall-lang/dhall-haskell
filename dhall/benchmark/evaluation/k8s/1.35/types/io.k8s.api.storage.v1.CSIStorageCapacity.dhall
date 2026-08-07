{ apiVersion : Text
, kind : Text
, metadata : ./io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
, storageClassName : Text
, capacity : Optional ./io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
, maximumVolumeSize :
    Optional ./io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
, nodeTopology :
    Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
}
