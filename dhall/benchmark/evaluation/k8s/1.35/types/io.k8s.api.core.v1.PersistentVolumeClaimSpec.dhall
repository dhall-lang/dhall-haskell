{ accessModes : Optional (List Text)
, dataSource : Optional ./io.k8s.api.core.v1.TypedLocalObjectReference.dhall
, dataSourceRef : Optional ./io.k8s.api.core.v1.TypedObjectReference.dhall
, resources : Optional ./io.k8s.api.core.v1.VolumeResourceRequirements.dhall
, selector : Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
, storageClassName : Optional Text
, volumeAttributesClassName : Optional Text
, volumeMode : Optional Text
, volumeName : Optional Text
}
