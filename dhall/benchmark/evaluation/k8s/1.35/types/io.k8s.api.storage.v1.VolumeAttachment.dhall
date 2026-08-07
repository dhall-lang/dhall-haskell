{ apiVersion : Text
, kind : Text
, metadata : ./io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
, spec : ./io.k8s.api.storage.v1.VolumeAttachmentSpec.dhall
, status : Optional ./io.k8s.api.storage.v1.VolumeAttachmentStatus.dhall
}
