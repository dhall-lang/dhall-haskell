{ apiVersion : Text
, kind : Text
, metadata : ./io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
, spec : ./io.k8s.api.resource.v1beta2.ResourceClaimSpec.dhall
, status : Optional ./io.k8s.api.resource.v1beta2.ResourceClaimStatus.dhall
}
