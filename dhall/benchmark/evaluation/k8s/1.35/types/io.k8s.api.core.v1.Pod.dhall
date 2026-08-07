{ apiVersion : Text
, kind : Text
, metadata : ./io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
, spec : Optional ./io.k8s.api.core.v1.PodSpec.dhall
, status : Optional ./io.k8s.api.core.v1.PodStatus.dhall
}
