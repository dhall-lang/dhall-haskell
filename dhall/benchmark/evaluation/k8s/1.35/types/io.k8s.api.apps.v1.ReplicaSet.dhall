{ apiVersion : Text
, kind : Text
, metadata : ./io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
, spec : Optional ./io.k8s.api.apps.v1.ReplicaSetSpec.dhall
, status : Optional ./io.k8s.api.apps.v1.ReplicaSetStatus.dhall
}
