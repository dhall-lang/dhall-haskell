{ apiVersion : Text
, kind : Text
, metadata : ./io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
, automountServiceAccountToken : Optional Bool
, imagePullSecrets :
    Optional (List ./io.k8s.api.core.v1.LocalObjectReference.dhall)
, secrets : Optional (List ./io.k8s.api.core.v1.ObjectReference.dhall)
}
