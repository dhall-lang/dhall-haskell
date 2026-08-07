{ apiVersion : Text
, kind : Text
, metadata : ./io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
, spec : ./io.k8s.api.authorization.v1.SelfSubjectAccessReviewSpec.dhall
, status :
    Optional ./io.k8s.api.authorization.v1.SubjectAccessReviewStatus.dhall
}
