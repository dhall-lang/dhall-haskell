{ apiVersion : Text
, kind : Text
, metadata : ./io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
, spec : ./io.k8s.api.authentication.v1.TokenReviewSpec.dhall
, status : Optional ./io.k8s.api.authentication.v1.TokenReviewStatus.dhall
}
