{ apiVersion : Text
, kind : Text
, metadata : ./io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
, spec : ./io.k8s.api.certificates.v1beta1.PodCertificateRequestSpec.dhall
, status :
    Optional ./io.k8s.api.certificates.v1beta1.PodCertificateRequestStatus.dhall
}
