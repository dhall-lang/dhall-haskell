{ apiVersion : Text
, kind : Text
, metadata : ./io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
, spec :
    Optional
      ./io.k8s.kube-aggregator.pkg.apis.apiregistration.v1.APIServiceSpec.dhall
, status :
    Optional
      ./io.k8s.kube-aggregator.pkg.apis.apiregistration.v1.APIServiceStatus.dhall
}
