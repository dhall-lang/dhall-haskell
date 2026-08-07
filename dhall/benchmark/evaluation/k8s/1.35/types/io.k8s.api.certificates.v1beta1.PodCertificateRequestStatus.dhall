{ beginRefreshAt : Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.Time.dhall
, certificateChain : Optional Text
, conditions :
    Optional (List ./io.k8s.apimachinery.pkg.apis.meta.v1.Condition.dhall)
, notAfter : Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.Time.dhall
, notBefore : Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.Time.dhall
}
