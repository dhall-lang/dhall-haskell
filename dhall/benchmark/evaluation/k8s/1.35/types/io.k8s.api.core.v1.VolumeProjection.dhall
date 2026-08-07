{ clusterTrustBundle :
    Optional ./io.k8s.api.core.v1.ClusterTrustBundleProjection.dhall
, configMap : Optional ./io.k8s.api.core.v1.ConfigMapProjection.dhall
, downwardAPI : Optional ./io.k8s.api.core.v1.DownwardAPIProjection.dhall
, podCertificate : Optional ./io.k8s.api.core.v1.PodCertificateProjection.dhall
, secret : Optional ./io.k8s.api.core.v1.SecretProjection.dhall
, serviceAccountToken :
    Optional ./io.k8s.api.core.v1.ServiceAccountTokenProjection.dhall
}
