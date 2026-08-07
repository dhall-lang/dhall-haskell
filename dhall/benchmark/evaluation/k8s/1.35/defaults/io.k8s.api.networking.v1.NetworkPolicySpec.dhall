{ egress =
    None
      (List ./../types/io.k8s.api.networking.v1.NetworkPolicyEgressRule.dhall)
, ingress =
    None
      (List ./../types/io.k8s.api.networking.v1.NetworkPolicyIngressRule.dhall)
, podSelector =
    None ./../types/io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
, policyTypes = None (List Text)
}
