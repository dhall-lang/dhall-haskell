{ egress :
    Optional (List ./io.k8s.api.networking.v1.NetworkPolicyEgressRule.dhall)
, ingress :
    Optional (List ./io.k8s.api.networking.v1.NetworkPolicyIngressRule.dhall)
, podSelector :
    Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
, policyTypes : Optional (List Text)
}
