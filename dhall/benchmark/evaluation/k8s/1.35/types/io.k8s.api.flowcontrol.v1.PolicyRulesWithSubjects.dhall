{ subjects : List ./io.k8s.api.flowcontrol.v1.Subject.dhall
, nonResourceRules :
    Optional (List ./io.k8s.api.flowcontrol.v1.NonResourcePolicyRule.dhall)
, resourceRules :
    Optional (List ./io.k8s.api.flowcontrol.v1.ResourcePolicyRule.dhall)
}
