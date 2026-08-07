{ configSource : Optional ./io.k8s.api.core.v1.NodeConfigSource.dhall
, externalID : Optional Text
, podCIDR : Optional Text
, podCIDRs : Optional (List Text)
, providerID : Optional Text
, taints : Optional (List ./io.k8s.api.core.v1.Taint.dhall)
, unschedulable : Optional Bool
}
