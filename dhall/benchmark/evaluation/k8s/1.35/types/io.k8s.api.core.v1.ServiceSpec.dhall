{ allocateLoadBalancerNodePorts : Optional Bool
, clusterIP : Optional Text
, clusterIPs : Optional (List Text)
, externalIPs : Optional (List Text)
, externalName : Optional Text
, externalTrafficPolicy : Optional Text
, healthCheckNodePort : Optional Natural
, internalTrafficPolicy : Optional Text
, ipFamilies : Optional (List Text)
, ipFamilyPolicy : Optional Text
, loadBalancerClass : Optional Text
, loadBalancerIP : Optional Text
, loadBalancerSourceRanges : Optional (List Text)
, ports : Optional (List ./io.k8s.api.core.v1.ServicePort.dhall)
, publishNotReadyAddresses : Optional Bool
, selector : Optional (List { mapKey : Text, mapValue : Text })
, sessionAffinity : Optional Text
, sessionAffinityConfig :
    Optional ./io.k8s.api.core.v1.SessionAffinityConfig.dhall
, trafficDistribution : Optional Text
, type : Optional Text
}
