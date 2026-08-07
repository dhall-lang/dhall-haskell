{ driver : Text
, pool : ./io.k8s.api.resource.v1beta1.ResourcePool.dhall
, allNodes : Optional Bool
, devices : Optional (List ./io.k8s.api.resource.v1beta1.Device.dhall)
, nodeName : Optional Text
, nodeSelector : Optional ./io.k8s.api.core.v1.NodeSelector.dhall
, perDeviceNodeSelection : Optional Bool
, sharedCounters :
    Optional (List ./io.k8s.api.resource.v1beta1.CounterSet.dhall)
}
