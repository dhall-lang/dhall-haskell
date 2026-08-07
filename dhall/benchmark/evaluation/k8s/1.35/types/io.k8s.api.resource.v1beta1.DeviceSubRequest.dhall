{ deviceClassName : Text
, name : Text
, allocationMode : Optional Text
, capacity : Optional ./io.k8s.api.resource.v1beta1.CapacityRequirements.dhall
, count : Optional Natural
, selectors : Optional (List ./io.k8s.api.resource.v1beta1.DeviceSelector.dhall)
, tolerations :
    Optional (List ./io.k8s.api.resource.v1beta1.DeviceToleration.dhall)
}
