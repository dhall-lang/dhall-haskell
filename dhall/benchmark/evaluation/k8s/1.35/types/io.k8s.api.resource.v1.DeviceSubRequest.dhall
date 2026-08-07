{ deviceClassName : Text
, name : Text
, allocationMode : Optional Text
, capacity : Optional ./io.k8s.api.resource.v1.CapacityRequirements.dhall
, count : Optional Natural
, selectors : Optional (List ./io.k8s.api.resource.v1.DeviceSelector.dhall)
, tolerations : Optional (List ./io.k8s.api.resource.v1.DeviceToleration.dhall)
}
