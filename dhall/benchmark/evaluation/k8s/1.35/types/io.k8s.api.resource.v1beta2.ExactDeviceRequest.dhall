{ deviceClassName : Text
, adminAccess : Optional Bool
, allocationMode : Optional Text
, capacity : Optional ./io.k8s.api.resource.v1beta2.CapacityRequirements.dhall
, count : Optional Natural
, selectors : Optional (List ./io.k8s.api.resource.v1beta2.DeviceSelector.dhall)
, tolerations :
    Optional (List ./io.k8s.api.resource.v1beta2.DeviceToleration.dhall)
}
