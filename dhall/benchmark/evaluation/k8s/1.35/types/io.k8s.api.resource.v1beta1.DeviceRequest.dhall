{ name : Text
, adminAccess : Optional Bool
, allocationMode : Optional Text
, capacity : Optional ./io.k8s.api.resource.v1beta1.CapacityRequirements.dhall
, count : Optional Natural
, deviceClassName : Optional Text
, firstAvailable :
    Optional (List ./io.k8s.api.resource.v1beta1.DeviceSubRequest.dhall)
, selectors : Optional (List ./io.k8s.api.resource.v1beta1.DeviceSelector.dhall)
, tolerations :
    Optional (List ./io.k8s.api.resource.v1beta1.DeviceToleration.dhall)
}
