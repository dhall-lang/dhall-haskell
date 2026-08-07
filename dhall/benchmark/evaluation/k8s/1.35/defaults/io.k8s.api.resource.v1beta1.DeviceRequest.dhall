{ adminAccess = None Bool
, allocationMode = None Text
, capacity =
    None ./../types/io.k8s.api.resource.v1beta1.CapacityRequirements.dhall
, count = None Natural
, deviceClassName = None Text
, firstAvailable =
    None (List ./../types/io.k8s.api.resource.v1beta1.DeviceSubRequest.dhall)
, selectors =
    None (List ./../types/io.k8s.api.resource.v1beta1.DeviceSelector.dhall)
, tolerations =
    None (List ./../types/io.k8s.api.resource.v1beta1.DeviceToleration.dhall)
}
