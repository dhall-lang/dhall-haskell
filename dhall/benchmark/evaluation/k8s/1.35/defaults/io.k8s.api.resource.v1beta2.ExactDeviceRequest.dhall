{ adminAccess = None Bool
, allocationMode = None Text
, capacity =
    None ./../types/io.k8s.api.resource.v1beta2.CapacityRequirements.dhall
, count = None Natural
, selectors =
    None (List ./../types/io.k8s.api.resource.v1beta2.DeviceSelector.dhall)
, tolerations =
    None (List ./../types/io.k8s.api.resource.v1beta2.DeviceToleration.dhall)
}
