{ allocation =
    None ./../types/io.k8s.api.resource.v1beta2.AllocationResult.dhall
, devices =
    None
      (List ./../types/io.k8s.api.resource.v1beta2.AllocatedDeviceStatus.dhall)
, reservedFor =
    None
      ( List
          ./../types/io.k8s.api.resource.v1beta2.ResourceClaimConsumerReference.dhall
      )
}
