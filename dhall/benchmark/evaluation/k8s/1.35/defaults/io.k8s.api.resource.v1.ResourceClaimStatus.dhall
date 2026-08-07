{ allocation = None ./../types/io.k8s.api.resource.v1.AllocationResult.dhall
, devices =
    None (List ./../types/io.k8s.api.resource.v1.AllocatedDeviceStatus.dhall)
, reservedFor =
    None
      ( List
          ./../types/io.k8s.api.resource.v1.ResourceClaimConsumerReference.dhall
      )
}
