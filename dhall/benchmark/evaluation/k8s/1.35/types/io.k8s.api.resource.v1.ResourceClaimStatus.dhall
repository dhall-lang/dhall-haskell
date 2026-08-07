{ allocation : Optional ./io.k8s.api.resource.v1.AllocationResult.dhall
, devices : Optional (List ./io.k8s.api.resource.v1.AllocatedDeviceStatus.dhall)
, reservedFor :
    Optional
      (List ./io.k8s.api.resource.v1.ResourceClaimConsumerReference.dhall)
}
