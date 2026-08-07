{ allocation : Optional ./io.k8s.api.resource.v1beta2.AllocationResult.dhall
, devices :
    Optional (List ./io.k8s.api.resource.v1beta2.AllocatedDeviceStatus.dhall)
, reservedFor :
    Optional
      (List ./io.k8s.api.resource.v1beta2.ResourceClaimConsumerReference.dhall)
}
