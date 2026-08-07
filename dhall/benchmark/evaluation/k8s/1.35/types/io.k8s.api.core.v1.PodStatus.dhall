{ allocatedResources :
    Optional
      ( List
          { mapKey : Text
          , mapValue : ./io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, conditions : Optional (List ./io.k8s.api.core.v1.PodCondition.dhall)
, containerStatuses : Optional (List ./io.k8s.api.core.v1.ContainerStatus.dhall)
, ephemeralContainerStatuses :
    Optional (List ./io.k8s.api.core.v1.ContainerStatus.dhall)
, extendedResourceClaimStatus :
    Optional ./io.k8s.api.core.v1.PodExtendedResourceClaimStatus.dhall
, hostIP : Optional Text
, hostIPs : Optional (List ./io.k8s.api.core.v1.HostIP.dhall)
, initContainerStatuses :
    Optional (List ./io.k8s.api.core.v1.ContainerStatus.dhall)
, message : Optional Text
, nominatedNodeName : Optional Text
, observedGeneration : Optional Natural
, phase : Optional Text
, podIP : Optional Text
, podIPs : Optional (List ./io.k8s.api.core.v1.PodIP.dhall)
, qosClass : Optional Text
, reason : Optional Text
, resize : Optional Text
, resourceClaimStatuses :
    Optional (List ./io.k8s.api.core.v1.PodResourceClaimStatus.dhall)
, resources : Optional ./io.k8s.api.core.v1.ResourceRequirements.dhall
, startTime : Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.Time.dhall
}
