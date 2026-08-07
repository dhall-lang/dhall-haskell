{ allocatedResources =
    None
      ( List
          { mapKey : Text
          , mapValue :
              ./../types/io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, conditions = None (List ./../types/io.k8s.api.core.v1.PodCondition.dhall)
, containerStatuses =
    None (List ./../types/io.k8s.api.core.v1.ContainerStatus.dhall)
, ephemeralContainerStatuses =
    None (List ./../types/io.k8s.api.core.v1.ContainerStatus.dhall)
, extendedResourceClaimStatus =
    None ./../types/io.k8s.api.core.v1.PodExtendedResourceClaimStatus.dhall
, hostIP = None Text
, hostIPs = None (List ./../types/io.k8s.api.core.v1.HostIP.dhall)
, initContainerStatuses =
    None (List ./../types/io.k8s.api.core.v1.ContainerStatus.dhall)
, message = None Text
, nominatedNodeName = None Text
, observedGeneration = None Natural
, phase = None Text
, podIP = None Text
, podIPs = None (List ./../types/io.k8s.api.core.v1.PodIP.dhall)
, qosClass = None Text
, reason = None Text
, resize = None Text
, resourceClaimStatuses =
    None (List ./../types/io.k8s.api.core.v1.PodResourceClaimStatus.dhall)
, resources = None ./../types/io.k8s.api.core.v1.ResourceRequirements.dhall
, startTime = None ./../types/io.k8s.apimachinery.pkg.apis.meta.v1.Time.dhall
}
