{ image : Text
, imageID : Text
, name : Text
, ready : Bool
, restartCount : Natural
, allocatedResources :
    Optional
      ( List
          { mapKey : Text
          , mapValue : ./io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, allocatedResourcesStatus :
    Optional (List ./io.k8s.api.core.v1.ResourceStatus.dhall)
, containerID : Optional Text
, lastState : Optional ./io.k8s.api.core.v1.ContainerState.dhall
, resources : Optional ./io.k8s.api.core.v1.ResourceRequirements.dhall
, started : Optional Bool
, state : Optional ./io.k8s.api.core.v1.ContainerState.dhall
, stopSignal : Optional Text
, user : Optional ./io.k8s.api.core.v1.ContainerUser.dhall
, volumeMounts : Optional (List ./io.k8s.api.core.v1.VolumeMountStatus.dhall)
}
