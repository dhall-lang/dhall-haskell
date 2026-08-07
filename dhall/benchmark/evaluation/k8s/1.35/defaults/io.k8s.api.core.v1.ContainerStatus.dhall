{ allocatedResources =
    None
      ( List
          { mapKey : Text
          , mapValue :
              ./../types/io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, allocatedResourcesStatus =
    None (List ./../types/io.k8s.api.core.v1.ResourceStatus.dhall)
, containerID = None Text
, lastState = None ./../types/io.k8s.api.core.v1.ContainerState.dhall
, resources = None ./../types/io.k8s.api.core.v1.ResourceRequirements.dhall
, started = None Bool
, state = None ./../types/io.k8s.api.core.v1.ContainerState.dhall
, stopSignal = None Text
, user = None ./../types/io.k8s.api.core.v1.ContainerUser.dhall
, volumeMounts =
    None (List ./../types/io.k8s.api.core.v1.VolumeMountStatus.dhall)
}
