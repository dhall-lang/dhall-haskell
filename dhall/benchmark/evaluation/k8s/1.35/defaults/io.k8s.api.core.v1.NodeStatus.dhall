{ addresses = None (List ./../types/io.k8s.api.core.v1.NodeAddress.dhall)
, allocatable =
    None
      ( List
          { mapKey : Text
          , mapValue :
              ./../types/io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, capacity =
    None
      ( List
          { mapKey : Text
          , mapValue :
              ./../types/io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, conditions = None (List ./../types/io.k8s.api.core.v1.NodeCondition.dhall)
, config = None ./../types/io.k8s.api.core.v1.NodeConfigStatus.dhall
, daemonEndpoints = None ./../types/io.k8s.api.core.v1.NodeDaemonEndpoints.dhall
, declaredFeatures = None (List Text)
, features = None ./../types/io.k8s.api.core.v1.NodeFeatures.dhall
, images = None (List ./../types/io.k8s.api.core.v1.ContainerImage.dhall)
, nodeInfo = None ./../types/io.k8s.api.core.v1.NodeSystemInfo.dhall
, phase = None Text
, runtimeHandlers =
    None (List ./../types/io.k8s.api.core.v1.NodeRuntimeHandler.dhall)
, volumesAttached =
    None (List ./../types/io.k8s.api.core.v1.AttachedVolume.dhall)
, volumesInUse = None (List Text)
}
