{ addresses : Optional (List ./io.k8s.api.core.v1.NodeAddress.dhall)
, allocatable :
    Optional
      ( List
          { mapKey : Text
          , mapValue : ./io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, capacity :
    Optional
      ( List
          { mapKey : Text
          , mapValue : ./io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, conditions : Optional (List ./io.k8s.api.core.v1.NodeCondition.dhall)
, config : Optional ./io.k8s.api.core.v1.NodeConfigStatus.dhall
, daemonEndpoints : Optional ./io.k8s.api.core.v1.NodeDaemonEndpoints.dhall
, declaredFeatures : Optional (List Text)
, features : Optional ./io.k8s.api.core.v1.NodeFeatures.dhall
, images : Optional (List ./io.k8s.api.core.v1.ContainerImage.dhall)
, nodeInfo : Optional ./io.k8s.api.core.v1.NodeSystemInfo.dhall
, phase : Optional Text
, runtimeHandlers :
    Optional (List ./io.k8s.api.core.v1.NodeRuntimeHandler.dhall)
, volumesAttached : Optional (List ./io.k8s.api.core.v1.AttachedVolume.dhall)
, volumesInUse : Optional (List Text)
}
