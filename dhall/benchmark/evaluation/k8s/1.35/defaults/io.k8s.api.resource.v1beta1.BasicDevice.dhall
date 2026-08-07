{ allNodes = None Bool
, allowMultipleAllocations = None Bool
, attributes =
    None
      ( List
          { mapKey : Text
          , mapValue :
              ./../types/io.k8s.api.resource.v1beta1.DeviceAttribute.dhall
          }
      )
, bindingConditions = None (List Text)
, bindingFailureConditions = None (List Text)
, bindsToNode = None Bool
, capacity =
    None
      ( List
          { mapKey : Text
          , mapValue :
              ./../types/io.k8s.api.resource.v1beta1.DeviceCapacity.dhall
          }
      )
, consumesCounters =
    None
      ( List
          ./../types/io.k8s.api.resource.v1beta1.DeviceCounterConsumption.dhall
      )
, nodeName = None Text
, nodeSelector = None ./../types/io.k8s.api.core.v1.NodeSelector.dhall
, taints = None (List ./../types/io.k8s.api.resource.v1beta1.DeviceTaint.dhall)
}
