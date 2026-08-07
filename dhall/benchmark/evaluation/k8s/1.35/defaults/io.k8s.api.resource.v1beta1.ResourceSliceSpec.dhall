{ allNodes = None Bool
, devices = None (List ./../types/io.k8s.api.resource.v1beta1.Device.dhall)
, nodeName = None Text
, nodeSelector = None ./../types/io.k8s.api.core.v1.NodeSelector.dhall
, perDeviceNodeSelection = None Bool
, sharedCounters =
    None (List ./../types/io.k8s.api.resource.v1beta1.CounterSet.dhall)
}
