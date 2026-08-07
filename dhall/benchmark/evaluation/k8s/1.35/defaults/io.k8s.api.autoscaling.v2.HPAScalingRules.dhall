{ policies =
    None (List ./../types/io.k8s.api.autoscaling.v2.HPAScalingPolicy.dhall)
, selectPolicy = None Text
, stabilizationWindowSeconds = None Natural
, tolerance =
    None ./../types/io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
}
