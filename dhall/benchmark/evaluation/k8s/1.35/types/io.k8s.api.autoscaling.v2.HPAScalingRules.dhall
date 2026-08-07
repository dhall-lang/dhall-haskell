{ policies : Optional (List ./io.k8s.api.autoscaling.v2.HPAScalingPolicy.dhall)
, selectPolicy : Optional Text
, stabilizationWindowSeconds : Optional Natural
, tolerance : Optional ./io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
}
