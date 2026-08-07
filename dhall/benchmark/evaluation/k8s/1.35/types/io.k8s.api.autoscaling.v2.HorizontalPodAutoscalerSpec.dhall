{ maxReplicas : Natural
, scaleTargetRef : ./io.k8s.api.autoscaling.v2.CrossVersionObjectReference.dhall
, behavior :
    Optional ./io.k8s.api.autoscaling.v2.HorizontalPodAutoscalerBehavior.dhall
, metrics : Optional (List ./io.k8s.api.autoscaling.v2.MetricSpec.dhall)
, minReplicas : Optional Natural
}
