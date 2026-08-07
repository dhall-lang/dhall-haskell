{ desiredReplicas : Natural
, conditions :
    Optional
      (List ./io.k8s.api.autoscaling.v2.HorizontalPodAutoscalerCondition.dhall)
, currentMetrics :
    Optional (List ./io.k8s.api.autoscaling.v2.MetricStatus.dhall)
, currentReplicas : Optional Natural
, lastScaleTime : Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.Time.dhall
, observedGeneration : Optional Natural
}
