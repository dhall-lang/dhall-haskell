{ maxReplicas : Natural
, scaleTargetRef : ./io.k8s.api.autoscaling.v1.CrossVersionObjectReference.dhall
, minReplicas : Optional Natural
, targetCPUUtilizationPercentage : Optional Natural
}
