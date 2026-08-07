{ replicas : Natural
, availableReplicas : Optional Natural
, conditions :
    Optional (List ./io.k8s.api.core.v1.ReplicationControllerCondition.dhall)
, fullyLabeledReplicas : Optional Natural
, observedGeneration : Optional Natural
, readyReplicas : Optional Natural
}
