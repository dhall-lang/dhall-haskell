{ replicas : Natural
, availableReplicas : Optional Natural
, conditions : Optional (List ./io.k8s.api.apps.v1.ReplicaSetCondition.dhall)
, fullyLabeledReplicas : Optional Natural
, observedGeneration : Optional Natural
, readyReplicas : Optional Natural
, terminatingReplicas : Optional Natural
}
