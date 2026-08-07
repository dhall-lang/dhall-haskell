{ active : Optional Natural
, completedIndexes : Optional Text
, completionTime : Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.Time.dhall
, conditions : Optional (List ./io.k8s.api.batch.v1.JobCondition.dhall)
, failed : Optional Natural
, failedIndexes : Optional Text
, ready : Optional Natural
, startTime : Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.Time.dhall
, succeeded : Optional Natural
, terminating : Optional Natural
, uncountedTerminatedPods :
    Optional ./io.k8s.api.batch.v1.UncountedTerminatedPods.dhall
}
