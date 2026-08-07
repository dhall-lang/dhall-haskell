{ template : ./io.k8s.api.core.v1.PodTemplateSpec.dhall
, activeDeadlineSeconds : Optional Natural
, backoffLimit : Optional Natural
, backoffLimitPerIndex : Optional Natural
, completionMode : Optional Text
, completions : Optional Natural
, managedBy : Optional Text
, manualSelector : Optional Bool
, maxFailedIndexes : Optional Natural
, parallelism : Optional Natural
, podFailurePolicy : Optional ./io.k8s.api.batch.v1.PodFailurePolicy.dhall
, podReplacementPolicy : Optional Text
, selector : Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
, successPolicy : Optional ./io.k8s.api.batch.v1.SuccessPolicy.dhall
, suspend : Optional Bool
, ttlSecondsAfterFinished : Optional Natural
}
