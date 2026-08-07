{ jobTemplate : ./io.k8s.api.batch.v1.JobTemplateSpec.dhall
, schedule : Text
, concurrencyPolicy : Optional Text
, failedJobsHistoryLimit : Optional Natural
, startingDeadlineSeconds : Optional Natural
, successfulJobsHistoryLimit : Optional Natural
, suspend : Optional Bool
, timeZone : Optional Text
}
