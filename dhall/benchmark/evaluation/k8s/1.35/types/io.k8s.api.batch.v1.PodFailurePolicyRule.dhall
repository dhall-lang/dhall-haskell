{ action : Text
, onExitCodes :
    Optional ./io.k8s.api.batch.v1.PodFailurePolicyOnExitCodesRequirement.dhall
, onPodConditions :
    Optional
      (List ./io.k8s.api.batch.v1.PodFailurePolicyOnPodConditionsPattern.dhall)
}
