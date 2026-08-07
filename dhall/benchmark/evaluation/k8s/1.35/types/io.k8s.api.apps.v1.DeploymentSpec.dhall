{ selector : ./io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
, template : ./io.k8s.api.core.v1.PodTemplateSpec.dhall
, minReadySeconds : Optional Natural
, paused : Optional Bool
, progressDeadlineSeconds : Optional Natural
, replicas : Optional Natural
, revisionHistoryLimit : Optional Natural
, strategy : Optional ./io.k8s.api.apps.v1.DeploymentStrategy.dhall
}
