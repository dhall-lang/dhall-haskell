{ selector : ./io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
, template : ./io.k8s.api.core.v1.PodTemplateSpec.dhall
, minReadySeconds : Optional Natural
, revisionHistoryLimit : Optional Natural
, updateStrategy : Optional ./io.k8s.api.apps.v1.DaemonSetUpdateStrategy.dhall
}
