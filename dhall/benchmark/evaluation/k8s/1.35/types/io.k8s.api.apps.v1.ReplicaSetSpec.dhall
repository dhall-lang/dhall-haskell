{ selector : ./io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
, minReadySeconds : Optional Natural
, replicas : Optional Natural
, template : Optional ./io.k8s.api.core.v1.PodTemplateSpec.dhall
}
