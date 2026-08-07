{ type : Text
, containerResource :
    Optional ./io.k8s.api.autoscaling.v2.ContainerResourceMetricSource.dhall
, external : Optional ./io.k8s.api.autoscaling.v2.ExternalMetricSource.dhall
, object : Optional ./io.k8s.api.autoscaling.v2.ObjectMetricSource.dhall
, pods : Optional ./io.k8s.api.autoscaling.v2.PodsMetricSource.dhall
, resource : Optional ./io.k8s.api.autoscaling.v2.ResourceMetricSource.dhall
}
