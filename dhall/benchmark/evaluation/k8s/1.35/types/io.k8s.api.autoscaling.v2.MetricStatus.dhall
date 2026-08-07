{ type : Text
, containerResource :
    Optional ./io.k8s.api.autoscaling.v2.ContainerResourceMetricStatus.dhall
, external : Optional ./io.k8s.api.autoscaling.v2.ExternalMetricStatus.dhall
, object : Optional ./io.k8s.api.autoscaling.v2.ObjectMetricStatus.dhall
, pods : Optional ./io.k8s.api.autoscaling.v2.PodsMetricStatus.dhall
, resource : Optional ./io.k8s.api.autoscaling.v2.ResourceMetricStatus.dhall
}
