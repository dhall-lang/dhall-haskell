{ maxUnavailable :
    Optional ./io.k8s.apimachinery.pkg.util.intstr.NatOrString.dhall
, minAvailable :
    Optional ./io.k8s.apimachinery.pkg.util.intstr.NatOrString.dhall
, selector : Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
, unhealthyPodEvictionPolicy : Optional Text
}
