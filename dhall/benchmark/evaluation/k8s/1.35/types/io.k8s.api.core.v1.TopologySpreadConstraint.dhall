{ maxSkew : Natural
, topologyKey : Text
, whenUnsatisfiable : Text
, labelSelector :
    Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
, matchLabelKeys : Optional (List Text)
, minDomains : Optional Natural
, nodeAffinityPolicy : Optional Text
, nodeTaintsPolicy : Optional Text
}
