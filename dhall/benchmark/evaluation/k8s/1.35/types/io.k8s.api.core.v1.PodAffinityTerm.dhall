{ topologyKey : Text
, labelSelector :
    Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
, matchLabelKeys : Optional (List Text)
, mismatchLabelKeys : Optional (List Text)
, namespaceSelector :
    Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
, namespaces : Optional (List Text)
}
