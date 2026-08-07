{ default : Optional ./io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
, validRange :
    Optional ./io.k8s.api.resource.v1beta2.CapacityRequestPolicyRange.dhall
, validValues :
    Optional (List ./io.k8s.apimachinery.pkg.api.resource.Quantity.dhall)
}
