{ conditions :
    Optional (List ./io.k8s.apimachinery.pkg.apis.meta.v1.Condition.dhall)
, observedGeneration : Optional Natural
, typeChecking :
    Optional ./io.k8s.api.admissionregistration.v1.TypeChecking.dhall
}
