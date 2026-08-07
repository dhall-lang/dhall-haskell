{ failurePolicy = None Text
, matchConditions =
    None
      (List ./../types/io.k8s.api.admissionregistration.v1.MatchCondition.dhall)
, matchPolicy = None Text
, namespaceSelector =
    None ./../types/io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
, objectSelector =
    None ./../types/io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
, rules =
    None
      ( List
          ./../types/io.k8s.api.admissionregistration.v1.RuleWithOperations.dhall
      )
, timeoutSeconds = None Natural
}
