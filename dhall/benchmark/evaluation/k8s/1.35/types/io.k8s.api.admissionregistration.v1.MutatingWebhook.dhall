{ admissionReviewVersions : List Text
, clientConfig : ./io.k8s.api.admissionregistration.v1.WebhookClientConfig.dhall
, name : Text
, sideEffects : Text
, failurePolicy : Optional Text
, matchConditions :
    Optional (List ./io.k8s.api.admissionregistration.v1.MatchCondition.dhall)
, matchPolicy : Optional Text
, namespaceSelector :
    Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
, objectSelector :
    Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
, reinvocationPolicy : Optional Text
, rules :
    Optional
      (List ./io.k8s.api.admissionregistration.v1.RuleWithOperations.dhall)
, timeoutSeconds : Optional Natural
}
