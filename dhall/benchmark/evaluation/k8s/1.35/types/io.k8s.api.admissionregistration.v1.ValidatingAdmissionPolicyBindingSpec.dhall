{ matchResources :
    Optional ./io.k8s.api.admissionregistration.v1.MatchResources.dhall
, paramRef : Optional ./io.k8s.api.admissionregistration.v1.ParamRef.dhall
, policyName : Optional Text
, validationActions : Optional (List Text)
}
