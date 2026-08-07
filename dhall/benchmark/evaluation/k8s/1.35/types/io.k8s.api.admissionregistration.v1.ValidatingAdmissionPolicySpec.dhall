{ auditAnnotations :
    Optional (List ./io.k8s.api.admissionregistration.v1.AuditAnnotation.dhall)
, failurePolicy : Optional Text
, matchConditions :
    Optional (List ./io.k8s.api.admissionregistration.v1.MatchCondition.dhall)
, matchConstraints :
    Optional ./io.k8s.api.admissionregistration.v1.MatchResources.dhall
, paramKind : Optional ./io.k8s.api.admissionregistration.v1.ParamKind.dhall
, validations :
    Optional (List ./io.k8s.api.admissionregistration.v1.Validation.dhall)
, variables :
    Optional (List ./io.k8s.api.admissionregistration.v1.Variable.dhall)
}
