{ failurePolicy : Optional Text
, matchConditions :
    Optional
      (List ./io.k8s.api.admissionregistration.v1beta1.MatchCondition.dhall)
, matchConstraints :
    Optional ./io.k8s.api.admissionregistration.v1beta1.MatchResources.dhall
, mutations :
    Optional (List ./io.k8s.api.admissionregistration.v1beta1.Mutation.dhall)
, paramKind :
    Optional ./io.k8s.api.admissionregistration.v1beta1.ParamKind.dhall
, reinvocationPolicy : Optional Text
, variables :
    Optional (List ./io.k8s.api.admissionregistration.v1beta1.Variable.dhall)
}
