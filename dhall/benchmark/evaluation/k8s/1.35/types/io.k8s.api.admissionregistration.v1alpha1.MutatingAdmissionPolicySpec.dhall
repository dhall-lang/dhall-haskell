{ failurePolicy : Optional Text
, matchConditions :
    Optional
      (List ./io.k8s.api.admissionregistration.v1alpha1.MatchCondition.dhall)
, matchConstraints :
    Optional ./io.k8s.api.admissionregistration.v1alpha1.MatchResources.dhall
, mutations :
    Optional (List ./io.k8s.api.admissionregistration.v1alpha1.Mutation.dhall)
, paramKind :
    Optional ./io.k8s.api.admissionregistration.v1alpha1.ParamKind.dhall
, reinvocationPolicy : Optional Text
, variables :
    Optional (List ./io.k8s.api.admissionregistration.v1alpha1.Variable.dhall)
}
