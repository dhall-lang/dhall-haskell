{ failurePolicy = None Text
, matchConditions =
    None
      ( List
          ./../types/io.k8s.api.admissionregistration.v1beta1.MatchCondition.dhall
      )
, matchConstraints =
    None
      ./../types/io.k8s.api.admissionregistration.v1beta1.MatchResources.dhall
, mutations =
    None
      (List ./../types/io.k8s.api.admissionregistration.v1beta1.Mutation.dhall)
, paramKind =
    None ./../types/io.k8s.api.admissionregistration.v1beta1.ParamKind.dhall
, reinvocationPolicy = None Text
, variables =
    None
      (List ./../types/io.k8s.api.admissionregistration.v1beta1.Variable.dhall)
}
