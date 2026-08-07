{ acceptedNames :
    Optional
      ./io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.CustomResourceDefinitionNames.dhall
, conditions :
    Optional
      ( List
          ./io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.CustomResourceDefinitionCondition.dhall
      )
, observedGeneration : Optional Natural
, storedVersions : Optional (List Text)
}
