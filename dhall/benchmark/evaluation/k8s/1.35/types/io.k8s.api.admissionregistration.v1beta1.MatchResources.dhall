{ excludeResourceRules :
    Optional
      ( List
          ./io.k8s.api.admissionregistration.v1beta1.NamedRuleWithOperations.dhall
      )
, matchPolicy : Optional Text
, namespaceSelector :
    Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
, objectSelector :
    Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall
, resourceRules :
    Optional
      ( List
          ./io.k8s.api.admissionregistration.v1beta1.NamedRuleWithOperations.dhall
      )
}
