{ name : Text
, served : Bool
, storage : Bool
, additionalPrinterColumns :
    Optional
      ( List
          ./io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.CustomResourceColumnDefinition.dhall
      )
, deprecated : Optional Bool
, deprecationWarning : Optional Text
, schema :
    Optional
      ./io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.CustomResourceValidation.dhall
, selectableFields :
    Optional
      ( List
          ./io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.SelectableField.dhall
      )
, subresources :
    Optional
      ./io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.CustomResourceSubresources.dhall
}
