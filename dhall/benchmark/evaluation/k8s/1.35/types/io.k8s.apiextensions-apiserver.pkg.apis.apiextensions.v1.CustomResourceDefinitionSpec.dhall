{ group : Text
, names :
    ./io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.CustomResourceDefinitionNames.dhall
, scope : Text
, versions :
    List
      ./io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.CustomResourceDefinitionVersion.dhall
, conversion :
    Optional
      ./io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.CustomResourceConversion.dhall
, preserveUnknownFields : Optional Bool
}
