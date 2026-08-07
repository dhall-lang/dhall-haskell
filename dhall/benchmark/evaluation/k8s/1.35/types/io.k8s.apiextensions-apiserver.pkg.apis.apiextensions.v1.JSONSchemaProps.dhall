{ `$ref` : Optional Text
, `$schema` : Optional Text
, default :
    Optional
      ./io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.JSON.dhall
, description : Optional Text
, enum :
    Optional
      ( List
          ./io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.JSON.dhall
      )
, example :
    Optional
      ./io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.JSON.dhall
, exclusiveMaximum : Optional Bool
, exclusiveMinimum : Optional Bool
, externalDocs :
    Optional
      ./io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.ExternalDocumentation.dhall
, format : Optional Text
, id : Optional Text
, maxItems : Optional Natural
, maxLength : Optional Natural
, maxProperties : Optional Natural
, maximum : Optional Double
, minItems : Optional Natural
, minLength : Optional Natural
, minProperties : Optional Natural
, minimum : Optional Double
, multipleOf : Optional Double
, nullable : Optional Bool
, pattern : Optional Text
, required : Optional (List Text)
, title : Optional Text
, type : Optional Text
, uniqueItems : Optional Bool
, x-kubernetes-embedded-resource : Optional Bool
, x-kubernetes-int-or-string : Optional Bool
, x-kubernetes-list-map-keys : Optional (List Text)
, x-kubernetes-list-type : Optional Text
, x-kubernetes-map-type : Optional Text
, x-kubernetes-preserve-unknown-fields : Optional Bool
, x-kubernetes-validations :
    Optional
      ( List
          ./io.k8s.apiextensions-apiserver.pkg.apis.apiextensions.v1.ValidationRule.dhall
      )
}
