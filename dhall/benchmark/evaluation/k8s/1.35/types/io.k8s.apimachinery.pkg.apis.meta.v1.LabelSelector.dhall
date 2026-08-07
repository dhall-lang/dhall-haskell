{ matchExpressions :
    Optional
      ( List
          ./io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelectorRequirement.dhall
      )
, matchLabels : Optional (List { mapKey : Text, mapValue : Text })
}
