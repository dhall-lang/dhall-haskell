{ hard :
    Optional
      ( List
          { mapKey : Text
          , mapValue : ./io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, scopeSelector : Optional ./io.k8s.api.core.v1.ScopeSelector.dhall
, scopes : Optional (List Text)
}
