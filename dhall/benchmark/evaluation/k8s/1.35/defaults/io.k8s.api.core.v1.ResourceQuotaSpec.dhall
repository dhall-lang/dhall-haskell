{ hard =
    None
      ( List
          { mapKey : Text
          , mapValue :
              ./../types/io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, scopeSelector = None ./../types/io.k8s.api.core.v1.ScopeSelector.dhall
, scopes = None (List Text)
}
