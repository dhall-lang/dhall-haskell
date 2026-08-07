{ adminAccess = None Bool
, bindingConditions = None (List Text)
, bindingFailureConditions = None (List Text)
, consumedCapacity =
    None
      ( List
          { mapKey : Text
          , mapValue :
              ./../types/io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, shareID = None Text
, tolerations =
    None (List ./../types/io.k8s.api.resource.v1.DeviceToleration.dhall)
}
