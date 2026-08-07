{ device : Text
, driver : Text
, pool : Text
, request : Text
, adminAccess : Optional Bool
, bindingConditions : Optional (List Text)
, bindingFailureConditions : Optional (List Text)
, consumedCapacity :
    Optional
      ( List
          { mapKey : Text
          , mapValue : ./io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, shareID : Optional Text
, tolerations :
    Optional (List ./io.k8s.api.resource.v1beta2.DeviceToleration.dhall)
}
