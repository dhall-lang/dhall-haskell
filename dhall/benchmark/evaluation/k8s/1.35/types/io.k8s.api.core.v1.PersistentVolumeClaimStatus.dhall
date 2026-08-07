{ accessModes : Optional (List Text)
, allocatedResourceStatuses : Optional (List { mapKey : Text, mapValue : Text })
, allocatedResources :
    Optional
      ( List
          { mapKey : Text
          , mapValue : ./io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, capacity :
    Optional
      ( List
          { mapKey : Text
          , mapValue : ./io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, conditions :
    Optional (List ./io.k8s.api.core.v1.PersistentVolumeClaimCondition.dhall)
, currentVolumeAttributesClassName : Optional Text
, modifyVolumeStatus : Optional ./io.k8s.api.core.v1.ModifyVolumeStatus.dhall
, phase : Optional Text
}
