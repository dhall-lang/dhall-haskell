{ accessModes = None (List Text)
, allocatedResourceStatuses = None (List { mapKey : Text, mapValue : Text })
, allocatedResources =
    None
      ( List
          { mapKey : Text
          , mapValue :
              ./../types/io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, capacity =
    None
      ( List
          { mapKey : Text
          , mapValue :
              ./../types/io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, conditions =
    None
      (List ./../types/io.k8s.api.core.v1.PersistentVolumeClaimCondition.dhall)
, currentVolumeAttributesClassName = None Text
, modifyVolumeStatus =
    None ./../types/io.k8s.api.core.v1.ModifyVolumeStatus.dhall
, phase = None Text
}
