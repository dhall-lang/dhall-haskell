{ driver : Text
, fsType : Optional Text
, nodePublishSecretRef :
    Optional ./io.k8s.api.core.v1.LocalObjectReference.dhall
, readOnly : Optional Bool
, volumeAttributes : Optional (List { mapKey : Text, mapValue : Text })
}
