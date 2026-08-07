{ driver : Text
, volumeHandle : Text
, controllerExpandSecretRef :
    Optional ./io.k8s.api.core.v1.SecretReference.dhall
, controllerPublishSecretRef :
    Optional ./io.k8s.api.core.v1.SecretReference.dhall
, fsType : Optional Text
, nodeExpandSecretRef : Optional ./io.k8s.api.core.v1.SecretReference.dhall
, nodePublishSecretRef : Optional ./io.k8s.api.core.v1.SecretReference.dhall
, nodeStageSecretRef : Optional ./io.k8s.api.core.v1.SecretReference.dhall
, readOnly : Optional Bool
, volumeAttributes : Optional (List { mapKey : Text, mapValue : Text })
}
