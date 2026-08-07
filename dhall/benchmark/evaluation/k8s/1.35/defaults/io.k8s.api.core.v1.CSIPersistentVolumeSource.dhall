{ controllerExpandSecretRef =
    None ./../types/io.k8s.api.core.v1.SecretReference.dhall
, controllerPublishSecretRef =
    None ./../types/io.k8s.api.core.v1.SecretReference.dhall
, fsType = None Text
, nodeExpandSecretRef = None ./../types/io.k8s.api.core.v1.SecretReference.dhall
, nodePublishSecretRef =
    None ./../types/io.k8s.api.core.v1.SecretReference.dhall
, nodeStageSecretRef = None ./../types/io.k8s.api.core.v1.SecretReference.dhall
, readOnly = None Bool
, volumeAttributes = None (List { mapKey : Text, mapValue : Text })
}
