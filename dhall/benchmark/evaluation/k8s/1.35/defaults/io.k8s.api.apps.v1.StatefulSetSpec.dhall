{ minReadySeconds = None Natural
, ordinals = None ./../types/io.k8s.api.apps.v1.StatefulSetOrdinals.dhall
, persistentVolumeClaimRetentionPolicy =
    None
      ./../types/io.k8s.api.apps.v1.StatefulSetPersistentVolumeClaimRetentionPolicy.dhall
, podManagementPolicy = None Text
, replicas = None Natural
, revisionHistoryLimit = None Natural
, serviceName = None Text
, updateStrategy =
    None ./../types/io.k8s.api.apps.v1.StatefulSetUpdateStrategy.dhall
, volumeClaimTemplates =
    None (List ./../types/io.k8s.api.core.v1.PersistentVolumeClaim.dhall)
}
