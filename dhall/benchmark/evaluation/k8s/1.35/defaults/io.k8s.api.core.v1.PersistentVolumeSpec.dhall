{ accessModes = None (List Text)
, awsElasticBlockStore =
    None ./../types/io.k8s.api.core.v1.AWSElasticBlockStoreVolumeSource.dhall
, azureDisk = None ./../types/io.k8s.api.core.v1.AzureDiskVolumeSource.dhall
, azureFile =
    None ./../types/io.k8s.api.core.v1.AzureFilePersistentVolumeSource.dhall
, capacity =
    None
      ( List
          { mapKey : Text
          , mapValue :
              ./../types/io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, cephfs = None ./../types/io.k8s.api.core.v1.CephFSPersistentVolumeSource.dhall
, cinder = None ./../types/io.k8s.api.core.v1.CinderPersistentVolumeSource.dhall
, claimRef = None ./../types/io.k8s.api.core.v1.ObjectReference.dhall
, csi = None ./../types/io.k8s.api.core.v1.CSIPersistentVolumeSource.dhall
, fc = None ./../types/io.k8s.api.core.v1.FCVolumeSource.dhall
, flexVolume =
    None ./../types/io.k8s.api.core.v1.FlexPersistentVolumeSource.dhall
, flocker = None ./../types/io.k8s.api.core.v1.FlockerVolumeSource.dhall
, gcePersistentDisk =
    None ./../types/io.k8s.api.core.v1.GCEPersistentDiskVolumeSource.dhall
, glusterfs =
    None ./../types/io.k8s.api.core.v1.GlusterfsPersistentVolumeSource.dhall
, hostPath = None ./../types/io.k8s.api.core.v1.HostPathVolumeSource.dhall
, iscsi = None ./../types/io.k8s.api.core.v1.ISCSIPersistentVolumeSource.dhall
, local = None ./../types/io.k8s.api.core.v1.LocalVolumeSource.dhall
, mountOptions = None (List Text)
, nfs = None ./../types/io.k8s.api.core.v1.NFSVolumeSource.dhall
, nodeAffinity = None ./../types/io.k8s.api.core.v1.VolumeNodeAffinity.dhall
, persistentVolumeReclaimPolicy = None Text
, photonPersistentDisk =
    None ./../types/io.k8s.api.core.v1.PhotonPersistentDiskVolumeSource.dhall
, portworxVolume = None ./../types/io.k8s.api.core.v1.PortworxVolumeSource.dhall
, quobyte = None ./../types/io.k8s.api.core.v1.QuobyteVolumeSource.dhall
, rbd = None ./../types/io.k8s.api.core.v1.RBDPersistentVolumeSource.dhall
, scaleIO =
    None ./../types/io.k8s.api.core.v1.ScaleIOPersistentVolumeSource.dhall
, storageClassName = None Text
, storageos =
    None ./../types/io.k8s.api.core.v1.StorageOSPersistentVolumeSource.dhall
, volumeAttributesClassName = None Text
, volumeMode = None Text
, vsphereVolume =
    None ./../types/io.k8s.api.core.v1.VsphereVirtualDiskVolumeSource.dhall
}
