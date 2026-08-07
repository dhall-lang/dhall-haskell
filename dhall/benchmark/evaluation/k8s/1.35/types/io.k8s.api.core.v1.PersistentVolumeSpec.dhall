{ accessModes : Optional (List Text)
, awsElasticBlockStore :
    Optional ./io.k8s.api.core.v1.AWSElasticBlockStoreVolumeSource.dhall
, azureDisk : Optional ./io.k8s.api.core.v1.AzureDiskVolumeSource.dhall
, azureFile :
    Optional ./io.k8s.api.core.v1.AzureFilePersistentVolumeSource.dhall
, capacity :
    Optional
      ( List
          { mapKey : Text
          , mapValue : ./io.k8s.apimachinery.pkg.api.resource.Quantity.dhall
          }
      )
, cephfs : Optional ./io.k8s.api.core.v1.CephFSPersistentVolumeSource.dhall
, cinder : Optional ./io.k8s.api.core.v1.CinderPersistentVolumeSource.dhall
, claimRef : Optional ./io.k8s.api.core.v1.ObjectReference.dhall
, csi : Optional ./io.k8s.api.core.v1.CSIPersistentVolumeSource.dhall
, fc : Optional ./io.k8s.api.core.v1.FCVolumeSource.dhall
, flexVolume : Optional ./io.k8s.api.core.v1.FlexPersistentVolumeSource.dhall
, flocker : Optional ./io.k8s.api.core.v1.FlockerVolumeSource.dhall
, gcePersistentDisk :
    Optional ./io.k8s.api.core.v1.GCEPersistentDiskVolumeSource.dhall
, glusterfs :
    Optional ./io.k8s.api.core.v1.GlusterfsPersistentVolumeSource.dhall
, hostPath : Optional ./io.k8s.api.core.v1.HostPathVolumeSource.dhall
, iscsi : Optional ./io.k8s.api.core.v1.ISCSIPersistentVolumeSource.dhall
, local : Optional ./io.k8s.api.core.v1.LocalVolumeSource.dhall
, mountOptions : Optional (List Text)
, nfs : Optional ./io.k8s.api.core.v1.NFSVolumeSource.dhall
, nodeAffinity : Optional ./io.k8s.api.core.v1.VolumeNodeAffinity.dhall
, persistentVolumeReclaimPolicy : Optional Text
, photonPersistentDisk :
    Optional ./io.k8s.api.core.v1.PhotonPersistentDiskVolumeSource.dhall
, portworxVolume : Optional ./io.k8s.api.core.v1.PortworxVolumeSource.dhall
, quobyte : Optional ./io.k8s.api.core.v1.QuobyteVolumeSource.dhall
, rbd : Optional ./io.k8s.api.core.v1.RBDPersistentVolumeSource.dhall
, scaleIO : Optional ./io.k8s.api.core.v1.ScaleIOPersistentVolumeSource.dhall
, storageClassName : Optional Text
, storageos :
    Optional ./io.k8s.api.core.v1.StorageOSPersistentVolumeSource.dhall
, volumeAttributesClassName : Optional Text
, volumeMode : Optional Text
, vsphereVolume :
    Optional ./io.k8s.api.core.v1.VsphereVirtualDiskVolumeSource.dhall
}
