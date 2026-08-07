{ iqn : Text
, lun : Natural
, targetPortal : Text
, chapAuthDiscovery : Optional Bool
, chapAuthSession : Optional Bool
, fsType : Optional Text
, initiatorName : Optional Text
, iscsiInterface : Optional Text
, portals : Optional (List Text)
, readOnly : Optional Bool
, secretRef : Optional ./io.k8s.api.core.v1.SecretReference.dhall
}
