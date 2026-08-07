{ driver : Text
, fsType : Optional Text
, options : Optional (List { mapKey : Text, mapValue : Text })
, readOnly : Optional Bool
, secretRef : Optional ./io.k8s.api.core.v1.LocalObjectReference.dhall
}
