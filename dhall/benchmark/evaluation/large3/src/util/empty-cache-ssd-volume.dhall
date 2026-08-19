let Kubernetes/Volume = ../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.core.v1.Volume.dhall

in  Kubernetes/Volume::{
    , emptyDir = Some { medium = None Text, sizeLimit = None Text }
    , name = "cache-ssd"
    }
