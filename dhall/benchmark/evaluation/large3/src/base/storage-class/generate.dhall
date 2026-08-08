let Kubernetes/StorageClass =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.api.storage.v1.StorageClass.dhall

let Kubernetes/ObjectMeta =
      ../../../../k8s/dhall-kubernetes/1.17/schemas/io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall

let Configuration/global = ../../configuration/global.dhall

let Util/KeyValuePair = ../../util/key-value-pair.dhall

let component = ./component.dhall

let generate
    : Configuration/global.Type → component
    = λ(c : Configuration/global.Type) →
        let base =
              Kubernetes/StorageClass::{
              , metadata = Kubernetes/ObjectMeta::{
                , labels = Some
                  [ { mapKey = "deploy", mapValue = "sourcegraph" } ]
                , name = Some "sourcegraph"
                }
              , parameters = None (List Util/KeyValuePair)
              , provisioner = ""
              , reclaimPolicy = Some "Retain"
              }

        in  merge
              { GCP = Some
                  (   base
                    ⫽ { parameters = Some
                        [ { mapKey = "type", mapValue = "pd-ssd" } ]
                      , provisioner = "kubernetes.io/gce-pd"
                      }
                  )
              , AWS = Some
                  (   base
                    ⫽ { parameters = Some
                        [ { mapKey = "type", mapValue = "gp2" } ]
                      , provisioner = "kubernetes.io/aws-ebs"
                      }
                  )
              , AZURE = Some
                  (   base
                    ⫽ { parameters = Some
                        [ { mapKey = "storageaccounttype"
                          , mapValue = "Premium_LRS"
                          }
                        ]
                      , provisioner = "kubernetes.io/azure-disk"
                      }
                  )
              , CUSTOM = None Kubernetes/StorageClass.Type
              }
              c.StorageClass.CloudProvider

in  generate
