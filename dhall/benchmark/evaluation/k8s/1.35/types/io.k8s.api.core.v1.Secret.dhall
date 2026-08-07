{ apiVersion : Text
, kind : Text
, metadata : ./io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
, data : Optional (List { mapKey : Text, mapValue : Text })
, immutable : Optional Bool
, stringData : Optional (List { mapKey : Text, mapValue : Text })
, type : Optional Text
}
