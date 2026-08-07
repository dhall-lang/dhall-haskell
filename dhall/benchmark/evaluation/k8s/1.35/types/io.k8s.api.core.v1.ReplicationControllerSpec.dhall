{ minReadySeconds : Optional Natural
, replicas : Optional Natural
, selector : Optional (List { mapKey : Text, mapValue : Text })
, template : Optional ./io.k8s.api.core.v1.PodTemplateSpec.dhall
}
