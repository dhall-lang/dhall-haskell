{ addresses : List Text
, conditions : Optional ./io.k8s.api.discovery.v1.EndpointConditions.dhall
, deprecatedTopology : Optional (List { mapKey : Text, mapValue : Text })
, hints : Optional ./io.k8s.api.discovery.v1.EndpointHints.dhall
, hostname : Optional Text
, nodeName : Optional Text
, targetRef : Optional ./io.k8s.api.core.v1.ObjectReference.dhall
, zone : Optional Text
}
