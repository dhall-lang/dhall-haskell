{ extra : Optional (List { mapKey : Text, mapValue : List Text })
, groups : Optional (List Text)
, nonResourceAttributes :
    Optional ./io.k8s.api.authorization.v1.NonResourceAttributes.dhall
, resourceAttributes :
    Optional ./io.k8s.api.authorization.v1.ResourceAttributes.dhall
, uid : Optional Text
, user : Optional Text
}
