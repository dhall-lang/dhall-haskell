{ annotations : Optional (List { mapKey : Text, mapValue : Text })
, creationTimestamp : Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.Time.dhall
, deletionGracePeriodSeconds : Optional Natural
, deletionTimestamp : Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.Time.dhall
, finalizers : Optional (List Text)
, generateName : Optional Text
, generation : Optional Natural
, labels : Optional (List { mapKey : Text, mapValue : Text })
, managedFields :
    Optional
      (List ./io.k8s.apimachinery.pkg.apis.meta.v1.ManagedFieldsEntry.dhall)
, name : Optional Text
, namespace : Optional Text
, ownerReferences :
    Optional (List ./io.k8s.apimachinery.pkg.apis.meta.v1.OwnerReference.dhall)
, resourceVersion : Optional Text
, selfLink : Optional Text
, uid : Optional Text
}
