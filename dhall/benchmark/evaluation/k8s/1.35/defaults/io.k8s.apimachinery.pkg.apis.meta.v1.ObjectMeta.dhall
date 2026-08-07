{ annotations = None (List { mapKey : Text, mapValue : Text })
, creationTimestamp =
    None ./../types/io.k8s.apimachinery.pkg.apis.meta.v1.Time.dhall
, deletionGracePeriodSeconds = None Natural
, deletionTimestamp =
    None ./../types/io.k8s.apimachinery.pkg.apis.meta.v1.Time.dhall
, finalizers = None (List Text)
, generateName = None Text
, generation = None Natural
, labels = None (List { mapKey : Text, mapValue : Text })
, managedFields =
    None
      ( List
          ./../types/io.k8s.apimachinery.pkg.apis.meta.v1.ManagedFieldsEntry.dhall
      )
, name = None Text
, namespace = None Text
, ownerReferences =
    None
      ( List
          ./../types/io.k8s.apimachinery.pkg.apis.meta.v1.OwnerReference.dhall
      )
, resourceVersion = None Text
, selfLink = None Text
, uid = None Text
}
