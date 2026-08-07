{ commonEncodingVersion : Optional Text
, conditions :
    Optional
      ( List
          ./io.k8s.api.apiserverinternal.v1alpha1.StorageVersionCondition.dhall
      )
, storageVersions :
    Optional
      (List ./io.k8s.api.apiserverinternal.v1alpha1.ServerStorageVersion.dhall)
}
