{ apiVersion : Text
, kind : Text
, metadata : ./io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
, spec :
    Optional
      ./io.k8s.api.storagemigration.v1beta1.StorageVersionMigrationSpec.dhall
, status :
    Optional
      ./io.k8s.api.storagemigration.v1beta1.StorageVersionMigrationStatus.dhall
}
