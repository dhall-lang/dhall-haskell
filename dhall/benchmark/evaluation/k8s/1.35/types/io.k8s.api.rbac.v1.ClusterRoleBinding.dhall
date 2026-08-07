{ apiVersion : Text
, kind : Text
, metadata : ./io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
, roleRef : ./io.k8s.api.rbac.v1.RoleRef.dhall
, subjects : Optional (List ./io.k8s.api.rbac.v1.Subject.dhall)
}
