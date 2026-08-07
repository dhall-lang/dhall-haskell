{ configMapKeyRef : Optional ./io.k8s.api.core.v1.ConfigMapKeySelector.dhall
, fieldRef : Optional ./io.k8s.api.core.v1.ObjectFieldSelector.dhall
, fileKeyRef : Optional ./io.k8s.api.core.v1.FileKeySelector.dhall
, resourceFieldRef : Optional ./io.k8s.api.core.v1.ResourceFieldSelector.dhall
, secretKeyRef : Optional ./io.k8s.api.core.v1.SecretKeySelector.dhall
}
