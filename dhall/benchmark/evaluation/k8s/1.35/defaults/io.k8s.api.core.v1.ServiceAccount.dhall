{ apiVersion = "v1"
, kind = "ServiceAccount"
, automountServiceAccountToken = None Bool
, imagePullSecrets =
    None (List ./../types/io.k8s.api.core.v1.LocalObjectReference.dhall)
, secrets = None (List ./../types/io.k8s.api.core.v1.ObjectReference.dhall)
}
