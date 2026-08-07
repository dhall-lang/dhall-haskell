{ attachRequired = None Bool
, fsGroupPolicy = None Text
, nodeAllocatableUpdatePeriodSeconds = None Natural
, podInfoOnMount = None Bool
, requiresRepublish = None Bool
, seLinuxMount = None Bool
, serviceAccountTokenInSecrets = None Bool
, storageCapacity = None Bool
, tokenRequests =
    None (List ./../types/io.k8s.api.storage.v1.TokenRequest.dhall)
, volumeLifecycleModes = None (List Text)
}
