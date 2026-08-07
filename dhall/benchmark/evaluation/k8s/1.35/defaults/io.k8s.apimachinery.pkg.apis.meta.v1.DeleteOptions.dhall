{ apiVersion = "v1"
, kind = "DeleteOptions"
, dryRun = None (List Text)
, gracePeriodSeconds = None Natural
, ignoreStoreReadErrorWithClusterBreakingPotential = None Bool
, orphanDependents = None Bool
, preconditions =
    None ./../types/io.k8s.apimachinery.pkg.apis.meta.v1.Preconditions.dhall
, propagationPolicy = None Text
}
