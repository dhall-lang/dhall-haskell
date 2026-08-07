{ apiVersion : Text
, kind : Text
, dryRun : Optional (List Text)
, gracePeriodSeconds : Optional Natural
, ignoreStoreReadErrorWithClusterBreakingPotential : Optional Bool
, orphanDependents : Optional Bool
, preconditions :
    Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.Preconditions.dhall
, propagationPolicy : Optional Text
}
