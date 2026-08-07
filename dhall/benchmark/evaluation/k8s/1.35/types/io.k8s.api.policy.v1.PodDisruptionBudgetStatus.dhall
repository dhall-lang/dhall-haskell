{ currentHealthy : Natural
, desiredHealthy : Natural
, disruptionsAllowed : Natural
, expectedPods : Natural
, conditions :
    Optional (List ./io.k8s.apimachinery.pkg.apis.meta.v1.Condition.dhall)
, disruptedPods :
    Optional
      ( List
          { mapKey : Text
          , mapValue : ./io.k8s.apimachinery.pkg.apis.meta.v1.Time.dhall
          }
      )
, observedGeneration : Optional Natural
}
