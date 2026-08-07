{ conditions =
    None (List ./../types/io.k8s.apimachinery.pkg.apis.meta.v1.Condition.dhall)
, disruptedPods =
    None
      ( List
          { mapKey : Text
          , mapValue :
              ./../types/io.k8s.apimachinery.pkg.apis.meta.v1.Time.dhall
          }
      )
, observedGeneration = None Natural
}
