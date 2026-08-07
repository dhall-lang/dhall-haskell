{ preferredDuringSchedulingIgnoredDuringExecution :
    Optional (List ./io.k8s.api.core.v1.PreferredSchedulingTerm.dhall)
, requiredDuringSchedulingIgnoredDuringExecution :
    Optional ./io.k8s.api.core.v1.NodeSelector.dhall
}
