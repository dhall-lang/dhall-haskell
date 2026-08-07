{ apiVersion : Text
, kind : Text
, metadata : ./io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
, aggregationRule : Optional ./io.k8s.api.rbac.v1.AggregationRule.dhall
, rules : Optional (List ./io.k8s.api.rbac.v1.PolicyRule.dhall)
}
