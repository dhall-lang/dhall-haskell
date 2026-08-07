{ apiVersion : Text
, involvedObject : ./io.k8s.api.core.v1.ObjectReference.dhall
, kind : Text
, metadata : ./io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
, action : Optional Text
, count : Optional Natural
, eventTime : Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.MicroTime.dhall
, firstTimestamp : Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.Time.dhall
, lastTimestamp : Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.Time.dhall
, message : Optional Text
, reason : Optional Text
, related : Optional ./io.k8s.api.core.v1.ObjectReference.dhall
, reportingComponent : Optional Text
, reportingInstance : Optional Text
, series : Optional ./io.k8s.api.core.v1.EventSeries.dhall
, source : Optional ./io.k8s.api.core.v1.EventSource.dhall
, type : Optional Text
}
