{ apiVersion = "events.k8s.io/v1"
, kind = "Event"
, action = None Text
, deprecatedCount = None Natural
, deprecatedFirstTimestamp =
    None ./../types/io.k8s.apimachinery.pkg.apis.meta.v1.Time.dhall
, deprecatedLastTimestamp =
    None ./../types/io.k8s.apimachinery.pkg.apis.meta.v1.Time.dhall
, deprecatedSource = None ./../types/io.k8s.api.core.v1.EventSource.dhall
, note = None Text
, reason = None Text
, regarding = None ./../types/io.k8s.api.core.v1.ObjectReference.dhall
, related = None ./../types/io.k8s.api.core.v1.ObjectReference.dhall
, reportingController = None Text
, reportingInstance = None Text
, series = None ./../types/io.k8s.api.events.v1.EventSeries.dhall
, type = None Text
}
