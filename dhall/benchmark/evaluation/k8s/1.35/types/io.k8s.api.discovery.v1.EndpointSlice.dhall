{ addressType : Text
, apiVersion : Text
, endpoints : List ./io.k8s.api.discovery.v1.Endpoint.dhall
, kind : Text
, metadata : ./io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
, ports : Optional (List ./io.k8s.api.discovery.v1.EndpointPort.dhall)
}
