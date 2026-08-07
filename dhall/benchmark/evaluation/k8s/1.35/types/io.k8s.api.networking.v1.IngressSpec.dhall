{ defaultBackend : Optional ./io.k8s.api.networking.v1.IngressBackend.dhall
, ingressClassName : Optional Text
, rules : Optional (List ./io.k8s.api.networking.v1.IngressRule.dhall)
, tls : Optional (List ./io.k8s.api.networking.v1.IngressTLS.dhall)
}
