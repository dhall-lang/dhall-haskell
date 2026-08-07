{ kind : Text
, group : Optional ./io.k8s.api.flowcontrol.v1.GroupSubject.dhall
, serviceAccount :
    Optional ./io.k8s.api.flowcontrol.v1.ServiceAccountSubject.dhall
, user : Optional ./io.k8s.api.flowcontrol.v1.UserSubject.dhall
}
