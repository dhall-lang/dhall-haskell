{ acquireTime : Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.MicroTime.dhall
, holderIdentity : Optional Text
, leaseDurationSeconds : Optional Natural
, leaseTransitions : Optional Natural
, preferredHolder : Optional Text
, renewTime : Optional ./io.k8s.apimachinery.pkg.apis.meta.v1.MicroTime.dhall
, strategy : Optional Text
}
