{ apiVersion : Text
, kind : Text
, name : Text
, versions :
    List ./io.k8s.apimachinery.pkg.apis.meta.v1.GroupVersionForDiscovery.dhall
, preferredVersion :
    Optional
      ./io.k8s.apimachinery.pkg.apis.meta.v1.GroupVersionForDiscovery.dhall
, serverAddressByClientCIDRs :
    Optional
      ( List
          ./io.k8s.apimachinery.pkg.apis.meta.v1.ServerAddressByClientCIDR.dhall
      )
}
