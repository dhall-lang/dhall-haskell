{ device : Text
, driver : Text
, pool : Text
, conditions :
    Optional (List ./io.k8s.apimachinery.pkg.apis.meta.v1.Condition.dhall)
, data : Optional ./io.k8s.apimachinery.pkg.runtime.RawExtension.dhall
, networkData : Optional ./io.k8s.api.resource.v1beta1.NetworkDeviceData.dhall
, shareID : Optional Text
}
