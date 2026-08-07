{ apiVersion : Text
, kind : Text
, metadata : ./io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall
, webhooks :
    Optional
      (List ./io.k8s.api.admissionregistration.v1.ValidatingWebhook.dhall)
}
