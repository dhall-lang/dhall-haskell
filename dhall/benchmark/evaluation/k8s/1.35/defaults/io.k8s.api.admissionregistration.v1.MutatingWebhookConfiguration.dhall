{ apiVersion = "admissionregistration.k8s.io/v1"
, kind = "MutatingWebhookConfiguration"
, webhooks =
    None
      ( List
          ./../types/io.k8s.api.admissionregistration.v1.MutatingWebhook.dhall
      )
}
