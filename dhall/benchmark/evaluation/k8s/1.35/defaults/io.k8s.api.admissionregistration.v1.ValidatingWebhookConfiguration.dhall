{ apiVersion = "admissionregistration.k8s.io/v1"
, kind = "ValidatingWebhookConfiguration"
, webhooks =
    None
      ( List
          ./../types/io.k8s.api.admissionregistration.v1.ValidatingWebhook.dhall
      )
}
