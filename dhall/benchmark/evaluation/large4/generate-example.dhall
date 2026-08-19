let baseSchema = ./base/schema.dhall

let overlaySchema = ./customizations/schema.dhall

let apply = ./apply/apply-all.dhall

let base = baseSchema::{=}

let overlay = overlaySchema::{=}

let overlay = overlay with Shared.imageMods.registry = Some "google.io"

let overlay = overlay with Shared.namespace = Some "ns-sourcegraph"

let overlay =
      overlay
      with   Symbols
           . Deployment
           . symbols
           . containers
           . jaeger-agent
           . resources
           . limits
           . cpu
           = Some
          "500m"

let r = apply base overlay

in  r
