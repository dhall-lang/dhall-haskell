let k8s = ./package.dhall

let exportVolumes = (./file1.dhall).exportVolumes

let mkContainer =
      \(doExport : Bool) ->
      \(sharedVolumeMount : k8s.VolumeMount.Type) ->
        k8s.Container::{
        , name = "exporter"
        , image = Some "alpine"
        , command = Some [ "/bin/sh" ]
        , volumeMounts = exportVolumes doExport sharedVolumeMount
        }

in  { mkContainer }
