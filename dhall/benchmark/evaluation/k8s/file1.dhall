let k8s = ./package.dhall

let exportVolumes =
      \(doExport : Bool) ->
      \(sharedVolumeMount : k8s.VolumeMount.Type) ->
        if    doExport
        then  Some [ sharedVolumeMount ]
        else  None (List k8s.VolumeMount.Type)

in  { exportVolumes }
