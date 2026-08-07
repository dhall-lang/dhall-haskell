let k8s = ./package.dhall

let file1_exportVolumes =
      \(doExport : Bool) ->
      \(sharedVolumeMount : k8s.VolumeMount.Type) ->
        if    doExport
        then  Some [ sharedVolumeMount ]
        else  None (List k8s.VolumeMount.Type)

let file2_mkContainer =
      \(doExport : Bool) ->
      \(sharedVolumeMount : k8s.VolumeMount.Type) ->
        k8s.Container::{
        , name = "exporter"
        , image = Some "alpine"
        , command = Some [ "/bin/sh" ]
        , volumeMounts = file1_exportVolumes doExport sharedVolumeMount
        }

let file2a_mkContainer2 =
      \(doExport : Bool) ->
      \(sharedVolumeMount : k8s.VolumeMount.Type) ->
        k8s.Container::{
        , name = "exporter2"
        , image = Some "alpine"
        , command = Some [ "/bin/sh" ]
        , volumeMounts = file1_exportVolumes doExport sharedVolumeMount
        }

let mkPod
    : Bool -> k8s.VolumeMount.Type -> k8s.Pod.Type
    = \(doExport : Bool) ->
      \(sharedVolumeMount : k8s.VolumeMount.Type) ->
        k8s.Pod::{
        , metadata = k8s.ObjectMeta::{ name = Some "ExportPod" }
        , spec = Some k8s.PodSpec::{
          , containers =
            [ file2_mkContainer doExport sharedVolumeMount
            , file2a_mkContainer2 doExport sharedVolumeMount
            ]
          }
        }

in  { mkPod }
