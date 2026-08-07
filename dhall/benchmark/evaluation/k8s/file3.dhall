let k8s = ./package.dhall

let mkContainer = (./file2.dhall).mkContainer

let mkContainer2 = (./file2a.dhall).mkContainer2

let mkPod
    : Bool -> k8s.VolumeMount.Type -> k8s.Pod.Type
    = \(doExport : Bool) ->
      \(sharedVolumeMount : k8s.VolumeMount.Type) ->
        k8s.Pod::{
        , metadata = k8s.ObjectMeta::{ name = Some "ExportPod" }
        , spec = Some k8s.PodSpec::{
          , containers =
            [ mkContainer doExport sharedVolumeMount
            , mkContainer2 doExport sharedVolumeMount
            ]
          }
        }

in  { mkPod }
