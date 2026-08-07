{ name : Text
, args : Optional (List Text)
, command : Optional (List Text)
, env : Optional (List ./io.k8s.api.core.v1.EnvVar.dhall)
, envFrom : Optional (List ./io.k8s.api.core.v1.EnvFromSource.dhall)
, image : Optional Text
, imagePullPolicy : Optional Text
, lifecycle : Optional ./io.k8s.api.core.v1.Lifecycle.dhall
, livenessProbe : Optional ./io.k8s.api.core.v1.Probe.dhall
, ports : Optional (List ./io.k8s.api.core.v1.ContainerPort.dhall)
, readinessProbe : Optional ./io.k8s.api.core.v1.Probe.dhall
, resizePolicy :
    Optional (List ./io.k8s.api.core.v1.ContainerResizePolicy.dhall)
, resources : Optional ./io.k8s.api.core.v1.ResourceRequirements.dhall
, restartPolicy : Optional Text
, restartPolicyRules :
    Optional (List ./io.k8s.api.core.v1.ContainerRestartRule.dhall)
, securityContext : Optional ./io.k8s.api.core.v1.SecurityContext.dhall
, startupProbe : Optional ./io.k8s.api.core.v1.Probe.dhall
, stdin : Optional Bool
, stdinOnce : Optional Bool
, targetContainerName : Optional Text
, terminationMessagePath : Optional Text
, terminationMessagePolicy : Optional Text
, tty : Optional Bool
, volumeDevices : Optional (List ./io.k8s.api.core.v1.VolumeDevice.dhall)
, volumeMounts : Optional (List ./io.k8s.api.core.v1.VolumeMount.dhall)
, workingDir : Optional Text
}
