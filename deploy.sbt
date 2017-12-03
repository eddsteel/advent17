enablePlugins(JavaAppPackaging)
maintainer in Docker := "Edd Steel <edward.steel@gmail.com>"
packageName in Docker := "eddsteel/advent17"
packageSummary in Docker := "a sane project"
packageDescription := "a sane project"
dockerExposedPorts := List(8080)
dockerBaseImage := "frolvlad/alpine-scala"
dockerUpdateLatest := true
