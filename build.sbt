name := "CarteAuxTresors"

version := "0.1"

scalaVersion := "2.13.1"

val akkaVersion = "2.5.25"
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % akkaVersion
libraryDependencies += "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion
