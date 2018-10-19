name := "Computability Zoo"

scalaVersion in ThisBuild := "2.12.6"

lazy val root = project.in(file(".")).
  aggregate(computabilityZooJVM, computabilityZooJS).
  settings(
    publish := {},
    publishLocal := {},
    scalaJSUseMainModuleInitializer := true
  )

lazy val computabilityZoo = crossProject.in(file(".")).
  settings(
    name := "computability-zoo",
    version := "0.1-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "1.2.0",
      "org.scalatest" %%% "scalatest" % "3.0.5" % "test",
      "org.scalacheck" %%% "scalacheck" % "1.14.0",
      "org.tpolecat" %%% "atto-core" % "0.6.3"
    )
  ).
  jvmSettings(
    // Add JVM-specific settings here
  ).
  jsSettings(
    // Add JS-specific settings here
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.6",
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.7",
  )

lazy val computabilityZooJVM = computabilityZoo.jvm
lazy val computabilityZooJS = computabilityZoo.js

scalaJSUseMainModuleInitializer := true
