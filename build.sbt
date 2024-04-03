ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.1"

val uTestVersion = "0.8.2"

lazy val root = (project in file("."))
  .settings(
    name := "golf",
    libraryDependencies ++= Seq (
      "com.lihaoyi" %% "utest" % uTestVersion % "test",
    ),
    testFrameworks += TestFramework ("utest.runner.Framework")
  )
