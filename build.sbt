enablePlugins(ScalaJSPlugin)

val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "rvmafia-stateapp3",
    version := "0.1.0",

    scalaVersion := scala3Version,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },

    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.10" % "test",

    testFrameworks += new TestFramework("utest.runner.Framework")
  )

