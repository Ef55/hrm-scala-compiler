val scala3Version = "3.1.1"

def commonSettings = List(
  version := "0.1.0-SNAPSHOT",

  scalaVersion := scala3Version,

  scalacOptions += "-Xcheck-macros",
)

lazy val compiler = project
  .in(file("compiler"))
  .settings(commonSettings)
  .settings(
    name := "compiler",

    libraryDependencies += "expressions-processor" %% "expressions-processor" % "0.1.0-SNAPSHOT",

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.11",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % "test",
  )

lazy val examples = project
  .in(file("examples"))
  .dependsOn(compiler)
  .settings(commonSettings)
  .settings(
    name := "examples"
  )