val scala3Version = "3.1.2-RC2"

def commonSettings = List(
  version := "0.1.0-SNAPSHOT",

  scalaVersion := scala3Version,

  scalacOptions ++= Seq("-explain"/*, "-Xcheck-macros"*/),
)

lazy val compiler = project
  .in(file("compiler"))
  .settings(commonSettings)
  .settings(
    name := "compiler",

    libraryDependencies += "expressions-processor" %% "expressions-processor" % "0.2.1",

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