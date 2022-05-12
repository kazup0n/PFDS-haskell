lazy val hello = (project in file("."))
  .settings(
    name := "PFDS-scala",
    ThisBuild / scalaVersion := "2.13.6",
    Compile / scalaSource := baseDirectory.value / "scala"
  )
addCompilerPlugin(
  "org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full
)
