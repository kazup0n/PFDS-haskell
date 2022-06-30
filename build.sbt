lazy val hello = (project in file("."))
  .settings(
    name := "PFDS-scala",
    ThisBuild / scalaVersion := "2.13.6",
    Compile / scalaSource := baseDirectory.value / "scala",
    Test / scalaSource := baseDirectory.value / "scala",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.12",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"
  )
addCompilerPlugin(
  "org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full
)
