lazy val root = (project in file(".")).
  settings(
    name := "interview",
    version := "0.1",
    scalaVersion := "2.11.7",
    scalariformSettings,
    scalacOptions += "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats" % "0.6.0",
      "org.scalatest" %% "scalatest" % "2.2.4" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
    )
  )
