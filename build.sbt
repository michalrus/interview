lazy val root = (project in file(".")).
  settings(
    name := "interview",
    version := "0.1",
    scalaVersion := "2.11.7",
    scalariformSettings,
    wartremoverErrors ++= Warts.all,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.4" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
    )
  )
