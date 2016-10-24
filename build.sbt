enablePlugins(GitVersioning)

git.useGitDescribe := true

organization in ThisBuild := "com.michalrus.pitch"

scalaVersion in ThisBuild := "2.11.8"

scalafmtConfig in ThisBuild := Some(file(".scalafmt.conf"))

lazy val root = (project in file("."))
  .settings(reformatOnCompileSettings)
  .settings(
    name := "interview",
    scalacOptions in Compile ++= Seq("-deprecation",
                                     "-feature",
                                     "-unchecked",
                                     "-Xlint",
                                     "-Xfatal-warnings",
                                     "-Yno-adapted-args",
                                     "-Yrangepos",
                                     "-Ywarn-dead-code",
                                     "-Ywarn-inaccessible",
                                     "-Ywarn-infer-any",
                                     "-Ywarn-nullary-override",
                                     "-Ywarn-numeric-widen",
                                     "-Ywarn-unused",
                                     "-Ywarn-unused-import",
                                     "-Ywarn-value-discard"),
    wartremoverErrors ++= Warts.all,
    libraryDependencies ++= Seq(
      "org.scalatest"  %% "scalatest"  % "2.2.4"  % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
    )
  )
