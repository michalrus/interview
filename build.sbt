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
    wartremoverErrors ++= Warts
      .allBut(Wart.Any, Wart.Nothing, Wart.Option2Iterable, Wart.Equals),
    libraryDependencies ++= Seq(
      "org.scalatest"     %% "scalatest"    % "2.2.4" % "test",
      "org.scalacheck"    %% "scalacheck"   % "1.12.5" % "test",
      "com.typesafe.akka" %% "akka-testkit" % AkkaVersion % "test",
      "com.typesafe.akka" %% "akka-actor"   % AkkaVersion
    ),
    unmanagedJars in Compile ++= {
      val scapiPath = baseDirectory.value / "lib" / "scapi" // change me if that’s untrue
      val deps = List("activemq-all-5.9.1",
                      "bcprov-jdk16-146",
                      "commons-exec-1.2",
                      "Scapi-2.4")
      deps.map(j ⇒ scapiPath / s"$j.jar")
    }
  )

val AkkaVersion = "2.4.11"
