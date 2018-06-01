val dependencies = Seq(
  "org.typelevel" %% "cats-core" % "1.1.0",
  "org.typelevel" %% "cats-effect" % "1.0.0-RC",
  "org.scalatest" %% "scalatest" % "3.0.5"
)

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Enterprise-FP",
    libraryDependencies ++= dependencies,
    scalacOptions += "-Ypartial-unification"
  )
