
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.18"

lazy val root = (project in file("."))
  .settings(
    name := "GameBura"
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.20" % Test,
  "com.bot4s" %% "telegram-core" % "7.1.0",
  "com.softwaremill.sttp.client4" %% "okhttp-backend" % "4.0.27"
)
