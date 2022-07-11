ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.12"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-16" % "3.2.12.0" % "test"

lazy val root = (project in file("."))
  .settings(
    name := "delaunay"
  )
