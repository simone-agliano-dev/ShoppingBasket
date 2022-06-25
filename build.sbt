ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.11.9"

lazy val root = (project in file("."))
  .settings(
    name := "PriceBasket"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % Test
