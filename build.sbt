name := "Assignment3"

version := "1.0"

scalaVersion := "2.12.2"

val circeVersion = "0.7.0"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

mainClass := Some("POSEvaluator")