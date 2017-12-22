name := "knol-blog"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "net.databinder.dispatch" %% "dispatch-core" % "0.13.2",
  "joda-time" % "joda-time" % "2.9.9",
  "net.liftweb" % "lift-json_2.11" % "2.6-M4",
  "com.typesafe" % "config" % "1.3.1",
  "com.typesafe.akka" %% "akka-slf4j" % "2.5.7",
  "com.typesafe.akka" %% "akka-actor" % "2.5.7",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.7" % Test)