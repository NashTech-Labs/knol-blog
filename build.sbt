name := "knol-blog"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "net.databinder.dispatch" %% "dispatch-core" % "0.13.2",
  "joda-time" % "joda-time" % "2.9.9",
  "net.liftweb" % "lift-json_2.11" % "2.6-M4",
  "com.typesafe" % "config" % "1.3.1",
  "com.github.gilbertw1" %% "slack-scala-client" % "0.2.2",
  "com.enragedginger" %% "akka-quartz-scheduler" % "1.6.0-akka-2.4.x")
