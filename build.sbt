name := "pac-man"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.2"

scalacOptions := Seq("-unchecked", "-deprecation", "-feature", "-encoding", "utf8")

libraryDependencies ++= {
  val akkaVersion = "2.3.4"
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test",
    "org.scalatest" %% "scalatest" % "2.2.0" % "test"
  )
}
