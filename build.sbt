packAutoSettings

name := "nxt-nsc"

version := "1.0"

scalaVersion := "2.11.5"

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.scalaj" %% "scalaj-http" % "1.1.4",
  "com.typesafe.play" %% "play-json" % "2.3.8"
)
