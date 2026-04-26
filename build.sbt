ThisBuild / scalaVersion := "3.7.4"
ThisBuild / version := "0.1.0-SNAPSHOT"

val betterGC = Seq("-Xms1G", "-Xmx4G", "-XX:+UseG1GC")

lazy val root = project
  .aggregate(library, example)
  .settings(
    name := "PwD4LLMs-root",
    publish / skip := true
  )

lazy val library = project
  .in(file("library"))
  .settings(
    name := "PwD4LLMs",
    libraryDependencies := Seq(
      "first-class-derivatives" %% "first-class-derivatives" % "3.0.1",
      "org.scalameta" %% "munit" % "1.0.0" % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings", // fail on warnings (optional)
      "-explain", // detailed explanations
      "-explain-types" // deeper type errors
    )
  )

lazy val example = project
  .in(file("example"))
  .dependsOn(library)
  .settings(
    name := "PwD4LLMs-example",
    libraryDependencies := Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6",
      "org.typelevel" %% "log4cats-slf4j" % "2.8.0",
      "org.slf4j" % "slf4j-api" % "2.0.12", // logging api
      "org.slf4j" % "slf4j-simple" % "2.0.12" // simple logger
    ),
    Compile / run / fork := true, // necessary for custom GC options
    Compile / run / javaOptions ++= betterGC ++ Seq(
      "-XX:StartFlightRecording=filename=recording.jfr,settings=profile,dumponexit=true",
      "-XX:FlightRecorderOptions=stackdepth=256"
    ),
    publish / skip := true
  )

lazy val bench = project
  .in(file("bench"))
  .enablePlugins(JmhPlugin)
  .dependsOn(library)
  .dependsOn(example)
  .settings(
    name := "PwD4LLMs-benchmarks",
    Jmh / fork := true,
    Jmh / run / javaOptions ++= betterGC
  )
